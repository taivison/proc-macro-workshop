use std::collections::HashSet;

use quote::{quote, ToTokens};


struct FieldMetaData<'ast> {
    ident: &'ast syn::Ident,
    format: Option<String>,
}

pub(super) struct CustomDebugImplementor<'ast> {
    ident: &'ast syn::Ident,
    fields: Vec<FieldMetaData<'ast>>,
    generics: &'ast syn::Generics,
}

impl<'ast> CustomDebugImplementor<'ast> {
    pub(super) fn parse_input(ast: &'ast mut syn::DeriveInput) -> syn::Result<Self> {
        let named_fields = Self::get_fields(&ast.data)?;

        let types = ast
            .generics
            .type_params()
            .map(|t| &t.ident)
            .collect::<HashSet<&syn::Ident>>();

        let attrs = Self::get_attrs_value(&ast.attrs)?;

        let mut used_types = HashSet::new();

        let mut predicates = syn::punctuated::Punctuated::new();
        let mut fields = Vec::with_capacity(named_fields.len());

        for f in named_fields {
            fields.push(FieldMetaData::parse(f)?);
            let ty = &f.ty;

            if !attrs.is_empty() {
                continue;
            }

            if let Some(t) = Self::type_need_bound(ty, &types) {
                if used_types.insert(t) {
                    predicates.push(syn::parse_quote!(#t: ::std::fmt::Debug));
                }
            }
        }

        if predicates.len() > 0 {
            ast.generics.make_where_clause().predicates = predicates;
        } else if !attrs.is_empty() {
            ast.generics.make_where_clause().predicates.extend(attrs);
        }

        Ok(Self {
            ident: &ast.ident,
            fields,
            generics: &ast.generics,
        })
    }

    pub(super) fn generate_impl(&self) -> proc_macro::TokenStream {
        let ident = self.ident;

        let fields = self.fields.iter().map(|f| {
            let ident = f.ident;
            if let Some(ref s) = f.format {
                quote!(.field(::std::stringify!(#ident), &::std::format_args!(#s ,&self.#ident)))
            } else {
                quote!(.field(::std::stringify!(#ident), &self.#ident))
            }
        });

        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

        quote!(
            impl #impl_generics ::std::fmt::Debug for #ident #ty_generics #where_clause
            {
                fn fmt(&self, fmt: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    fmt.debug_struct(stringify!(#ident))
                        #(#fields)*
                        .finish()
                }
            }
        )
        .into()
    }

    fn get_fields(
        data: &syn::Data,
    ) -> syn::Result<&syn::punctuated::Punctuated<syn::Field, syn::token::Comma>> {
        match data {
            syn::Data::Struct(s) => Self::get_fields_from_struct(s),
            syn::Data::Enum(e) => Err(syn::Error::new_spanned(
                e.enum_token,
                "Derive 'CustomDebug' only works for Structs not Enum's!",
            )),
            syn::Data::Union(u) => Err(syn::Error::new_spanned(
                u.union_token,
                "Derive 'CustomDebug' only works for Structs not Unions!",
            )),
        }
    }

    fn get_fields_from_struct(
        data_struct: &syn::DataStruct,
    ) -> syn::Result<&syn::punctuated::Punctuated<syn::Field, syn::token::Comma>> {
        match &data_struct.fields {
            syn::Fields::Named(n) => Ok(&n.named),
            _ => Err(syn::Error::new_spanned(
                &data_struct.fields,
                "Derive 'CustomDebug' only works for Structs with Named Fields",
            )),
        }
    }

    fn type_need_bound<'ty>(
        ty: &'ty syn::Type,
        hash: &HashSet<&syn::Ident>,
    ) -> Option<&'ty syn::Type> {
        if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
            for p in &path.segments {
                if hash.contains(&p.ident) {
                    return Some(ty);
                } else if p.arguments.is_none() || p.arguments.is_empty() {
                    continue;
                } else if let syn::PathArguments::AngleBracketed(
                    syn::AngleBracketedGenericArguments { args, .. },
                ) = &p.arguments
                {
                    if p.ident == "PhantomData" && args.len() == 1 {
                        if let syn::GenericArgument::Type(t) = &args[0] {
                            return Self::type_need_bound(t, hash).map(|_| ty);
                        } else {
                            return None;
                        }
                    } else {
                        for arg in args {
                            if let syn::GenericArgument::Type(t) = arg {
                                match Self::type_need_bound(t, hash) {
                                    Some(t) => return Some(t),
                                    None => continue,
                                }
                            }
                        }
                    }
                }
            }
            None
        } else {
            None
        }
    }

    fn get_attrs_value(attrs: &Vec<syn::Attribute>) -> syn::Result<Vec<syn::WherePredicate>> {
        attrs
            .iter()
            .filter_map(|a| {
                if !a.path().is_ident("debug") {
                    return None;
                }

                let meta = match a.parse_args::<syn::MetaNameValue>(){
                    Ok(m) => m,
                    Err(e) => return Some(Err(e)),
                };

                if !meta.path.is_ident("bound") {
                    return Some(Err(syn::Error::new_spanned(&a.meta, "expected `debug(bound = \"...\")`")));
                }


                let value = match meta.value {
                    syn::Expr::Lit(syn::ExprLit {lit: syn::Lit::Str(ref s ), ..}) => s.value(),
                    _ => return Some(Err(syn::Error::new_spanned(
                                    &meta.value,
                                    "The value must be a valid String!",
                                ))),
                };

                match syn::parse_str(&value) {
                    Ok(w) => Some(Ok(w)),
                    Err(e) => Some(Err(
                        syn::Error::new_spanned(
                            meta.value,
                            e.to_string()
                        )
                    )),
                }
            })
            .collect()
    }
}

impl<'ast> FieldMetaData<'ast> {
    fn parse(field: &'ast syn::Field) -> syn::Result<Self> {
        let ident = unsafe { field.ident.as_ref().unwrap_unchecked() };
        Ok(Self {
            ident,
            format: Self::get_format(field)?,
        })
    }

    fn get_format(field: &syn::Field) -> syn::Result<Option<String>> {
        let possible = field
            .attrs
            .iter()
            .filter(|a| a.path().is_ident("debug"))
            .collect::<Vec<&syn::Attribute>>();

        if possible.is_empty() {
            return Ok(None);
        } else if possible.len() > 1 {
            let mut f = proc_macro2::TokenStream::new();

            for p in &possible {
                p.to_tokens(&mut f);
            }

            return Err(syn::Error::new_spanned(
                f,
                format!("expected one 'debug' attribute found {}", possible.len()),
            ));
        }

        let meta = match possible[0].meta {
            syn::Meta::NameValue(ref n) => n,
            _ => {
                return Err(syn::Error::new_spanned(
                    possible[0],
                    "Invalid Attribute syntax!",
                ))
            }
        };

        if let syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(ref s),
            ..
        }) = meta.value
        {
            Ok(Some(s.value()))
        } else {
            Err(syn::Error::new_spanned(
                &meta.value,
                "The value must be a valid String!",
            ))
        }
    }
}
