use proc_macro::TokenStream;
use quote::{quote, ToTokens};

enum FieldType<'ast> {
    Option(&'ast syn::Type),
    VecEach((String, &'ast syn::Type)),
    Other(&'ast syn::Type),
}

struct FieldMetaData<'ast> {
    vis: &'ast syn::Visibility,
    ident: &'ast syn::Ident,
    ty: FieldType<'ast>,
}
pub(super) struct BuilderImplementor<'ast> {
    struct_ident: &'ast syn::Ident,
    fields: Vec<FieldMetaData<'ast>>,
    vis: &'ast syn::Visibility,
}

impl<'ast> BuilderImplementor<'ast> {
    pub(crate) fn parse_input(ast: &'ast syn::DeriveInput) -> syn::Result<Self> {
        let data_struct = Self::get_struct(&ast.data)?;
        let named_fields = Self::get_fields(&data_struct.fields)?;

        let mut fields = Vec::with_capacity(named_fields.len());

        for f in named_fields {
            fields.push(FieldMetaData::parse_field(f)?);
        }

        Ok(Self {
            struct_ident: &ast.ident,
            fields,
            vis: &ast.vis,
        })
    }

    pub(super) fn generate_impl(&self) -> TokenStream {
        let builder_ident = syn::Ident::new(
            &format!("{}Builder", self.struct_ident),
            self.struct_ident.span(),
        );
        let vis = self.vis;
        let ident = self.struct_ident;

        let builder_fields = self.fields.iter().map(|f| {
            let vis = f.vis;
            let ident = f.ident;
            match f.ty {
                FieldType::Option(ty) => quote!(#vis #ident: ::std::option::Option<#ty>),
                FieldType::VecEach((_, ty)) => quote!(#vis #ident: ::std::vec::Vec<#ty>),
                FieldType::Other(ty) => quote!(#vis #ident: ::std::option::Option<#ty>),
            }
        });

        let builder_methods = self.fields.iter().map(|f| {
            let ident = f.ident;
            match f.ty {
                FieldType::Option(ty) => {
                    quote!(#vis fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = ::std::option::Option::Some(#ident);
                        self
                    })
                }
                FieldType::VecEach((ref name, ty)) => {
                    let extra = if ident != name {
                        quote!(
                            #vis fn #ident(&mut self, #ident: ::std::vec::Vec<#ty>) -> &mut Self {
                                self.#ident = #ident;
                                self
                            }
                        )
                    } else {
                        quote!()
                    };

                    let n_ident = syn::Ident::new(name, ident.span());
                    quote!(
                        #extra
                        #vis fn #n_ident(&mut self, #n_ident: #ty) -> &mut Self {
                            self.#ident.push(#n_ident);
                            self
                        }
                    )
                }
                FieldType::Other(ty) => {
                    quote!(
                        #vis fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = ::std::option::Option::Some(#ident);
                        self
                    })
                }
            }
        });

        let build_fields = self.fields.iter().map(|f| {
            let ident = f.ident;
            match f.ty {
                FieldType::Option(_) => quote!(#ident: self.#ident.clone()),
                FieldType::VecEach(_) => quote!(#ident: self.#ident.clone()),
                FieldType::Other(_) => quote!(#ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), " is not set"))?),
            }
        });

        let builder_empty = self.fields.iter().map(|f| {
            let ident = f.ident;
            match f.ty {
                FieldType::VecEach(_) => quote!(#ident: ::std::vec![]),
                _ => quote!(#ident: ::std::option::Option::None),
            }
        });

        quote!(
            #vis struct #builder_ident {
                #(#builder_fields,)*
            }
    
            impl #builder_ident {
                #(#builder_methods)*
    
                #vis fn build(&mut self) -> ::std::result::Result<#ident, ::std::boxed::Box<dyn ::std::error::Error>> {
                    ::std::result::Result::Ok(#ident {
                        #(#build_fields,)*
                    })
                }
            }
    
            impl #ident {
                #vis fn builder() -> #builder_ident {
                    #builder_ident {
                        #(#builder_empty),*
                    }
                }
            }
    
        ).into()
    }

    fn get_struct(data: &syn::Data) -> syn::Result<&syn::DataStruct> {
        match data {
            syn::Data::Struct(ref s) => Ok(s),
            syn::Data::Enum(ref e) => Err(syn::Error::new_spanned(
                e.enum_token,
                "Derive 'builder' only works for Structs not Enum's",
            )),
            syn::Data::Union(ref u) => Err(syn::Error::new_spanned(
                u.union_token,
                "Derive 'builder' only works for Structs not Unions",
            )),
        }
    }

    fn get_fields(
        fields: &syn::Fields,
    ) -> syn::Result<&syn::punctuated::Punctuated<syn::Field, syn::token::Comma>> {
        match fields {
            syn::Fields::Named(n) => Ok(&n.named),
            _ => Err(syn::Error::new_spanned(
                fields,
                "Derive 'builder' only works for Structs with Named Fields",
            )),
        }
    }
}

impl<'ast> FieldMetaData<'ast> {
    fn parse_field(field: &'ast syn::Field) -> syn::Result<Self> {
        let ty = Self::get_type(field)?;
        let ident = unsafe { field.ident.as_ref().unwrap_unchecked() };

        Ok(Self {
            vis: &field.vis,
            ident,
            ty,
        })
    }

    fn get_type(field: &syn::Field) -> syn::Result<FieldType> {
        if let Some(v) = Self::get_builder_attr(field)? {
            Ok(FieldType::VecEach(v))
        } else if let Some(o) = Self::get_inner_type(&field.ty, "Option") {
            Ok(FieldType::Option(o))
        } else {
            Ok(FieldType::Other(&field.ty))
        }
    }

    fn get_inner_type<'a, T>(ty: &'a syn::Type, ident: &T) -> Option<&'a syn::Type>
    where
        T: AsRef<str> + ?Sized,
    {
        if let syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) = ty
        {
            let p = segments.last()?;
            if p.ident != ident || p.arguments.is_empty() {
                return None;
            }

            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                args,
                ..
            }) = &p.arguments
            {
                if args.len() != 1 {
                    return None;
                }

                if let syn::GenericArgument::Type(inner_ty) = &args[0] {
                    return Some(inner_ty);
                }
            }
        }

        None
    }

    fn get_builder_attr(field: &syn::Field) -> syn::Result<Option<(String, &syn::Type)>> {
        let possible = field
            .attrs
            .iter()
            .filter(|a| a.path().is_ident("builder"))
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
                format!("expected one 'builder' attribute found {}", possible.len()),
            ));
        }

        let ty = Self::get_inner_type(&field.ty, "Vec").ok_or(syn::Error::new_spanned(
            field,
            "'builder' is only allowed for 'Vec<T>'",
        ))?;

        let meta = possible[0].parse_args::<syn::MetaNameValue>()?;

        if !meta.path.is_ident("each") {
            return Err(syn::Error::new_spanned(
                &possible[0].meta,
                "expected `builder(each = \"...\")`",
            ));
        }

        if let syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(s),
            ..
        }) = meta.value
        {
            Ok(Some((s.value(), ty)))
        } else {
            Err(syn::Error::new_spanned(
                meta.value,
                "The value must be a valid String!",
            ))
        }
    }
}
