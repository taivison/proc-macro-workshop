use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal, Span};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{Attribute, Expr, ExprLit, Field, ItemEnum, ItemStruct, Lit, LitInt, Type, Variant};

pub(super) fn expand_struct(item_struct: &ItemStruct) -> TokenStream {
    let struct_ident = &item_struct.ident;

    let vis = &item_struct.vis;
    let size = compute_size(item_struct.fields.iter());
    let acessor_methods = item_struct.fields.iter().enumerate().map(|(i, f)| {
        let bit_start = compute_size(item_struct.fields.iter().take(i));
        let ident = f.ident.as_ref().unwrap();
        let ty = &f.ty;

        let get_id = format_ident!("get_{}", ident);
        let set_id = format_ident!("set_{}", ident);

        let ret_ty = create_type(ty);
        quote!(
            #vis fn #get_id(&self) -> #ret_ty {
                const BIT_START: usize = #bit_start;
                <#ty as ::bitfield::Specifier>::get::<BIT_START, SIZE>(&self.data)
            }

            #vis fn #set_id(&mut self, set_value: #ret_ty) {
                const BIT_START: usize = #bit_start;
                <#ty as ::bitfield::Specifier>::set::<BIT_START, SIZE>(&mut self.data, set_value)
            }
        )
    });

    let check_attrs = item_struct.fields.iter().filter_map(|f| {
        let literal = match get_bit_attr(&f.attrs) {
            Ok(l) => l,
            Err(e) => return Some(e.to_compile_error()),
        };
        literal.map(|lit| {
            let span = lit.span();
            let ty = &f.ty;
            quote_spanned!(span=> {const _: [(); #lit] = [(); <#ty as ::bitfield::Specifier>::BITS];})
        })
    });

    quote!(
        #vis struct #struct_ident {
            data: [u8; #size / 8]
        }

        const _: () = {
            const SIZE: usize = (#size / 8);
            impl #struct_ident {
                #vis fn new() -> Self {
                    Self {data: [0; #size / 8]}
                }

                #(#acessor_methods)*
            }

            let _: ::bitfield::MultipleOfEight<[(); #size % 8]>;

            #(#check_attrs)*
        };


    )
    .into()
}

fn compute_size<'a>(fields: impl Iterator<Item = &'a Field>) -> proc_macro2::TokenStream {
    let size = fields.map(|f| {
        let ty = &f.ty;
        quote!(<#ty as ::bitfield::Specifier>::BITS)
    });

    quote!((0 #(+ #size)*))
}

fn create_type(ty: &Type) -> proc_macro2::TokenStream {
    quote!(
        <#ty as ::bitfield::Specifier>::Type
    )
}

fn get_bit_attr(attrs: &[Attribute]) -> syn::Result<Option<&LitInt>> {
    let possible = attrs
        .iter()
        .filter(|a| a.path().is_ident("bits"))
        .collect::<Vec<_>>();

    if possible.is_empty() {
        return Ok(None);
    }

    if possible.len() > 1 {
        let mut f = proc_macro2::TokenStream::new();

        for p in &possible {
            p.to_tokens(&mut f);
        }

        return Err(syn::Error::new_spanned(
            f,
            format!("expected one 'builder' attribute found {}", possible.len()),
        ));
    }

    let attr = match possible[0].meta {
        syn::Meta::NameValue(ref n) => n,
        _ => {
            return Err(syn::Error::new_spanned(
                possible[0],
                "Invalid Attribute Syntax!",
            ))
        }
    };

    if let Expr::Lit(ExprLit {
        lit: Lit::Int(ref l),
        ..
    }) = attr.value
    {
        Ok(Some(l))
    } else {
        Err(syn::Error::new_spanned(
            &attr.value,
            "The value must be a integer literal!",
        ))
    }
}

pub(super) fn type_gen() -> TokenStream {
    let bit_u_ty = (0..4).map(|r| 8 * 2_usize.pow(r)).map(|r| {
        let ident = format_ident!("Bitu{}", r);
        let int_ty = format_ident!("u{}", r);
        let bits = Literal::usize_unsuffixed(r);
        quote!(
            struct #ident<const TOTAL_BITS: usize, const BIT_START: usize, const SIZE: usize>;
            impl<const TOTAL_BITS: usize, const BITS_START: usize, const SIZE: usize>
            Constants<TOTAL_BITS, BITS_START> for #ident<TOTAL_BITS, BITS_START, SIZE>
            {
                type Target = #int_ty;
                const MASK: Self::Target = (#int_ty::MAX >> (#bits - TOTAL_BITS)) << Self::OFFSET;
                const DROP_BITS: usize = (TOTAL_BITS - Self::MASK.count_ones() as usize);
                const ROTATE: usize = TOTAL_BITS - Self::DROP_BITS;
                const EXTRA_MASK: u8 = u8::MAX >> (8 - max_const(Self::DROP_BITS, 1));
            }

            impl<const TOTAL_BITS: usize, const BIT_START: usize, const SIZE: usize> GetSetterBits<SIZE>
            for #ident<TOTAL_BITS, BIT_START, SIZE>
            {
                type Target = #int_ty;
                #[inline(always)]
                fn get(values: &[u8; SIZE]) -> Self::Target {
                    let mut value = #int_ty::from_le_bytes(Self::copy_bytes(values, Self::START..Self::END));
                    value &= Self::MASK;
                    value >>= Self::OFFSET;
                    if Self::DROP_BITS == 0 {
                        value
                    } else {
                        let mut extra = (values[Self::START + ::std::mem::size_of::<Self::Target>()] & Self::EXTRA_MASK)
                            as Self::Target;
                        extra <<= Self::ROTATE;
                        value | extra
                    }
                }

                #[inline(always)]
                fn set(values: &mut [u8; SIZE], mut value: Self::Target) {
                    let mut current = #int_ty::from_le_bytes(Self::copy_bytes(values, Self::START..Self::END));
                    current &= !Self::MASK;
                    current |= (value << Self::OFFSET) & Self::MASK;
                    Self::copy_bytes_from(current.to_le_bytes(), values, Self::START..Self::END);
                    if Self::DROP_BITS > 0 {
                        let end_byte = ((value >> Self::ROTATE) as u8) & Self::EXTRA_MASK;
                        let new_byte = values[Self::START + ::std::mem::size_of::<Self::Target>()] & !Self::EXTRA_MASK;
                        values[Self::START + ::std::mem::size_of::<Self::Target>()] = new_byte | end_byte;
                    }
                }
            }
        )
    });

    let types = (1..=64).map(|v| {
        let ident = format_ident!("B{}", v);
        let literal = Literal::usize_unsuffixed(v);
        let ty_name = match v {
            1..=8 => "u8",
            9..=16 => "u16",
            17..=32 => "u32",
            33..=64 => "u64",
            _ => unreachable!(),
        };

        let ty = format_ident!("{}", ty_name);
        let impl_ty = format_ident!("Bit{}", ty_name);

        quote!(
            pub enum #ident {}

            impl Specifier for #ident {
                const BITS: usize = #literal;
                type Type = #ty;

                fn set<const BIT_START: usize, const SIZE: usize>(
                    arr: &mut [u8; SIZE],
                    num: <Self as Specifier>::Type,
                ) {
                    <#impl_ty <#literal, BIT_START, SIZE> as GetSetterBits<SIZE>>::set(arr, num)
                }
                fn get<const BIT_START: usize, const SIZE: usize>(
                    arr: &[u8; SIZE],
                ) -> <Self as Specifier>::Type {
                    <#impl_ty <#literal, BIT_START, SIZE> as GetSetterBits<SIZE>>::get(arr)
                }

            }
        )
    });

    quote!(
        #(#types)*
        #(#bit_u_ty)*
    )
    .into()
}

pub fn expand_enum(item: &ItemEnum) -> TokenStream {
    let (ty, bits, variants) = match get_repr_ty(item.variants.iter()) {
        Ok(s) => s,
        Err(e) => return e.to_compile_error().into(),
    };

    let ty = format_ident!("{}", ty);
    let bits_const = Literal::u32_unsuffixed(bits);

    let ident = &item.ident;

    let get_set_ty = format_ident!("B{}", bits);

    let check = Literal::usize_unsuffixed(2_usize.pow(bits));
    let checkings = variants.into_iter().map(|i| {
        let span = i.span();
        quote_spanned!(span =>
            let _: ::bitfield::InRange<[(); ((#ident::#i as isize) >= 0 && (#ident::#i as isize) < #check) as usize]>;
        )
    });

    quote!(
        const _: () = {
            impl Specifier for #ident {
                const BITS: usize = #bits_const;

                type Type = #ident;

                fn set<const BIT_START: usize, const SIZE: usize>(
                    arr: &mut [u8; SIZE],
                    num: <Self as Specifier>::Type,
                ) {
                    <#get_set_ty as Specifier>::set::<BIT_START, SIZE>(arr, num as #ty)
                }
        
                fn get<const BIT_START: usize, const SIZE: usize>(
                    arr: &[u8; SIZE],
                ) -> <Self as Specifier>::Type {
                    unsafe { ::std::mem::transmute(<#get_set_ty as Specifier>::get::<BIT_START, SIZE>(arr)) }
                }
            }

            #(#checkings)*
        };
    ).into()
}

fn get_repr_ty<'a, V>(variants: V) -> syn::Result<(&'static str, u32, Vec<&'a Ident>)>
where
    V: Iterator<Item = &'a Variant>,
{
    let mut variants_ids = vec![];
    let mut value: usize = 0;
    for v in variants {
        if !v.fields.is_empty() {
            return Err(syn::Error::new_spanned(
                v,
                "Variant cannot have any fields!",
            ));
        }
        value += 1;
        variants_ids.push(&v.ident);
    }

    if !value.is_power_of_two() {
        return Err(syn::Error::new(
            Span::call_site(),
            "BitfieldSpecifier expected a number of variants which is a power of 2",
        ));
    }

    value -= 1;

    let ty = if value <= u8::MAX as usize {
        "u8"
    } else if value <= u16::MAX as usize {
        "u16"
    } else if value <= u32::MAX as usize {
        "u32"
    } else {
        "u64"
    };

    Ok((ty, value.ilog2() + 1, variants_ids))
}
