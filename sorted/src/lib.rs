mod sorted_impl;

use proc_macro::TokenStream;
use syn::{parse_macro_input, visit_mut::VisitMut, Item, ItemFn};

use crate::sorted_impl::{is_sorted, MatchExprChecker};
use quote::quote;

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream, input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as Item);

    let errors = match is_sorted(&item) {
        Ok(_) => quote!(),
        Err(e) => e.to_compile_error(),
    };

    quote!(#item
           #errors)
    .into()
}

#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut f = parse_macro_input!(input as ItemFn);
    let mut m = MatchExprChecker::new();
    m.visit_item_fn_mut(&mut f);

    let errors = m.generate_errors();
    quote!(#f
           #errors)
    .into()
}
