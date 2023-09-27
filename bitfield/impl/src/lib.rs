mod bitfield;
use bitfield::*;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemEnum, ItemStruct};

#[proc_macro_attribute]
pub fn bitfield(_: TokenStream, input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as ItemStruct);
    expand_struct(&item)
}

#[proc_macro_derive(BitfieldSpecifier)]
pub fn derive_bitfield_specifier(input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as ItemEnum);
    expand_enum(&item)
}

#[proc_macro]
pub fn def_types(input: TokenStream) -> TokenStream {
    if !input.is_empty() {
        return quote!(::std::compile_error("This macro not accepts input!")).into();
    }

    type_gen()
}
