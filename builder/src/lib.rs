mod builder;
use builder::*;

use proc_macro::TokenStream;
use quote::quote;

use syn::*;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {

    let ast = parse_macro_input!(input as DeriveInput);
    let builder = match BuilderImplementor::parse_input(&ast)
    {
        Ok(b) => b,
        Err(e) => {
            let error = e.to_compile_error();
            return quote!(#error).into();
        }
    };

    builder.generate_impl()
}