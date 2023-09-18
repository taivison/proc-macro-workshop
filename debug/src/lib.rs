mod custom_debug;
use custom_debug::*;
use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let mut ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let debug =  match CustomDebugImplementor::parse_input(&mut ast)  {
        Ok(c) => c,
        Err(e) => {
            let error = e.to_compile_error();
            return quote!(#error).into();
        }
    };

    
    debug.generate_impl()
}
