#![feature(proc_macro)]

extern crate proc_macro;
extern crate syn;
#[macro_use] extern crate quote;

use proc_macro::TokenStream;


#[proc_macro]
pub fn halide(ts: TokenStream) -> TokenStream {
    let ast: syn::Block = syn::parse(ts).unwrap();
    let new_ast = quote!{ let x = 1; };
    new_ast.into()
    // (quote!{ #ast }).into()
}
