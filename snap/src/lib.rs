#![feature(proc_macro_span)]
#![feature(const_extern_fn)]

use proc_macro::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::path::PathBuf;
use syn::ItemFn;
use syn::{parse, parse_macro_input};

#[proc_macro_attribute]
pub fn snapshot(ignored: TokenStream, tokens: TokenStream) -> TokenStream {
    let mut resource_path = PathBuf::from("snapshots");

    let mut source_path =
    Span::call_site()
        .source_file()
        .path();

    source_path.set_extension("");

    source_path.iter()
        .skip(1) // We skip the first dir because it is the 'src' dir.
        .for_each(|p| resource_path.push(p));

    let mut parsed_fn = parse_macro_input!(tokens as ItemFn);
    let name = parsed_fn.sig.ident.clone();

    resource_path.push(name.to_string());
    resource_path.set_extension("json");
    let path_str = resource_path.to_str().unwrap();

    let tokens = quote!(
        pub fn #name() {

            use std::path::PathBuf;

            #parsed_fn

            let mut manifest_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            let snapshot_path = PathBuf::from(#path_str);
        }
    )
    .into();

    eprintln!("Tokens: {}", tokens);

    tokens
}
