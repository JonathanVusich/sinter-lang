#![feature(proc_macro_span)]

use proc_macro::{Span, TokenStream};
use std::path::PathBuf;
use quote::{quote, ToTokens};
use syn::{parse, parse_macro_input};
use syn::ItemFn;

struct FilePath {
    path: PathBuf
}

impl ToTokens for FilePath {

    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        for path in self.path.iter() {

        }
        todo!()
    }
}

#[proc_macro_attribute]
pub fn snapshot(ignored: TokenStream, tokens: TokenStream) -> TokenStream {
    let mut parsed_fn = parse_macro_input!(tokens as ItemFn);
    let name = parsed_fn.sig.ident.to_string();

    let file_path = Span::call_site().source_file().path();
    // Trim off the src path if it exists
    let mut target_path = file_path.iter()
        .skip_while(|segment| *segment == "src")
        .collect::<PathBuf>();

    target_path.push(name);


    parsed_fn.block.stmts.insert(0, parse(quote!({
    }).into()).unwrap());

    quote!({

        let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        pathbuf.push(target_path);

    }).into()
}

mod tests {

    use crate::snapshot;

    #[test]
    #[snapshot]
    pub fn source_file() {

    }
}
