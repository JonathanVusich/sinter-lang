#![feature(proc_macro_span)]
#![feature(const_extern_fn)]

use proc_macro::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::path::PathBuf;
use syn::{ItemFn, ReturnType};
use syn::{parse, parse_macro_input};

#[proc_macro_attribute]
pub fn snapshot(ignored: TokenStream, tokens: TokenStream) -> TokenStream {
    let mut resource_path = PathBuf::from("snapshots");

    let mut source_path =
    Span::call_site()
        .source_file()
        .path();

    source_path.set_extension("");
    resource_path.push(source_path);

    let parsed_fn = parse_macro_input!(tokens as ItemFn);
    let name = parsed_fn.sig.ident.clone();
    let return_ty = parsed_fn.sig.output.clone();

    let ty = match return_ty {
        ReturnType::Type(_, ty) => {
            ty
        },
        _ => panic!()
    };

    resource_path.push(name.to_string());
    resource_path.set_extension("json");
    let path_str = resource_path.to_str().unwrap();

    let tokens = quote!(
        pub fn #name() -> Result<(), Box<dyn std::error::Error>> {

            use std::fs::{File, OpenOptions};
            use std::io::{BufReader, BufWriter};
            use std::path::{Path, PathBuf};
            use ::serde::{Serialize, Deserialize};

            #parsed_fn

            let mut snapshot_path= PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            snapshot_path.push(PathBuf::from(#path_str));

            let value = #name();

            if let Ok(file) = File::open(snapshot_path) {
                let reader = BufReader::new(file);
                let saved_value: #ty = serde_json::from_reader(reader)?;
                assert_eq!(saved_value, value);
                Ok(())
            } else {
                if let Some(parent) = &snapshot_path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                let file = OpenOptions::new()
                    .write(true)
                    .create(true)
                    .open(snapshot_path)?;
                let writer = BufWriter::new(file);
                Ok(serde_json::to_writer_pretty(writer, &value)?)
            }
        }
    )
    .into();

    eprintln!("Tokens: {}", tokens);

    tokens
}
