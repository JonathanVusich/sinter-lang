#![feature(proc_macro_span)]
#![feature(const_extern_fn)]

use proc_macro::{Span, TokenStream};
use std::path::PathBuf;

use quote::quote;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::{ItemFn, ReturnType, Type};

#[proc_macro_attribute]
pub fn snapshot(_ignored: TokenStream, tokens: TokenStream) -> TokenStream {
    let mut resource_path = PathBuf::from("snapshots");

    let mut source_path = Span::call_site().source_file().path();

    source_path.set_extension("");
    resource_path.push(source_path);

    let parsed_fn = parse_macro_input!(tokens as ItemFn);
    let name = parsed_fn.sig.ident.clone();
    let return_ty = parsed_fn.sig.output.clone();

    let ty: Box<Type>;
    if let ReturnType::Type(_, boxed_type) = return_ty {
        ty = boxed_type;
    } else {
        return syn::Error::new(parsed_fn.sig.span(), "expected return type")
            .to_compile_error()
            .into();
    }

    resource_path.push(name.to_string());
    resource_path.set_extension("snap");
    let path_str = resource_path.to_str().unwrap();

    quote!(
        pub fn #name() -> Result<(), Box<dyn std::error::Error>> {

            use std::fs::{File, OpenOptions};
            use std::io::{BufReader, BufWriter};
            use std::path::{Path, PathBuf};
            use serde::{Serialize, Deserialize};

            let mut snap_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            snap_path.push(PathBuf::from(#path_str));

            #parsed_fn

            let value = #name();

            if let Ok(file) = File::open(&snap_path) {
                let reader = BufReader::new(file);
                let saved_value: #ty = serde_json::from_reader(reader)?;
                assert_eq!(saved_value, value);
                Ok(())
            } else {
                if let Some(parent) = &snap_path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                let file = OpenOptions::new()
                    .write(true)
                    .create(true)
                    .open(&snap_path)?;
                let writer = BufWriter::new(file);
                Ok(serde_json::to_writer_pretty(writer, &value)?)
            }
        }
    )
    .into()
}

#[proc_macro]
#[doc(hidden)]
pub fn snapshot_folder(_ignored: TokenStream) -> TokenStream {
    let mut resource_path = PathBuf::from("snapshots");

    let mut source_path = Span::call_site().source_file().path();

    source_path.set_extension("");
    resource_path.push(source_path);
    let path_str = resource_path.to_str().unwrap();

    quote!({
        use std::path::{Path, PathBuf};

        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push(PathBuf::from(#path_str));
        path
    })
    .into()
}
