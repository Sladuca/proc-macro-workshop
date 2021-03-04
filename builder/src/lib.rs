use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{self, Data, DeriveInput, Error, Fields, ItemStruct, parse_macro_input, parse_quote, spanned::Spanned};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // eprintln!("{:#?}", input);
    let ast: DeriveInput = parse_macro_input!(input as DeriveInput);
    expand(ast).unwrap_or_else(|err| err.to_compile_error()).into()
}

fn expand(ast: DeriveInput) -> Result<TokenStream2, Error> {
    let builder_decl = gen_builder_decl(&ast)?;
    let struct_ident = &ast.ident;
    let expanded = quote!{
        #builder_decl
        
        impl #struct_ident {
            pub fn builder() {}
        }
    };
    eprintln!("{:#?}", expanded);
    Ok(expanded.into())
}

fn gen_builder_decl(ast: &DeriveInput) -> Result<ItemStruct, Error> {
    let struct_ident = &ast.ident;
    match ast.data {
        Data::Struct(ref ast) => {
            let fields = match ast.fields {
                Fields::Named(ref fields) => fields.named.iter().map(|field| {
                    let mut new_field = field.clone();
                    let ty = &field.ty; 
                    new_field.ty = parse_quote! { Option<#ty> };
                    new_field
                }),
                _ => return Err(Error::new(ast.fields.span(), "fields must be named"))
            };
            let builder_ident = format_ident!("{}Builder", struct_ident);
            Ok(parse_quote! {
                pub struct #builder_ident {
                    #(#fields),*
                }
            })
        },
        _ => Err(Error::new_spanned(ast, "only structs are supported"))
    }
}