use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{self, PathArguments, GenericArgument, DataStruct, AngleBracketedGenericArguments, Data, DeriveInput, Error, Fields, parse_quote, parse_macro_input, spanned::Spanned, Type, Meta, TypePath};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // eprintln!("{:#?}", input);
    let ast: DeriveInput = parse_macro_input!(input as DeriveInput);
    expand(ast).unwrap_or_else(|err| err.to_compile_error()).into()
}

fn expand(ast: DeriveInput) -> Result<TokenStream2, Error> {
    let builder_fn_impl = gen_builder_fn_impl(&ast)?;
    let builder_build_fn_impl = gen_build_fn_impl(&ast)?;
    let builder_decl = gen_builder_decl(&ast)?;
    let builder_impl = gen_builder_impl(&ast)?;
    let expanded = quote!{
        #builder_decl
        #builder_fn_impl
        #builder_build_fn_impl
        #builder_impl
    };
    //eprintln!("{:#?}", expanded);
    Ok(expanded.into())
}

fn gen_builder_impl(ast: &DeriveInput) -> Result<TokenStream2, Error> {
    let builder_ident = format_ident!("{}Builder", ast.ident);
    match ast.data {
        Data::Struct(ref ast) => {
            let field_info = match ast.fields {
                Fields::Named(ref fields) => {
                    let field_idents: Result<Vec<_>, Error> = fields
                        .named
                        .iter()
                        .map(|field| match field.ident {
                            Some(ref ident) => {
                                match field.ty {
                                    Type::Path(TypePath  { ref path, .. }) => match path.segments.iter().find_map(|segment| {
                                        if segment.ident.to_string() == "Option" {
                                            Some(&segment.arguments)
                                        } else {
                                            None
                                        }
                                    }) {
                                        Some(PathArguments::AngleBracketed( AngleBracketedGenericArguments { ref args, ..})) => {
                                            match args.first() {
                                                Some(GenericArgument::Type(ref ty)) => Ok((ident, ty)),
                                                _ => Ok((ident, &field.ty)),
                                            }
                                        }
                                        Some(_) => Err(Error::new(field.span(), "invalid option type arguments!")),
                                        None => Ok((ident, &field.ty))
                                    },
                                    _ => Ok((ident, &field.ty))
                                }
                            }
                            None => Err(Error::new(field.span(), "named field has no name!"))
                        }).collect();
                    field_idents?
                },
                _ => return Err(Error::new(ast.fields.span(), "only structs are supported"))
            };
            let fns = field_info.iter().map(|(name, ty)| {
                quote! {
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            });
            
            let res = quote!{
                impl #builder_ident {
                    #(
                        #fns
                    )*
                }
            };
            //eprintln!("{}", res);
            Ok(res)
        },
        _ => Err(Error::new_spanned(ast, "only structs are supported"))
    }
}

fn get_repeatable_methods(ast: &DeriveInput) -> Result<Vec<TokenStream2>, Error> {
    let mut repeatable_methods = Vec::new();
    match ast.data {
        Data::Struct(DataStruct { fields: Fields::Named(ref fields), .. }) => {
            for field in fields.named.iter() {
                if let None = field.ident {
                    return Err(Error::new(field.span(), "unnamed field in struct!"));
                }
                for attr in field.attrs.iter() {
                    if let Ok(Meta::NameValue(ref nv)) = attr.parse_meta() {
                        if nv.path.segments.iter().find(|segment| segment.ident.to_string() == field.ident.unwrap().to_string()).is_some() {
                            let lit = &nv.lit;
                            repeatable_methods.push(quote! { });
                            break;
                        }
                    }
                }
            }
        },
        _ => return Err(Error::new(ast.span(), "only structs are supported"))
    };
    Ok(repeatable_methods)
}

fn gen_build_fn_impl(ast: &DeriveInput) -> Result<TokenStream2, Error> {
    let struct_ident = &ast.ident;
    match ast.data {
        Data::Struct(ref ast) => {
            let lines = match ast.fields {
                Fields::Named(ref fields) => {
                    let lines: Result<Vec<_>, Error> = fields.named.iter().map(|field| match field.ident {
                        Some(ref ident) => match field.ty {
                            Type::Path(TypePath { ref path, ..}) => match path.segments.iter().find(|segment| segment.ident.to_string() == "Option") {
                                Some(_) => {
                                    let res = quote! { #ident: self.#ident.as_ref().cloned() };
                                    Ok(res)
                                }
                                None => {
                                    let res = quote! { #ident: self.#ident.as_ref().ok_or("not all fields set!")?.to_owned() };
                                    Ok(res)
                                }
                            }
                            _ => {
                                let res = quote! { #ident: self.#ident.as_ref().ok_or("not all fields set!")?.to_owned() };
                                Ok(res)
                            }
                        }
                        None => Err(Error::new(field.span(), "named field has no name!")),
                    }).collect();
                    lines?
                },
                _ => return Err(Error::new(ast.fields.span(), "struct has non-named fields!")),
            };

            let builder_ident = format_ident!("{}Builder", struct_ident);
            let res = quote!{
                impl #builder_ident {
                    pub fn build(&mut self) -> Result<#struct_ident, Box<dyn std::error::Error>> {
                        Ok(#struct_ident {
                            #(
                                #lines
                            ),*
                        })
                    }
                }
            };
            // eprintln!("{:#?}", res);
            Ok(res)
        }
        _ => Err(Error::new_spanned(ast, "only structs are supported"))
    }
}

fn gen_builder_fn_impl(ast: &DeriveInput) -> Result<TokenStream2, Error> {
    let struct_ident = &ast.ident;
    match ast.data {
        Data::Struct(ref ast) => {
            let field_values = match ast.fields {
                Fields::Named(ref fields) => {
                    let fields: Result<Vec<_>, Error> = fields.named.iter().map(|field| match field.ident {
                        Some(ref ident) => Ok(quote! { #ident: None }),
                        None => Err(Error::new(field.span(), "named field has no name!")),
                    }).collect();
                    fields?
                },
                _ => return Err(Error::new(ast.fields.span(), "struct has non-named fields!")),
            };

            let builder_ident = format_ident!("{}Builder", struct_ident);
            let res = quote!{
                impl #struct_ident {
                    pub fn builder() -> #builder_ident {
                        #builder_ident {
                            #(
                                #field_values
                            ),*
                        }
                    } 
                }
            };
            Ok(res)
            
        }
        _ => Err(Error::new_spanned(ast, "only structs are supported"))
    }
}

fn gen_builder_decl(ast: &DeriveInput) -> Result<TokenStream2, Error> {
    let struct_ident = &ast.ident;
    match ast.data {
        Data::Struct(ref ast) => {
            let fields = match ast.fields {
                Fields::Named(ref fields) => fields.named.iter().map(|field| {
                    let mut new_field = field.clone();
                    let ty = &field.ty;
                    match ty {
                        Type::Path(TypePath  { ref path, .. }) => {
                            match path.segments.iter().find(|segment| segment.ident.to_string() == "Option") {
                                Some(_) => new_field.ty = parse_quote! { #ty },
                                None => new_field.ty = parse_quote! { Option<#ty> }
                            }
                        },
                        _ => new_field.ty = parse_quote! { Option<#ty> }
                    };
                    new_field
                }),
                _ => return Err(Error::new(ast.fields.span(), "fields must be named"))
            };
            let builder_ident = format_ident!("{}Builder", struct_ident);
            let res = quote! {
                pub struct #builder_ident {
                    #(
                        #fields
                    ),*
                }
            };
            Ok(res)
        },
        _ => Err(Error::new_spanned(ast, "only structs are supported"))
    }
}