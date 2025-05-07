use std::any::TypeId;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{Path, parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, GenericArgument, PathArguments, PathSegment, Type, TypePath};
use std::error::Error;

fn strip_optional_type(ty: &Type) -> Result<Type, Box<dyn Error>> {
    match ty {
        Type::Path(type_path) => {
            if let Some(segment) = type_path.path.segments.iter().next() {

                if segment.ident.to_string() != "Option" {
                    return Err("not an optional type".into());
                }

                match &segment.arguments {
                    PathArguments::AngleBracketed(angle_bracketed_arg) => {
                        if let Some(generic_arg) = angle_bracketed_arg.args.iter().next() {
                            return match generic_arg {
                                GenericArgument::Type(ty) => Ok(ty.to_owned()),
                                _ => Err("weird optional type".into())
                            }
                        } else {
                            return Err("no type argument for opional type".into());
                        }
                    },
                    _ => return Err("not an optional type".into()),
                }
            } else {
                return Err("not an optional type".into());
            }
        },
        _ => return Err("not an optional type".into())
    }
}

fn is_optional_type(ty: &Type) -> bool {
    match ty {
        Type::Path(type_path) => {
            type_path.path.segments.iter().any(|segment| {
                segment.ident == "Option" 
            })
        },
        _ => false,
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let m_input = parse_macro_input!(input as DeriveInput);
    let builder_name = format!("{}Builder", &m_input.ident);
    let orig_name: Ident = m_input.ident.into();

    let builder_ident = Ident::new(&builder_name, Span::call_site());

    let named_fields = match m_input.data {
        Data::Struct(data_struct) => {
            data_struct.fields.iter().map(|e| (e.ident.clone().unwrap(), e.ty.clone()) ).collect()
        },
        _ => {vec![]}
    };

    let required_fields: Vec<_>= named_fields.iter().map(|(ident, ty)| {
        if is_optional_type(ty) {
            quote!{#ident: #ty}
        } else {
            let option_type = quote!{Option<#ty>};
            quote! {#ident: #option_type}
        }
    }).collect();

    let builder_init_args: Vec<_>= named_fields.iter().map(|(ident, _ty)|{
        quote!{#ident: None}
    }).collect();

    let builder_setter_fn: Vec<_> = named_fields.iter().map(|(ident, ty)|{
        if is_optional_type(ty) {
            let stripped_type = strip_optional_type(ty).expect("stripped type");
            quote!{
                pub fn #ident(&mut self, #ident: #stripped_type) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        } else {
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
    }).collect();

    let orig_init_required_args_from_builder: Vec<_> = named_fields.iter().map(|(ident, ty)|{
        if is_optional_type(ty) {
            quote! {#ident: self.#ident.clone()}
        } else {
            quote! {#ident: self.#ident.as_ref().unwrap().to_owned()}
        }
    }).collect();

    let checks: Vec<_> = named_fields.iter().filter_map(|(ident, ty)|{
        if is_optional_type(ty) {
            return None;
        }
        Some(quote! {
            if self.#ident.is_none() {
                return Err("missing".into());
            }
        })
    }).collect();

    let builder_struct = quote!{
        impl #orig_name {
            pub fn builder() -> #builder_ident {
                #builder_ident {#(#builder_init_args),*}
            }
        }

        struct #builder_ident {
            #(#required_fields),*
        }

        use std::error::Error;

        impl #builder_ident {
            #(#builder_setter_fn)*

            pub fn build(&mut self) -> Result<#orig_name, Box<dyn Error>> {
                #(#checks)*
                Ok(#orig_name{
                    #(#orig_init_required_args_from_builder),*
                })
            }
        }

    };

    builder_struct.into()
}
