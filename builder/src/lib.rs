use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use syn::{ parse::Parse, parse_macro_input, Data, DeriveInput, Expr, ExprAssign, GenericArgument, Lit, Meta, PathArguments, Type };
use std::error::Error;

fn get_concrete_type(ty: &Type) -> Result<Type, Box<dyn Error>> {
    match ty {
        Type::Path(type_path) => {
            if let Some(segment) = type_path.path.segments.iter().next() {

                if segment.ident != "Vec" {
                    return Err("not an vector type".into());
                }

                match &segment.arguments {
                    PathArguments::AngleBracketed(angle_bracketed_arg) => {
                        if let Some(generic_arg) = angle_bracketed_arg.args.iter().next() {
                            match generic_arg {
                                GenericArgument::Type(ty) => Ok(ty.to_owned()),
                                _ => Err("weird vector type".into())
                            }
                        } else {
                            Err("no type argument for vector type".into())
                        }
                    },
                    _ => Err("not an vector type".into()),
                }
            } else {
                Err("not an vector type".into())
            }
        },
        _ => Err("not an vector type".into())
    }
}

fn strip_optional_type(ty: &Type) -> Result<Type, Box<dyn Error>> {
    match ty {
        Type::Path(type_path) => {
            if let Some(segment) = type_path.path.segments.iter().next() {

                if segment.ident != "Option" {
                    return Err("not an optional type".into());
                }

                match &segment.arguments {
                    PathArguments::AngleBracketed(angle_bracketed_arg) => {
                        if let Some(generic_arg) = angle_bracketed_arg.args.iter().next() {
                            match generic_arg {
                                GenericArgument::Type(ty) => Ok(ty.to_owned()),
                                _ => Err("weird optional type".into())
                            }
                        } else {
                            Err("no type argument for opional type".into())
                        }
                    },
                    _ => Err("not an optional type".into()),
                }
            } else {
                Err("not an optional type".into())
            }
        },
        _ => Err("not an optional type".into())
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

struct BuilderAttribute{
    name: Option<String>,
}

impl Parse for BuilderAttribute {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let initial_span = input.to_string();
        let assignment: ExprAssign = match input.parse() {
            Ok(a) => a,
            Err(_) => {
                return Ok(BuilderAttribute { name: None })
            }
        };
        let mut name = None;
        let assignment_lhs = assignment.left.to_token_stream().to_string();

        if assignment_lhs != "each" {
            let msg = format!("input: {}", initial_span);
            let _actual_input = "expected `builder(each = \"...\")`";
            return Err(syn::Error::new_spanned(assignment.to_token_stream(),&msg));
        }

        if let Expr::Lit(expr_lit) = *assignment.right {
            if let Lit::Str(s) = expr_lit.lit {
                name = Some(s.value())
            }
        }

        Ok(BuilderAttribute { name })
    }
}

struct StructField {
    ident: Ident,
    ty: Type,
    single_init: Option<BuilderAttribute>,
}

impl StructField {
    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }
}


#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let m_input = parse_macro_input!(input as DeriveInput);
    let builder_name = format!("{}Builder", &m_input.ident);
    let orig_name: Ident = m_input.ident;

    let builder_ident = Ident::new(&builder_name, Span::call_site());

    let mut named_fields = vec![];
    if let  Data::Struct(data_struct) = m_input.data {
        for field in data_struct.fields.iter() {
            let single_init_attr: Option<BuilderAttribute> = match field.attrs.first() {
                Some(attr) => {
                    match attr.parse_args() {
                        Ok(builder_attr) => Some(builder_attr),
                        Err(_e) => {
                            let list_span = if let Meta::List(meta_list) = &attr.meta {
                                meta_list.to_token_stream()
                            } else {
                                attr.to_token_stream()
                            };
                            let actual_input = "expected `builder(each = \"...\")`";
                            return syn::Error::new_spanned(list_span,&actual_input).to_compile_error().into();
                        },
                    }
                }
                None => None,
            };

            named_fields.push(
                StructField{
                    ident: field.ident.clone().unwrap(),
                    ty: field.ty.clone(),
                    single_init: single_init_attr,
            })
        }
    }

    let required_fields: Vec<_>= named_fields.iter().map(|field| {
        if field.single_init.is_some() || is_optional_type(field.ty()) {
            let ident = field.ident();
            let ty = field.ty();
            quote! {#ident: #ty}
        } else {
            let ident = field.ident();
            let ty = field.ty();
            let option_type = quote!{::core::option::Option<#ty>};
            quote! {#ident: #option_type}
        }
    }).collect();

    let builder_init_args: Vec<_>= named_fields.iter().map(|field|{
        let ident = field.ident();
        if field.single_init.is_some() {
            quote!{#ident: vec![]}
        } else {
            quote!{#ident: None}
        }
    }).collect();

    let builder_setter_fn: Vec<_> = named_fields.iter().map(|field|{
        if let Some(each_init) = &field.single_init {

            let single_type = get_concrete_type(field.ty()).expect("type");
            let ty = field.ty();
            let ident = field.ident();
            let each_ident_name = each_init.name.as_ref().expect("a valid method name");

            if each_ident_name.is_empty() {
                panic!("a method name cannot be empty!")
            }
            let each_ident = Ident::new(&each_ident_name, Span::call_site());

            if ident.to_string() == each_ident.to_string() {
                quote!{
                   pub fn #each_ident(&mut self, #each_ident: #single_type) -> &mut Self {
                        self.#ident.push(#each_ident);
                        self
                    }
                }
            } else {
                quote!{
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = #ident;
                        self
                    }

                    pub fn #each_ident(&mut self, #each_ident: #single_type) -> &mut Self {
                        self.#ident.push(#each_ident);
                        self
                    }
                }
            }
        } else if is_optional_type(field.ty()) {
            let stripped_type = strip_optional_type(field.ty()).expect("stripped type");
            let ident = field.ident();
            quote!{
                pub fn #ident(&mut self, #ident: #stripped_type) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        } else {
            let ident = field.ident();
            let ty = field.ty();
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
    }).collect();

    // parameters for the initialization of the original struct
    // Original {
    //    name: name.field.as_ref().unwrap().to_owned() // for required fields of type T
    //    many: many.clone() // for Vec<_> Types
    //    op: op.clone() // for optional types Option<T>
    // }
    let orig_init_required_args_from_builder: Vec<_> = named_fields.iter().map(|field|{
        let ident = field.ident();
        if is_optional_type(field.ty()) || field.single_init.is_some() {
            quote! {#ident: self.#ident.clone()}
        } else {
            quote! {#ident: self.#ident.as_ref().unwrap().to_owned()}
        }
    }).collect();

    let checks: Vec<_> = named_fields.iter().filter_map(|field|{
        if is_optional_type(field.ty()) || field.single_init.is_some(){
            return None;
        }
        let ident = field.ident();
        let ident_name = ident.to_token_stream().to_string();
        Some(quote! {
            if self.#ident.is_none() {
                return Err(format!("{} missing", #ident_name).into());
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

            pub fn build(&mut self) -> ::core::result::Result<#orig_name, std::boxed::Box<dyn std::error::Error>> {
                #(#checks)*
                Ok(#orig_name{
                    #(#orig_init_required_args_from_builder),*
                })
            }
        }
    };

    builder_struct.into()
}
