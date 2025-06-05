use std::collections::HashMap;

use itertools::Itertools;
use proc_macro::TokenStream;
use quote::{quote, ToTokens,};
use syn::{parenthesized, parse::Parse, parse_macro_input, token::Paren, Data, DeriveInput, Expr, ExprAssign, GenericArgument, Ident, Lit, LitStr, Meta, PathArguments, PathSegment, Token, Type};

struct DebugField {
    name: Ident,
    custom_fmt: Option<CustomDebugFmt>,
}

impl DebugField {
    pub fn name_as_str(&self) -> String {
        self.name.to_string()
    }

    pub fn ident(&self) -> &Ident {
        &self.name
    }
}

struct CustomDebugFmt(String);

impl Parse for CustomDebugFmt {
   fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {

       input.parse()

           .map_err(|_e| {
               let err_msg = format!("------------>>>>> Got input: {}", input);
               syn::Error::new(input.span(), err_msg)
           })
   }
}

fn type_path_seg(ty: &Type) -> Vec<PathSegment> {
    Some(ty).and_then(|ty| 
        match ty {
            Type::Path(type_path) => Some(type_path),
            _ => None,
        })
    .map(|type_path| {
        let path = type_path.path.clone();
        path.segments.iter().map(|ps| ps.to_owned()).collect()
    }).unwrap_or_default()
}

#[derive(Debug)]
struct BoundAttribute {
    value: String,
}

impl Parse for BoundAttribute {
   fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
       let assignment: ExprAssign = input.parse()?;
       if let Expr::Lit(exp_lit) = *assignment.right.to_owned() {
           if let Lit::Str(str_lit) = exp_lit.lit {
               return Ok(BoundAttribute{value: str_lit.value()})
           } else {
               return Err(syn::Error::new_spanned(assignment.into_token_stream(), "foooo: cannot be parsed!"));
           }
       } else {
           return Err(syn::Error::new_spanned(assignment.into_token_stream(), "foooo: cannot be parsed!"));
       }
   }
}

fn get_generic_type_parameters(path_segment: &PathSegment) -> Vec<Type> {
    Some(path_segment)
    .and_then(|path_segment| 
        match &path_segment.arguments{
            PathArguments::AngleBracketed(path_args) => Some(path_args.clone()),
            _ => None
        })
    .map(|path_args| {
        let type_args: Vec<_> = path_args.args.iter().filter_map(|a|
            match a {
                GenericArgument::Type(t) => Some(t.clone()),
                _ => None
            }).collect();
        type_args
    }).unwrap_or_default()
}

fn type_matches(ty: &Type, name: &str) -> bool {
    match ty {
        Type::Path(path_type) => {
            path_type.path.segments.iter().any(|segment| segment.ident == name)
        },
        _ => false,
    }
}

fn get_last_ident_from_path(ty: &Type) -> Option<Ident> {
    match ty {
        Type::Path(type_path) => type_path.path.get_ident().map(|i| i.to_owned()),
        _ => None,
    }
}

fn get_bound_attribute(input: &DeriveInput) -> proc_macro2::TokenStream {
    input.attrs.first().map(|attr|{
        if let Meta::NameValue(name_value) = &attr.meta {
            if name_value.path.get_ident().unwrap() != "bound" {
                panic!("Unknown path in {:?}", attr);
            }

            if let Expr::Lit(literal_expr) = &name_value.value {
                if let Lit::Str(literal_str) = &literal_expr.lit {
                    quote! {#literal_str}
                } else {
                    panic!("Unknown literal in {:?}", attr);
                }
            } else {
                panic!("Unknown value in {:?}", attr);
            }
        } else {
            panic!("Not a name value attribute: {:?}", attr);
        }
    }).unwrap_or_default()
}

#[proc_macro_derive(CustomDebug, attributes(debug,bound))]
pub fn derive(input: TokenStream) -> TokenStream {

    let m_input = parse_macro_input!(input as DeriveInput);
    let bound_attribute: Option<BoundAttribute>  = m_input.attrs.first().and_then(|attr|
        match &attr.meta {
            Meta::List(meta_list) => match meta_list.parse_args() {
                Ok(bound) => Some(bound),
                Err(e) => {
                    panic!("This did not work: {:?}", e);
                }
            }
            _ => None,
        }
    );
    let struct_name = m_input.ident;
    let (impl_generics, ty_generics, _where_clause) = m_input.generics.split_for_impl();
    let type_parameter: Vec<_> = m_input.generics.type_params().collect();
    let type_parameter_str: Vec<_> = type_parameter.iter()
        .map(|tp| tp.ident.to_string())
        .collect();
    let struct_name_str = struct_name.to_string();

    let mut debug_fields = vec![];
    let mut type_parameters: HashMap<Type,Vec<Type>> = HashMap::new();
    if let Data::Struct(ref struc) = m_input.data {
        for field in &struc.fields {
            let path_segments = type_path_seg(&field.ty);
            let mut generic_arguments = vec![];
            for path_segment in &path_segments {
                let types = get_generic_type_parameters(path_segment);
                generic_arguments.extend(types);
            }

            for generic_type_argument in &generic_arguments {
                if let Some(types) = type_parameters.get_mut(generic_type_argument) {
                    types.push(field.ty.clone());
                } else {
                    type_parameters.insert(generic_type_argument.clone(), vec![field.ty.clone()]);
                }
            }
        }
    }

    // type_parameters has exactly one key-value pair and this is the expected T -> PhantomData
    let mut type_excluded_from_guards = vec![];
    for (type_argument, types) in type_parameters.iter() {
        if types.len() == 1 {
            let single_type = types.first().unwrap();
            if type_matches(single_type, "PhantomData") {
                // in the hope, that this the segment of the type with the meat
                let ident = get_last_ident_from_path(type_argument).unwrap();
                type_excluded_from_guards.push(ident.to_string());
            }
        }
    }


    if let Data::Struct(ref struc) = m_input.data {
        for field in &struc.fields {
            let custom_debug_fmt: Option<CustomDebugFmt> = match field.attrs.first() {
                Some(attr) => {
                    Some(attr.meta.clone()).and_then(|m|{
                        match m {
                            Meta::NameValue(nm) => Some(nm),
                            _ => None,
                        }
                    }).and_then(|nm| 
                        match nm.value {
                            syn::Expr::Lit(lit) => Some(lit),
                            _ => None,
                        }
                    ).and_then(|lit| 
                        match lit.lit {
                            syn::Lit::Str(s) => Some(s.value()),
                            _ => None,
                        }
                    ).map(CustomDebugFmt)
                },
                None => None
            };

            debug_fields.push(DebugField{name: field.ident.as_ref().unwrap().to_owned(), custom_fmt: custom_debug_fmt});
        }
    }


    // TODO find associated types
    let mut associated_types_by_identifier = HashMap::new();
    if let Data::Struct(ref struc) = m_input.data {
        for field in &struc.fields {
            let path_segments = type_path_seg(&field.ty);

            if path_segments.len() > 1 {

                let t = path_segments.first().unwrap();

                if type_parameter_str.contains(&t.ident.to_string()){
                    associated_types_by_identifier.insert(t.ident.clone(), field.ty.clone());
                }
            }

            if !path_segments.is_empty() {
                let last_segment = path_segments.last().unwrap();
                match &last_segment.arguments {
                    PathArguments::AngleBracketed(args) => {
                        let first_arg = args.args.first().unwrap();
                        match first_arg {
                            GenericArgument::Type(first_type_arg) => {
                                let path_segments = type_path_seg(first_type_arg);

                                if path_segments.len() >= 1 {

                                    let t = path_segments.first().unwrap();

                                    if type_parameter_str.contains(&t.ident.to_string()){
                                        associated_types_by_identifier.insert(t.ident.clone(), first_type_arg.clone());
                                    }
                                }

                            },
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    let type_bounds: Vec<_> = type_parameter.iter()
        .filter(|tp| !type_excluded_from_guards.contains(&tp.ident.to_string()) )
        .map(|tp| {
            let id = &tp.ident;
            if let Some(assoc_ty) = associated_types_by_identifier.get(&tp.ident) {
                quote!{ #assoc_ty: std::fmt::Debug }
            } else {
                quote!{ #id: std::fmt::Debug }
            }
        }).collect();


    let where_guards = if type_bounds.is_empty() {
        quote!{}
    } else {
        quote!{
            where #(#type_bounds),*
        }
    };

    let impl_head = if type_parameter.is_empty() {
        quote! {
            impl std::fmt::Debug for #struct_name
        }
    } else if let Some(bound) = &bound_attribute{
        let custom: proc_macro2::TokenStream = bound.value.parse().unwrap();
        quote!{
            impl #impl_generics std::fmt::Debug for #struct_name #ty_generics
                where #custom
        }
    } else {
        quote! {
            impl #impl_generics std::fmt::Debug for #struct_name #ty_generics
                #where_guards
        }
    };

    let debug_fields_invocation: Vec<_> = debug_fields.iter().map(|f| {
        let ident = f.ident();
        let ident_str = format!("{}", ident);
        match &f.custom_fmt{
            Some(custom_fmt) => {
                let value = &custom_fmt.0;
                quote!{
                    write!(f, "{}: ", #ident_str)?;
                    write!(f, #value, &self.#ident)?;
                    write!(f, " ")?;
                }
            },
            None => {
                quote! {
                    write!(f, "{}: {:?}", #ident_str, &self.#ident)?;
                }
            }
        }
    })
    .intersperse(quote! {write!(f, ", ")?;})
    .collect();

    let code = quote! {
        #impl_head {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{} {{ ", #struct_name_str)?;
                #(#debug_fields_invocation)*
                write!(f, "}}")
            }
        }
    };

    code.into()
}
