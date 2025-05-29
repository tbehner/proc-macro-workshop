use std::collections::HashMap;

use itertools::Itertools;
use proc_macro::TokenStream;
use quote::{quote};
use syn::{parse::Parse, parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, GenericArgument, Ident, LitStr, Meta, PathArguments, PathSegment, Type};

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
           .map(|exr_str: LitStr| CustomDebugFmt(exr_str.value()))
           .map_err(|_e| {
               let err_msg = format!("------------>>>>> Got input: {}", input.to_string());
               syn::Error::new(input.span(), err_msg)
           })
   }
}

fn type_path_seg_containing(ty: &Type, ident_name: &str) -> Option<PathSegment> {
    Some(ty).and_then(|ty| 
        match ty {
            Type::Path(type_path) => Some(type_path),
            _ => None,
        })
    .and_then(|type_path| {
        let path = type_path.path.clone();
        path.segments.iter()
            .find(|type_path| type_path.ident == ident_name)
            .map(|ps| ps.to_owned())
    })
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
    }).unwrap_or(vec![])
}

fn type_matches(ty: &Type, name: &str) -> bool {
    match ty {
        Type::Path(path_type) => {
            path_type.path.segments.iter().find(|segment| segment.ident == name).is_some()
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

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {

    let m_input = parse_macro_input!(input as DeriveInput);
    let struct_name = m_input.ident;
    let type_parameter: Vec<_> = m_input.generics.type_params().collect();
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

    // let mut out = String::new();
    // for (k, v) in type_parameters.iter() {
    //     let dbg = format!("{:?}:      ({}){:?}\n", k, v.len() , v);
    //     out.push_str(&dbg);
    // }
    // panic!("Type Parameters: {:?}\n\n{}\n\n", type_parameters.keys(),out);

    // type_parameters has exactly one key-value pair and this is the expected T -> PhantomData

    let mut type_excluded_from_guards = vec![];
    for (type_argument, types) in type_parameters.iter() {
        if types.len() == 1 {
            let single_type = types.first().unwrap();
            if type_matches(single_type, "PhantomData") {
                // TODO don't insert the type, but type->type-path->path->segments->last->ident
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

    let debug_fields_invocation: Vec<_> = debug_fields.iter().map(|f| {
        let field_name_as_str = f.name_as_str();
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
    
    let type_parameter_list: Vec<_> = type_parameter.iter()
        .map(|tp| &tp.ident)
        .collect();

    // for tp in type_parameter.iter() {
    //     if type_excluded_from_guards.contains(&tp.ident.to_string()) {
    //         panic!("Yup, {:?} is in {:?}", tp.ident.to_string(), type_excluded_from_guards);
    //     } else {
    //         panic!("NOPE, {:?} is not in {:?}", tp.ident.to_string(), type_excluded_from_guards);
    //     }
    // }


    let type_bounds: Vec<_> = type_parameter.iter()
        .filter(|tp| !type_excluded_from_guards.contains(&tp.ident.to_string()) )
        .map(|tp| {
            let id = &tp.ident;
            quote!{ #id: std::fmt::Debug }
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
    } else {
        quote! {
            impl<#(#type_parameter_list),*> std::fmt::Debug for #struct_name<#(#type_parameter_list),*> 
                #where_guards
        }
    };

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
