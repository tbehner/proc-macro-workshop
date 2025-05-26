use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::Parse, parse_macro_input, Data, DeriveInput, Expr, ExprAssign, Ident, LitStr, Meta, Result};

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

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {

    let m_input = parse_macro_input!(input as DeriveInput);
    let struct_name = m_input.ident;
    let struct_name_str = struct_name.to_string();

    let mut debug_fields = vec![];
    if let Data::Struct(struc) = m_input.data {
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
                    write!(f, "{}: {:?}, ", #ident_str, &self.#ident)?;
                }
            }
        }
 
    }).collect();

    quote! {
        impl std::fmt::Debug for #struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{} {{ ", #struct_name_str)?;
                #(#debug_fields_invocation)*
                write!(f, "}}")
            }
        }

    }.into()
}
