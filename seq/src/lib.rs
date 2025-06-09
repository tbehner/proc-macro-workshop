use proc_macro2::TokenStream;
use quote::quote;
use syn::{braced, parse::Parse, parse_macro_input, Expr, ExprRange, Ident, Lit, LitInt, Token};

struct Seq {
    ident: Ident,
    range: ExprRange,
    tokens: proc_macro2::TokenStream,
}

impl Seq {
    fn get_ident(&self) -> &Ident {
        &self.ident
    }

    fn get_range(&self) {
        let start = *self.range.start.unwrap();
        let end = *self.range.end.unwrap();
    }

    fn quote(&self) -> TokenStream {
        self.tokens.into_iter().map(|t| {
            if let proc_macro2::TokenTree::Ident(i) = t {
                if i.to_string() == self.ident.to_string()
            }
        })

    }
}

fn get_range(input: ExprRange) {
    let start_expr = *input.start.clone().unwrap();
    let start = if let Expr::Lit(lit_expr) = start_expr {
        if let Lit::Int(lit_int) = lit_expr.lit {
            let value = lit_int.base10_parse<u16>().unwrap();
            value
        } else {
            panic!("Not a integer at start of range!");
        }
    } else {
        panic!("Not a literal at start of range!");
    };
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        let _in_token: Token![in] = input.parse()?;
        let range: ExprRange = input.parse()?;
        let content;
        let _braces = braced!(content in input);
        let tokens: proc_macro2::TokenStream = content.parse()?;
        Ok(Seq{ident, range, tokens})
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq: Seq = parse_macro_input!(input as Seq);

    let i = seq.get_ident();
    let code = quote! {  };
    code.into()
}
