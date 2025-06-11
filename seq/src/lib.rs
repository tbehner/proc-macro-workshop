use proc_macro2::{Span, TokenStream, TokenTree, Group};
use quote::quote;
use std::ops::Range;
use syn::{braced, parse::Parse, parse_macro_input, parse_quote, visit_mut, visit_mut::VisitMut, Expr, ExprRange, Ident, Lit, LitInt, Token};

#[derive(Debug)]
struct Seq {
    ident: Ident,
    range: ExprRange,
    tokens: proc_macro2::TokenStream,
}

fn get_range(input: &ExprRange) -> Range<u16> {
    let start_expr = *input.start.clone().unwrap();
    let start = get_limit(start_expr);
    let end_expr = *input.end.clone().unwrap();
    let end = get_limit(end_expr);
    start..end
}

struct ReplaceIdent{
    ident: Ident,
    replacement: u16,
}

impl ReplaceIdent {
    fn new(ident: Ident, replacement: u16) -> ReplaceIdent {
        ReplaceIdent { ident: ident, replacement: replacement }
    }
}

impl VisitMut for ReplaceIdent {

    // fn visit_token_stream_mut(&mut self, i: &mut proc_macro2::TokenStream) {
    //     let ident_res: syn::Result<Ident> = syn::parse2(i.clone());
    //     if let Ok(ident) = ident_res {
    //         panic!("Any Identifier found!");
    //         if ident.to_string() == self.ident.to_string() {
    //             panic!("Identifier found!");
    //             let value = self.replacement;
    //             *i = quote!{ #value };
    //         } else {
    //             panic!("differen identifier: {}", ident.to_string());
    //         }
    //     } 
    // }

    fn visit_expr_mut(&mut self, i: &mut syn::Expr) {
        if let Expr::Path(p_expr) = i {
            if let Some(path_ident) = p_expr.path.get_ident() {
                if *path_ident.to_string() == self.ident.to_string() {
                    let value = self.replacement;
                    *i = parse_quote!(#value);
                }
            } else {
                panic!("Found different identifier: {:?}", p_expr);
            }
        }
        visit_mut::visit_expr_mut(self, i);
    }
}

impl Seq {
    fn get_ident(&self) -> &Ident {
        &self.ident
    }

    fn get_range(&self) -> Range<u16> {
        get_range(&self.range)
    }

    fn subs(&self, stream: &TokenStream, num: u16) -> TokenStream {
        let mut out: Vec<TokenTree> = vec![];
        for t in stream.clone().into_iter() {
            match &t {
                i@TokenTree::Ident(ref ident) => {
                    if ident.to_string() == self.ident.to_string() {
                        // push the expr to out
                        let num_word = format!("{}", num);
                        let num_tokens: TokenStream = num_word.parse().unwrap();
                        let num_stream = quote! {#num_tokens};
                        out.extend(num_stream.into_iter());
                    } else {
                        out.push(i.clone());
                    }
                }
                g@TokenTree::Group(ref gr) => {
                    // recurse on gr.stream
                    let new_stream = self.subs(&gr.stream(), num);
                    let mut new_group = Group::new(gr.delimiter(), new_stream);
                    new_group.set_span(gr.span());
                    out.push(TokenTree::Group(new_group));
                }
                lit@TokenTree::Literal(_) => {
                    out.push(lit.clone());
                }
                p@TokenTree::Punct(_) => {
                    out.push(p.clone());
                }
            }
        }
        TokenStream::from_iter(out)
    }

    fn quote(&self) -> TokenStream {
        let all: Vec<_> = self.get_range().map(|n| {
             self.subs(&self.tokens, n)
        }).collect();
        quote!{#(#all)*}
    }
}

fn get_limit(expr: Expr) -> u16 {
    if let Expr::Lit(lit_expr) = expr {
        if let Lit::Int(lit_int) = lit_expr.lit {
            let value = lit_int.base10_parse::<u16>().unwrap();
            value
        } else {
            panic!("Not a integer at start of range!");
        }
    } else {
        panic!("Not a literal at start of range!");
    }
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

    let code = seq.quote();

    return code.into();

    panic!("{}", code);
    let ghost_code = quote!{};
    ghost_code.into()

}
