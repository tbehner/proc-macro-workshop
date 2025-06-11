use itertools::{Itertools, MultiPeek};
use proc_macro2::{token_stream, Group, Span, TokenStream, TokenTree};
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

impl Seq {

    fn get_range(&self) -> Range<u16> {
        get_range(&self.range)
    }

    fn peek_is_tilde_token(&self, iter: &mut MultiPeek<token_stream::IntoIter>) -> bool {
        if let Some(TokenTree::Punct(maybe_tilde)) = iter.peek() {
            maybe_tilde.as_char() == '~'
        } else {
            false
        }
    }

    fn peek_is_ident(&self, iter: &mut MultiPeek<token_stream::IntoIter>) -> bool {
        if let Some(TokenTree::Ident(i)) = iter.peek() {
            self.ident.to_string() == i.to_string()
        } else {
            false
        }
    }


    fn subs(&self, stream: &TokenStream, num: u16) -> TokenStream {
        let mut out: Vec<TokenTree> = vec![];
        let mut iter = stream.clone().into_iter().multipeek();
        while let Some(t) = iter.next() {
            match &t {
                i@TokenTree::Ident(ref ident) => {
                    if ident.to_string() == self.ident.to_string() {
                        let num_word = format!("{}", num);
                        let num_tokens: TokenStream = num_word.parse().unwrap();
                        let num_stream = quote! {#num_tokens};
                        out.extend(num_stream.into_iter());
                    } else if self.peek_is_tilde_token(&mut iter) && self.peek_is_ident(&mut iter) {
                        let num_word = format!("{}{}", ident, num);
                        let num_tokens: TokenStream = num_word.parse().unwrap();
                        let num_stream = quote! {#num_tokens};
                        out.extend(num_stream.into_iter());
                        // since we have found the identifier, we have to extend the iter twice
                        for _ in 0..2 {
                            iter.next();
                        }
                    } else {
                        out.push(i.clone());
                    } 
                }
                TokenTree::Group(ref gr) => {
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
            lit_int.base10_parse::<u16>().expect("a valid integer")
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
