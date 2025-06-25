use itertools::{Itertools, MultiPeek};
use proc_macro2::{token_stream, Delimiter, Group, TokenStream, TokenTree};
use quote::quote;
use std::{io::repeat, ops::Range};
use syn::{braced, parse::Parse, parse_macro_input, Expr, ExprRange, Ident, Lit, Token};

/// Get a literal integer from an Expr
fn get_int_from_expr(expr: Expr) -> u16 {
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

#[derive(Debug)]
struct Seq {
    ident: Ident,
    range: ExprRange,
    tokens: proc_macro2::TokenStream,
}

fn clone_group_with_stream(old_group: &Group, stream: TokenStream) -> Group {
    let mut new_group = Group::new(old_group.delimiter(), stream);
    new_group.set_span(old_group.span());
    new_group
}

impl Seq {

    fn get_range(&self) -> Range<u16> {
        let start_expr = *self.range.start.clone().unwrap();
        let start = get_int_from_expr(start_expr);
        let end_expr = *self.range.end.clone().unwrap();
        let end = get_int_from_expr(end_expr);
        match self.range.limits {
            syn::RangeLimits::HalfOpen(_) => {
                start..end
            },
            syn::RangeLimits::Closed(_) => {
                start..(end+1)
            }
        }
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
                        // TODO set span respectively
                        let num_word = format!("{}", num);
                        let num_tokens: TokenStream = num_word.parse().unwrap();
                        out.extend(num_tokens.into_iter());
                    } else if self.peek_is_tilde_token(&mut iter) && self.peek_is_ident(&mut iter) {
                        // TODO set span respectively
                        let num_word = format!("{}{}", ident, num);
                        // we get the span from the identifier
                        let num_tokens: TokenStream = num_word.parse().unwrap();
                        if let Some(TokenTree::Ident(new_ident)) = num_tokens.into_iter().next() {
                            let mut new_ident_with_span = new_ident;
                            new_ident_with_span.set_span(ident.span());
                            out.extend(std::iter::once(TokenTree::Ident(new_ident_with_span)));
                        } else {
                            panic!("The identifier I just created cannot be parsed as an identifier...")
                        }
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

    fn next_is_parenthesis_group(&self, iter: &mut MultiPeek<token_stream::IntoIter>) -> bool {
        if let Some(TokenTree::Group(gr)) = iter.peek() {
            gr.delimiter() == Delimiter::Parenthesis
        } else { false }
    }

    fn next_is_punct_star(&self, iter: &mut MultiPeek<token_stream::IntoIter>) -> bool {
        if let Some(TokenTree::Punct(p)) = iter.peek() {
            p.as_char() == '*'
        } else {false}
    }

    fn extract_repeat_section(&self, iter: &mut MultiPeek<token_stream::IntoIter>) -> TokenStream {
        if let Some(TokenTree::Group(gr)) = iter.next() {
            gr.stream().clone()
        } else {
            panic!("You have to identify next token tree as a group first!");
        }
    }

    fn repeat_section(&self, section: TokenStream) -> TokenStream {
        let all: Vec<_> = self.get_range().map(|n| {
            self.subs(&section, n)
        }).collect();
        quote! {#(#all)*}
    }

    /// Find the repeated section in the macro and repeat it
    ///
    /// The returned values are an optional pre- and post-section that are not repeated
    /// as well as the repeated section.
    ///
    /// (pre, repeated_section, post)
    fn identify_repeat_section(&self, stream: &TokenStream) -> (Option<TokenStream>, TokenStream, Option<TokenStream>) {
        // find a Punct(#), followed by a Group with paranthesis as delimiter, followed by a Punct(*)
        let mut pre = TokenStream::new();
        let mut repeat_section = None;
        let mut post = TokenStream::new();
        let mut iter = stream.clone().into_iter().multipeek();

        while let Some(t) = iter.next() {
            match &t {
                tt_punct@TokenTree::Punct(c) if c.as_char() == '#' => {
                    if self.next_is_parenthesis_group(&mut iter) && self.next_is_punct_star(&mut iter) {
                        // repeat this section
                        repeat_section = Some(self.repeat_section(self.extract_repeat_section(&mut iter)));
                        // also skip the '*'
                        iter.next();
                    } else {
                        // not it
                        pre.extend(Some(tt_punct.clone()));
                    }
                }

                token_tree_group@TokenTree::Group(ref gr) => {
                    // if we have *not* found a section yet, descend into the group
                    if repeat_section.is_none() {
                        let (maybe_pre, sec, maybe_post) = self.identify_repeat_section(&gr.stream());
                        // if the pre and post are not None: add the pre to pre, the post to post and set the section
                        if let (Some(pre_sec), Some(post_sec)) = (maybe_pre, maybe_post) {
                            // repeat sec here then create group with [pre, #(sec)*, post] as stream

                            let mut complete = TokenStream::new();
                            complete.extend(pre_sec);
                            complete.extend(sec);
                            complete.extend(post_sec);

                            let group_section = clone_group_with_stream(gr, complete);

                            repeat_section = Some(TokenStream::from_iter([TokenTree::Group(group_section)]));
                        } else {
                            // push the entire group to pre
                            pre.extend(Some(token_tree_group.clone()));
                        }
                    } else {
                        // else add the group to post
                        post.extend(Some(token_tree_group.clone()));
                    }
                }
                _ => {
                    if repeat_section.is_none() {
                        pre.extend(Some(t));
                    } else {
                        post.extend(Some(t));
                    }
                },
            }
        }

        if let Some(ts) = repeat_section {
            (Some(pre), ts, Some(post))
        } else {
            (None, self.tokens.clone(), None)
        }
    }

    /// Generate the TokenStream with a repeated section.
    ///
    /// Entry point to the macro.
    pub fn quote(&self) -> TokenStream {
        let (maybe_pre, repeat_section , maybe_post)= self.identify_repeat_section(&self.tokens);
        // repeat_section should be equal to self.tokens, since there is no repeat section,
        // and pre and post should be None
        if let (Some(pre), Some(post)) = (maybe_pre, maybe_post) {
            quote!{#pre #repeat_section #post}
        } else {
            let repeated_section = self.repeat_section(repeat_section);
            quote!{#repeated_section}
        }
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
    // panic!("{:?}", seq.tokens);

    let code = seq.quote();

    return code.into();

    panic!("{}", code);
    let ghost_code = quote!{};
    ghost_code.into()

}
