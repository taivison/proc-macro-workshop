use proc_macro2::{Delimiter, Group, Ident, Literal, TokenStream, TokenTree};
use quote::TokenStreamExt;
use syn::buffer::{Cursor, TokenBuffer};

pub(super) struct Seq {
    ident: syn::Ident,
    start: usize,
    end: usize,
    inclusive: bool,
    tokens: TokenBuffer,
}

impl syn::parse::Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        _ = input.parse::<syn::Token![in]>()?;
        let start = input.parse::<syn::LitInt>()?.base10_parse()?;

        let inclusive = input.peek(syn::Token![..=]);
        if inclusive {
            _ = input.parse::<syn::Token![..=]>()?;
        } else {
            _ = input.parse::<syn::Token![..]>()?;
        }

        let end = input.parse::<syn::LitInt>()?.base10_parse()?;
        let f;
        syn::braced!(f in input);
        Ok(Self {
            ident,
            start,
            end,
            inclusive,
            tokens: TokenBuffer::new2(f.parse()?),
        })
    }
}

impl Into<proc_macro::TokenStream> for Seq {
    fn into(self) -> proc_macro::TokenStream {
        match self.try_expand_section() {
            Some(t) => t.into(),
            None => self.expand_all().into(),
        }
    }
}

impl Seq {
    fn try_expand_section(&self) -> Option<TokenStream> {
        let mut expanded = false;
        let stream = self.try_expand_section_recurse(&self.tokens, &mut expanded);
        if expanded {
            Some(stream)
        } else {
            None
        }
    }

    fn try_expand_section_recurse(&self, buffer: &TokenBuffer, expanded: &mut bool) -> TokenStream {
        let mut stream = TokenStream::new();
        let mut cursor = buffer.begin();
        while let Some((mut tree, next_cursor)) = cursor.token_tree() {
            match self.try_find_section(&mut tree, next_cursor, expanded) {
                Some((inner, next_cursor)) => {
                    cursor = next_cursor;
                    let streams = self.range().map(|v| self.replace_section(inner, v));
                    stream.extend(streams);
                    *expanded = true;
                }
                None => {
                    cursor = next_cursor;
                    stream.append(tree);
                }
            }
        }

        stream
    }

    fn try_find_section<'a>(
        &'a self,
        tree: &mut TokenTree,
        cursor: Cursor<'a>,
        expanded: &mut bool,
    ) -> Option<(Cursor<'a>, Cursor<'a>)> {
        match tree {
            TokenTree::Punct(p) if p.as_char() == '#' => cursor
                .group(Delimiter::Parenthesis)
                .and_then(|(inner, _, next)| match next.is_punct('*') {
                    Some(c) => Some((inner, c)),
                    None => None,
                }),
            TokenTree::Group(g) => {
                let mut group_expanded = false;
                let mut group = Group::new(
                    g.delimiter(),
                    self.try_expand_section_recurse(
                        &TokenBuffer::new2(g.stream()),
                        &mut group_expanded,
                    ),
                );
                group.set_span(g.span());
                *tree = group.into();
                *expanded = group_expanded;
                None
            }
            _ => None,
        }
    }

    fn expand_all(&self) -> TokenStream {
        let streams = self
            .range()
            .map(|v| self.replace_token_stream(&self.tokens, v));

        TokenStream::from_iter(streams)
    }

    fn range(&self) -> impl Iterator<Item = usize> {
        if self.inclusive {
            self.start..(self.end + 1)
        } else {
            self.start..self.end
        }
    }

    fn replace_section<'a>(&'a self, mut cursor: Cursor<'a>, value: usize) -> TokenStream {
        let mut stream = TokenStream::new();
        while let Some((tree, next_cursor)) = cursor.token_tree() {
            let (tree, next_cursor) = self.replace_token_tree(tree, value, next_cursor);
            cursor = next_cursor;
            stream.append(tree);
        }

        stream
    }

    fn replace_token_stream(&self, buffer: &TokenBuffer, value: usize) -> TokenStream {
        let mut cursor = buffer.begin();
        let mut stream = TokenStream::new();
        while let Some((tree, next_cursor)) = cursor.token_tree() {
            let (tree, next_cursor) = self.replace_token_tree(tree, value, next_cursor);
            cursor = next_cursor;
            stream.append(tree);
        }

        stream
    }

    fn replace_token_tree<'a>(
        &'a self,
        tree: TokenTree,
        value: usize,
        cursor: Cursor<'a>,
    ) -> (TokenTree, Cursor) {
        match tree {
            TokenTree::Group(g) => {
                let buffer = TokenBuffer::new2(g.stream());
                let mut group =
                    Group::new(g.delimiter(), self.replace_token_stream(&buffer, value));
                group.set_span(g.span());

                (group.into(), cursor)
            }
            TokenTree::Ident(ref i) if &self.ident == i => {
                let mut lit = Literal::usize_unsuffixed(value);
                lit.set_span(i.span());
                (lit.into(), cursor)
            }
            TokenTree::Ident(ref i) => cursor
                .is_punct('~')
                .and_then(|c| c.is_ident(&self.ident))
                .map_or_else(
                    || (i.clone().into(), cursor),
                    |c| (Ident::new(&format!("{}{}", i, value), i.span()).into(), c),
                ),
            _ => (tree, cursor),
        }
    }
}

//Extra helper methods on Cursor
trait CursorExt<'a> {
    fn is_punct(self, punct: char) -> Option<Cursor<'a>>;
    fn is_ident(self, ident: &Ident) -> Option<Cursor<'a>>;
}

impl<'a> CursorExt<'a> for Cursor<'a> {
    fn is_punct(self, punct: char) -> Option<Cursor<'a>> {
        match self.punct() {
            Some((ref p, cursor)) if p.as_char() == punct => Some(cursor),
            _ => None,
        }
    }

    fn is_ident(self, ident: &Ident) -> Option<Cursor<'a>> {
        match self.ident() {
            Some((ref i, cursor)) if i == ident => Some(cursor),
            _ => None,
        }
    }
}
