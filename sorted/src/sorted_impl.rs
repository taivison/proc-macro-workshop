use std::cmp::Ordering;
use std::fmt::Display;

use proc_macro2::{Span, TokenStream};
use quote::ToTokens;

use syn::visit_mut::VisitMut;
use syn::{Item, Path};

pub(super) fn is_sorted(item: &Item) -> syn::Result<()> {
    let e = match item {
        Item::Enum(e) => e,
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "expected enum or match expression",
            ))
        }
    };

    check_order(e.variants.iter().map(|v| &v.ident), e.variants.len())?;

    Ok(())
}

fn compare_variants(this: &str, other: &str) -> Ordering {
    if this == "_" && other != "_" {
        Ordering::Greater
    } else if other == "_" && this != "_" {
        Ordering::Less
    } else {
        this.cmp(other)
    }
}

fn check_order<I, T>(variants: I, size: usize) -> syn::Result<()>
where
    I: Iterator<Item = T>,
    T: ToTokens + Display,
{
    if size < 2 {
        return Ok(());
    }
    let mut variants_order: Vec<String> = Vec::with_capacity(size);

    for i in variants {
        let value = i.to_string();
        match variants_order.binary_search_by(|v| compare_variants(v, &value)) {
            Ok(_) => return Ok(()), //If we reached here, there are duplicates, so let the compiler generate the error
            Err(p) if p == variants_order.len() => variants_order.push(i.to_string()), //Is Sorted
            Err(p) => {
                let before = &variants_order[p];

                return Err(syn::Error::new_spanned(
                    &i,
                    &format!("{} should sort before {}", &i, before),
                ));
            }
        }
    }

    Ok(())
}

pub(super) struct MatchExprChecker {
    errors: Vec<syn::Error>,
}

impl MatchExprChecker {
    pub(super) fn new() -> Self {
        Self { errors: vec![] }
    }
    pub(super) fn generate_errors(&self) -> TokenStream {
        TokenStream::from_iter(self.errors.iter().map(|e| e.to_compile_error()))
    }
}

impl VisitMut for MatchExprChecker {
    fn visit_expr_match_mut(&mut self, i: &mut syn::ExprMatch) {
        // if !i.attrs.contains(&self.attribute) {
        //     return;
        // }
        if !i.attrs.iter().any(|a| a.path().is_ident("sorted")) {
            return;
        }

        i.attrs.retain(|a| !a.path().is_ident("sorted"));

        let paths = i
            .arms
            .iter()
            .map(|arm| {
                dbg!(arm);
                match arm.pat {
                    syn::Pat::Path(ref p) => Ok(p.path.clone().into()),
                    syn::Pat::Struct(ref s) => Ok(s.path.clone().into()),
                    syn::Pat::TupleStruct(ref t) => Ok(t.path.clone().into()),
                    syn::Pat::Wild(ref u) => Ok(u.underscore_token.into()),
                    syn::Pat::Ident(ref i) => Ok(i.ident.clone().into()),
                    _ => Err(syn::Error::new_spanned(
                        &arm.pat,
                        "unsupported by #[sorted]",
                    )),
                }
            })
            .collect::<syn::Result<Vec<PathWrapper>>>();

        let paths = match paths {
            Ok(p) => p,
            Err(e) => {
                self.errors.push(e);
                return;
            }
        };

        match check_order(paths.into_iter(), i.arms.len()) {
            Ok(_) => {}
            Err(e) => self.errors.push(e),
        };
    }
}

#[repr(transparent)]
struct PathWrapper(Path);

impl Display for PathWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = self
            .0
            .segments
            .iter()
            .map(|p| p.ident.to_string())
            .collect::<Vec<_>>()
            .join("::");
        write!(f, "{}", output)
    }
}

impl ToTokens for PathWrapper {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
    }
}

impl<T> From<T> for PathWrapper
where
    T: Into<Path>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}
