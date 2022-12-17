use std::borrow::Cow;
use std::iter;
use std::fmt::Debug;
use std::hash::Hash;
use derivative::Derivative;
use regex::{Regex};
use string_interner::backend::Backend;
use string_interner::Symbol;

#[derive(Default)]
pub(crate) struct SpecialPrefixBackend<B: Backend>{
    items: Vec<Option<Cow<'static, str>>>,
    inner: B,
}

lazy_static! {
    static ref RE: Regex = Regex::new("^item([0-9]+)").unwrap();
}

#[derive(Derivative)]
#[derivative(Eq(bound = ""), Copy(bound=""), PartialEq(bound=""), Clone(bound=""), Debug(bound="B::Symbol: Debug"), Hash(bound="B::Symbol: Hash"))]
pub enum SpecialPrefixSymbol<B: Backend>{
    Regular(B::Symbol),
    Item(usize),
}

impl<B: Backend> Symbol for SpecialPrefixSymbol<B>{
    fn try_from_usize(_index: usize) -> Option<Self> {
        unimplemented!()
    }

    // our working assumption is that these symbol's methods will never be used
    fn to_usize(self) -> usize {
        unimplemented!()
    }
}

impl<B: Backend> Backend for SpecialPrefixBackend<B>{
    type Symbol = SpecialPrefixSymbol<B>;

    fn with_capacity(cap: usize) -> Self {
        Self{
            inner: B::with_capacity(cap),
            items: Vec::new(),
        }
    }

    fn intern(&mut self, string: &str) -> Self::Symbol {
        if let Some(m) = RE.captures(string) {
            let idx: usize = m[1].parse().unwrap();
            if self.items.len() <= idx || self.items[idx].is_none(){
                self.items.extend(iter::repeat(None).take(idx-self.items.len()-1));
                self.items.push(Some(Cow::Owned(format!("item{idx}"))));
            }
            SpecialPrefixSymbol::Item(idx)
        } else {
            SpecialPrefixSymbol::Regular(self.inner.intern(string))
        }
    }

    fn intern_static(&mut self, string: &'static str) -> Self::Symbol {
        if let Some(m) = RE.captures(string) {
            let idx: usize = m[1].parse().unwrap();
            if self.items.len() <= idx || self.items[idx].is_none(){
                self.items.extend(iter::repeat(None).take(idx-self.items.len()-1));
                self.items.push(Some(Cow::Borrowed(string)));
            }
            SpecialPrefixSymbol::Item(idx)
        } else {
            SpecialPrefixSymbol::Regular(self.inner.intern(string))
        }
    }

    fn shrink_to_fit(&mut self) {
        self.inner.shrink_to_fit();
        self.items.shrink_to_fit();
    }

    fn resolve(&self, symbol: Self::Symbol) -> Option<&str> {
        match symbol {
            SpecialPrefixSymbol::Item(idx) => self.items.get(idx).unwrap().as_deref(),
            SpecialPrefixSymbol::Regular(sym) => self.inner.resolve(sym)
        }
    }

    unsafe fn resolve_unchecked(&self, symbol: Self::Symbol) -> &str {
        match symbol {
            SpecialPrefixSymbol::Item(idx) => self.items.get(idx).unwrap().as_deref().unwrap(),
            SpecialPrefixSymbol::Regular(sym) => self.inner.resolve_unchecked(sym)
        }
    }
}