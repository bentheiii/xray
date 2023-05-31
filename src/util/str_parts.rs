use std::borrow::Cow;
use std::collections::VecDeque;
use std::fmt::Display;
use std::iter;

#[derive(Default)]
pub(crate) struct StrParts<'a> {
    parts: VecDeque<Cow<'a, str>>,
    len: usize,
}

impl StrParts<'_> {
    pub(crate) fn len(&self) -> usize {
        self.len
    }
}

impl<'a> From<&'a str> for StrParts<'a> {
    fn from(value: &'a str) -> Self {
        iter::once(value).collect()
    }
}

impl From<String> for StrParts<'_> {
    fn from(value: String) -> Self {
        iter::once(value).collect()
    }
}

impl<I> FromIterator<I> for StrParts<'_>
where
    Self: Extend<I>,
{
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let mut ret = Self::default();
        ret.extend(iter);
        ret
    }
}

impl<'a> Extend<&'a str> for StrParts<'a> {
    fn extend<T: IntoIterator<Item = &'a str>>(&mut self, iter: T) {
        let mut total_len = 0usize;
        let i = iter
            .into_iter()
            .inspect(|p| total_len += p.len())
            .map(Cow::Borrowed);
        self.parts.extend(i);
        self.len += total_len;
    }
}

impl Extend<String> for StrParts<'_> {
    fn extend<T: IntoIterator<Item = String>>(&mut self, iter: T) {
        let mut total_len = 0usize;
        let i = iter
            .into_iter()
            .inspect(|p| total_len += p.len())
            .map(Cow::Owned);
        self.parts.extend(i);
        self.len += total_len;
    }
}

impl<'a> Extend<Cow<'a, str>> for StrParts<'a> {
    fn extend<T: IntoIterator<Item = Cow<'a, str>>>(&mut self, iter: T) {
        let mut total_len = 0usize;
        let i = iter.into_iter().inspect(|p| total_len += p.len());
        self.parts.extend(i);
        self.len += total_len;
    }
}

impl Extend<Self> for StrParts<'_> {
    fn extend<T: IntoIterator<Item = Self>>(&mut self, iter: T) {
        let mut total_len = 0usize;
        let i = iter
            .into_iter()
            .inspect(|p| total_len += p.len())
            .flat_map(|p| p.parts);
        self.parts.extend(i);
        self.len += total_len;
    }
}

impl Display for StrParts<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for p in &self.parts {
            write!(f, "{}", p.as_ref())?;
        }
        Ok(())
    }
}
