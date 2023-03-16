use std::fmt::Display;
use std::borrow::Cow;

#[derive(Default)]
pub(crate) struct _StrParts<'a, const REV: bool>{
    parts: Vec<Cow<'a, str>>,
    len: usize
}

pub(crate) type StrParts<'a> = _StrParts<'a, false>;

impl<'a, const REV: bool> _StrParts<'a, REV>{
    pub(crate) fn push(&mut self, part: Cow<'a, str>){
        self.len += part.len();
        self.parts.push(part);
    }

    pub(crate) fn len(&self)->usize{
        self.len
    }
}

impl<'a, const REV: bool> Extend<&'a str> for _StrParts<'a, REV>{
    fn extend<T: IntoIterator<Item = &'a str>>(&mut self, iter: T) {
        let mut total_len = 0usize;
        let i = iter.into_iter().inspect(|p| total_len += p.len()).map(Cow::Borrowed);
        self.parts.extend(i);
        self.len += total_len;
    }
}

impl<'a, const REV: bool> Extend<String> for _StrParts<'a, REV>{
    fn extend<T: IntoIterator<Item = String>>(&mut self, iter: T) {
        let mut total_len = 0usize;
        let i = iter.into_iter().inspect(|p| total_len += p.len()).map(Cow::Owned);
        self.parts.extend(i);
        self.len += total_len;
    }
}

impl Display for _StrParts<'_, false>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for p in &self.parts{
            write!(f, "{}", p.as_ref())?;
        }
        Ok(())
    }
}

impl Display for _StrParts<'_, true>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for p in self.parts.iter().rev(){
            write!(f, "{}", p.as_ref())?;
        }
        Ok(())
    }
}