use std::ops::Index;

pub(crate) struct IPush<T>(Vec<T>);

impl<T> Default for IPush<T> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<T> IPush<T> {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn ipush(&mut self, item: T) -> usize {
        self.0.push(item);
        self.0.len() - 1
    }

    pub(crate) fn iter(&self)-> impl Iterator<Item = &T>{
        self.0.iter()
    }
}

impl<T> From<IPush<T>> for Vec<T> {
    fn from(i: IPush<T>) -> Self {
        i.0
    }
}

impl<T, I> Index<I> for IPush<T>
where
    Vec<T>: Index<I>,
{
    type Output = <Vec<T> as Index<I>>::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.0[index]
    }
}

impl<T> IntoIterator for IPush<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a IPush<T> {
    type Item = &'a T;
    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
