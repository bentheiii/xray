pub trait TryExtend<T> {
    fn try_extend<E, I: Iterator<Item = Result<T, E>>>(&mut self, i: I) -> Result<(), E>;
}

impl<T> TryExtend<T> for Vec<T> {
    fn try_extend<E, I: Iterator<Item = Result<T, E>>>(&mut self, i: I) -> Result<(), E> {
        let (min, max) = i.size_hint();
        self.reserve_exact(max.unwrap_or(min));
        for item in i {
            self.push(item?);
        }
        Ok(())
    }
}
