use crate::forward_err;

pub trait TryExtend<T> {
    fn try_extend<E0, E1, I: Iterator<Item = Result<Result<T, E1>, E0>>>(
        &mut self,
        i: I,
    ) -> Result<Result<(), E1>, E0>;
}

impl<T> TryExtend<T> for Vec<T> {
    fn try_extend<E0, E1, I: Iterator<Item = Result<Result<T, E1>, E0>>>(
        &mut self,
        i: I,
    ) -> Result<Result<(), E1>, E0> {
        let (min, max) = i.size_hint();
        self.reserve_exact(max.unwrap_or(min));
        for item in i {
            self.push(forward_err!(item?));
        }
        Ok(Ok(()))
    }
}
