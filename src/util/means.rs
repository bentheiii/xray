use num_traits::ToPrimitive;

pub(crate) struct Means<const N: usize>(pub(crate) [f64; N]);

impl<const N: usize> Default for Means<N> {
    fn default() -> Self {
        Self([0.0; N])
    }
}

impl<const N: usize> Means<N> {
    pub(crate) fn insert<K: ToPrimitive>(&mut self, k: &K, p: f64) -> bool {
        let Some(first) = k.to_f64() else {return false};
        let mut v = 1.0;
        for i in 0..N {
            v *= first;
            self.0[i] += p * v;
        }
        true
    }
}
