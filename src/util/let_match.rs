#[macro_export]
macro_rules! let_match {
    ($e: expr; $pat: pat => $out: expr) => {
        if let $pat = $e{
            $out
        } else {
            unreachable!()
        }
    };
}