#[macro_export]
macro_rules! forward_err {
    ($e: expr) => {{
        match $e{
            Ok(__i)=> __i,
            Err(__e) =>return Ok(Err(__e)),
        }
    }};
}