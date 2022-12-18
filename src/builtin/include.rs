pub const INCLUDE: &str = r#"
fn abs(f: float)->float{
    if(f < 0.0, -f, f)
}

fn abs(i: int)->int{
    if(i < 0, -i, i)
}

fn bit_or<T>(a: Set<T>, b: Set<T>)->Set<T>{
    a.update(b.to_array())
}
"#;
