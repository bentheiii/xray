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

fn any<T>(a: Sequence<T>, f: (T)->(bool))->bool{
    a.nth(0, f).has_value()
}

fn all<T>(a: Sequence<T>, f: (T)->(bool))->bool{
    !a.nth(0, (t: T) -> {!f(t)}).has_value()
}

fn first<T>(a: Sequence<T>, f: (T)->(bool))->Optional<T>{
     a.nth(0, f)
}

fn last<T>(a: Sequence<T>, f: (T)->(bool))->Optional<T>{
     a.nth(-1, f)
}
"#;
