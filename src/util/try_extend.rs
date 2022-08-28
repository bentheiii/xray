pub fn try_extend<A, E, I: Iterator<Item = Result<A, E>>>(v: &mut Vec<A>, i: I) -> Result<(), E> {
    let (min, max) = i.size_hint();
    v.reserve_exact(max.unwrap_or(min));
    for item in i {
        v.push(item?);
    }
    Ok(())
}
