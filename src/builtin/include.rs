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

fn any<T>(a: Generator<T>, f: (T)->(bool))->bool{
    a.nth(0, f).has_value()
}

fn all<T>(a: Generator<T>, f: (T)->(bool))->bool{
    !a.nth(0, (t: T) -> {!f(t)}).has_value()
}

fn first<T>(a: Sequence<T>, f: (T)->(bool))->Optional<T>{
     a.nth(0, f)
}

fn first<T>(a: Generator<T>, f: (T)->(bool))->Optional<T>{
     a.nth(0, f)
}

fn last<T>(a: Sequence<T>, f: (T)->(bool))->Optional<T>{
     a.nth(-1, f)
}

fn count(start: int, offset: int ?= 1)->Sequence<int>{
    count().map((x:int)->{x*offset+start})
}

fn enumerate<T>(a: Sequence<T>, start: int ?= 0, offset: int ?= 1)->Sequence<(int, T)>{
    count(start, offset).zip(a)
}

fn enumerate<T>(a: Generator<T>, start: int ?= 0, offset: int ?= 1)->Generator<(int, T)>{
    count(start, offset).zip(a)
}

fn repeat<T>(a: Sequence<T>)->Sequence<T>{
    let length = a.len();
    if(is_error(length),
        a,
        count().map((idx: int)->{a[idx%length]})
    )
}

fn repeat<T>(a: Sequence<T>, n: int)->Sequence<T>{
    let length = a.len();
    if(is_error(length),
        a,
        count().map((idx: int)->{a[idx%length]}).take(n*length)
    )
}

fn mul<T>(a: Sequence<T>, b: int)->Sequence<T>{
    a.repeat(b)
}

fn mul(a: str, n: int)->str{
    fn helper(n: int, ret: str)->str{
        if(n==0, ret, helper(n-1, ret+a))
    }
    helper(n, "")
}

fn successors<T>(start: T, f: (T)->(T))->Generator<T>{
    successors_until(start, (t: T) -> {some(f(t))})
}

fn filter<T>(s: Sequence<T>, f: (T)->(bool))->Generator<T>{
    s.to_generator().filter(f)
}

fn count<T>(s: Generator<T>, f: (T)->(bool))->int{
    s.filter(f).len()
}

fn substring<T>(s: str, start: int, end: Optional<int> ?= none())->str{
    s.substring(start, end || s.len())
}

fn split(s: str, n: str)->Generator<str>{
    fn next_bound(prev_bounds: (int, Optional<int>))->Optional<(int, Optional<int>)>{
        fn from_prev_end(prev_end: int)->(int, Optional<int>){
            let next_start = prev_end+n.len();
            (next_start, debug(s.find(n,next_start), "hi "))
        }
        prev_bounds::item1.map(from_prev_end)
    }
    fn bounds_to_string(bounds: (int, Optional<int>))->str{
        s.substring(bounds::item0, bounds::item1)
    }
    let first_match = s.find(n);
    successors_until((0, first_match), next_bound)
    .map(bounds_to_string)
}

fn binary_search<T>(seq: Sequence<T>, cmp_: (T)->(int))->Optional<int>{
    fn helper(seq: Sequence<T>, offset: int)->Optional<int>{
        let mid = floor(seq.len()/2);
        let res = if(mid==seq.len(), -1, cmp_(seq[mid]));
        if(res == 0, some(mid+offset),
            if(res > 0, helper(seq.take(mid), offset),
                if(mid==seq.len(), none(),
                    helper(seq.skip(mid+1), offset+mid+1)
                )
            )
        )
    }
    helper(seq, 0)
}

fn bisect<T>(seq: Sequence<T>, left_predicate: (T)->(bool))->int{
    fn helper(seq: Sequence<T>, offset: int)->int{
        let mid = floor(seq.len()/2);
        let res = if(mid==seq.len(), true, left_predicate(seq[mid]));
        if(res,
            if(mid==seq.len(), offset,
                helper(seq.skip(mid+1), offset+mid+1)
            ),
            helper(seq.take(mid), offset),
        )
    }
    helper(seq, 0)
}
"#;
