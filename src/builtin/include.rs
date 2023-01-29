pub const INCLUDE: &str = r#"
// int
fn abs(i: int)->int{
    if(i < 0, -i, i)
}

fn count(start: int, offset: int ?= 1)->Sequence<int>{
    count().map((x:int)->{x*offset+start})
}

fn gcd(a: int, b: int)->int{
    fn helper(a: int, b: int)->int{
        if(a == 0, b, helper(b % a, a))
    }
    let a = abs(a);
    let b = abs(b);
    if(a<b, helper(a,b), helper(b,a))
}

fn lcm(a: int, b: int)->int{
    let g = gcd(a,b);
    trunc(a.abs()/g)*b.abs()
}

// float
let pi = 3.1415926535897932384626433832795028841971693;
let e = 2.7182818284590452353602874713526624977572;

fn abs(f: float)->float{
    if(f < 0.0, -f, f)
}

fn div(f: float, i: int)->float{
    f/i.to_float()
}

// bool
fn bit_xor(a: bool, b: bool)->bool{
    a != b
}

// string
fn mul(a: str, n: int)->str{
    fn helper(n: int, ret: str)->str{
        if(n==0, ret, helper(n-1, ret+a))
    }
    helper(n, "")
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


fn join(s: Sequence<str>, delimiter: str ?= "")->str{
    s.to_generator().join(delimiter)
}

// cont distributions
fn chisq_distribution(d: int)->ContinuousDistribution{
    gamma_distribution(d/2, 0.5)
}

// dates
struct Date(year: int, month: int, day: int)

fn date(jd: int)->Date{
    let f = jd + 1401 + trunc((trunc((4*jd + 274277)/146097) * 3)/4) - 38;
    let e = 4*f+3;
    let g = trunc((e % 1461)/4);
    let h = 5*g+2;
    let days = trunc((h % 153) / 5) + 1;
    let months = (trunc(h/153) + 2)%12 + 1;
    let years = trunc(e/1461) - 4716 + trunc((14 - months)/12);
    Date(years, months, days)
}

fn julian_day(date: Date)->int{
    let year = date::year;
    let month = date::month;
    let day = date::day;

    let a = trunc((14 - month) / 12);
    let y = year + 4800 - a;
    let m = month + 12 * a - 3;
    day + trunc((153 * m + 2)/5) + y*365 + trunc(y/4) - trunc(y/100) + trunc(y/400) - 32045
}

fn eq(d0: Date, d1: Date)->bool{
    d0.julian_day() == d1.julian_day()
}

fn cmp(d0: Date, d1: Date)->int{
    cmp(d0.julian_day(), d1.julian_day())
}

//sequences
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

fn enumerate<T>(a: Sequence<T>, start: int ?= 0, offset: int ?= 1)->Sequence<(int, T)>{
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

fn filter<T>(s: Sequence<T>, f: (T)->(bool))->Generator<T>{
    s.to_generator().filter(f)
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

fn aggregate<T0, T1>(seq: Sequence<T0>, initial_state: T1, func: (T1, T0)->(T1))->Generator<T1>{
    seq.to_generator().aggregate(initial_state, func)
}

//generators
fn any<T>(a: Generator<T>, f: (T)->(bool))->bool{
    a.nth(0, f).has_value()
}

fn all<T>(a: Generator<T>, f: (T)->(bool))->bool{
    !a.nth(0, (t: T) -> {!f(t)}).has_value()
}

fn first<T>(a: Generator<T>, f: (T)->(bool))->Optional<T>{
     a.nth(0, f)
}

fn enumerate<T>(a: Generator<T>, start: int ?= 0, offset: int ?= 1)->Generator<(int, T)>{
    count(start, offset).zip(a)
}

fn successors<T>(start: T, f: (T)->(T))->Generator<T>{
    successors_until(start, (t: T) -> {some(f(t))})
}

fn count<T>(s: Generator<T>, f: (T)->(bool))->int{
    s.filter(f).len()
}

fn aggregate<T>(g: Generator<T>, f: (T, T)->(T))->Generator<T>{
    g.aggregate(none(),
        (prev: Optional<T>, next: T)->{if(prev.has_value(), some(f(prev.value(), next)), some(next))}
    ).skip(1).map(value{Optional<T>})
}

fn reduce<T0, T1>(g: Generator<T0>, initial_state: T1, func: (T1, T0)->(T1)) -> T1{
    g.aggregate(initial_state, func).last()
}

fn reduce<T>(g: Generator<T>, func: (T, T)->(T)) -> T{
    g.aggregate(func).last()
}

fn sum(g: Generator<int>)->int{
    g.reduce(add{int, int})
}

fn sum(g: Generator<float>)->float{
    g.reduce(add{float, float})
}

fn mean(g: Generator<int>)->float{
    let v = g.reduce((0, 0), (a: (int, int), v:int) -> {(a::item0+v, a::item1+1)});
    v::item0/v::item1
}

fn mean(g: Generator<float>)->float{
    let v = g.reduce((0.0, 0), (a: (float, int), v:float) -> {(a::item0+v, a::item1+1)});
    v::item0/v::item1.to_float()
}

// sets
fn __std_xset_order_by_cardinality<T>(a: Set<T>, b: Set<T>) -> (Set<T>, Set<T>){
    if(a.len() < b.len(), (a,b), (b,a))
}

fn bit_and<T>(a: Set<T>, b: Set<T>)->Set<T>{
    let ord = __std_xset_order_by_cardinality(a, b);
    a.clear().update(ord::item0.to_generator().filter((i: T) -> {ord::item1.contains(i)}))
}

fn bit_or<T>(a: Set<T>, b: Set<T>)->Set<T>{
    a.update(b.to_generator())
}

fn to_array<T>(s: Set<T>)->Sequence<T>{
    s.to_generator().to_array()
}

fn update<T>(s: Set<T>, a: Sequence<T>)->Set<T>{
    s.update(a.to_generator())
}

fn eq<T>(a: Set<T>, b: Set<T>)->bool{
    let ord = __std_xset_order_by_cardinality(a, b);
    a.len() == b.len() && ord::item0.to_generator().all((x: T)->{ord::item1.contains(x)})
}

fn lt<T>(a: Set<T>, b: Set<T>)->bool{
    a.len() < b.len() && a.to_generator().all((x: T)->{b.contains(x)})
}

fn le<T>(a: Set<T>, b: Set<T>)->bool{
    a.len() <= b.len() && a.to_generator().all((x: T)->{b.contains(x)})
}

fn gt<T>(a: Set<T>, b: Set<T>)->bool{
    b.lt(a)
}

fn ge<T>(a: Set<T>, b: Set<T>)->bool{
    b.le(a)
}

// mappings
fn update<K,V>(m: Mapping<K,V>, s: Sequence<(K,V)>)->Mapping<K,V>{
    m.update(s.to_generator())
}

fn get<K,V>(m: Mapping<K,V>, k: K)->V{
    m.lookup(k).value("key not found")
}

fn contains<K,V>(m: Mapping<K,V>, k: K)->bool{
    m.lookup(k).has_value()
}

fn keys<K, V>(m: Mapping<K,V>)->Generator<K>{
    m.to_generator().map((t: (K,V)) -> {t::item0})
}

fn values<K, V>(m: Mapping<K,V>)->Generator<V>{
    m.to_generator().map((t: (K,V)) -> {t::item1})
}


// gen 2:
// floats
fn covariance(s: Generator<(float, float)>)->float{
    let agg = (n: (int,float,float), i: (float, float))->{(n::item0+1, n::item1+i::item0, n::item2+i::item1)};
    let agg = s.reduce((0,0.0,0.0), agg);
    let mean_x = agg::item1/agg::item0;
    let mean_y = agg::item2/agg::item0;
    s.map((t:(float, float))->{(t::item0-mean_x)*(t::item1-mean_y)}).mean()
}

fn factorial(n: int, step: int ?= 1)->int{
    if(n < 0, error("cannot get factorial of negative number"),
        range(n,0,-step).to_generator().reduce(1, mul{int, int}))
}

// sequences

fn aggregate<T>(seq: Sequence<T>,  f: (T, T)->(T))->Generator<T>{
    seq.to_generator().aggregate(f)
}

fn reduce<T0, T1>(g: Sequence<T0>, initial_state: T1, func: (T1, T0)->(T1)) -> T1{
    g.to_generator().reduce(initial_state, func)
}

fn reduce<T>(g: Sequence<T>, func: (T, T)->(T)) -> T{
    g.to_generator().reduce(func)
}

fn sum(g: Sequence<int>)->int{
    g.reduce(add{int, int})
}

fn sum(g: Sequence<float>)->float{
    g.reduce(add{float, float})
}

fn mean(g: Sequence<int>)->float{
    g.sum()/g.len()
}

fn mean(g: Sequence<float>)->float{
    g.sum()/g.len().to_float()
}
"#;
