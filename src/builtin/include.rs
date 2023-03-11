pub const INCLUDE: &str = r#"
// generic
fn min<T>(a: T, b: T, lt: (T,T)->(bool))->T{
    if(lt(a,b), a, b)
}

fn max<T>(a: T, b: T, lt: (T,T)->(bool))->T{
    if(lt(a,b), b, a)
}

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

fn harmonic_mean(a: int, b:int)->float{
    2.0/(1/a + 1/b)
}

fn sign(a: int)->int{
    if(a>0, 1, if(a<0, -1, 0))
}

// float
let pi = 3.1415926535897932384626433832795028841971693;
let e = 2.7182818284590452353602874713526624977572;

fn abs(f: float)->float{
    if(f < 0.0, -f, f)
}

fn add(f: float, i: int)->float{
    f+i.to_float()
}

fn add(a: int, b: float)->float{
    a.to_float()+b
}

fn mul(f: float, i: int)->float{
    f*i.to_float()
}

fn mul(a: int, b: float)->float{
    a.to_float()*b
}

fn div(f: float, i: int)->float{
    f/i.to_float()
}

fn div(a: int, b: float)->float{
    a.to_float()/b
}

fn pow(a: int, b: float)->float{
    a.to_float()**b
}

fn pow(b: float, a: int)->float{
    b**a.to_float()
}

fn harmonic_mean(a: float, b:float)->float{
    2.0/(1.0/a + 1.0/b)
}

fn log(a: float, base: float)->float{
    a.ln() / base.ln()
}

fn sign(a: float)->float{
    if(a>0.0, 1.0, if(a<0.0, -1.0, 0.0))
}

// complex
struct Complex(r: float, i: float)

fn eq(a: Complex, b: Complex)->bool{
    a::r == b::r && a::i == b::i
}

fn complex(r: float)->Complex{
    Complex(r, 0.0)
}

fn complex(r: int)->Complex{
    complex(r.to_float())
}

fn complex_from_polar(r: float, a: float)->Complex{
    Complex(a.cos() * r, a.sin() * r)
}

fn neg(a: Complex)->Complex{
    Complex(-a::r, -a::i)
}

fn abs(a: Complex) -> float{
    sqrt(a::r**2 + a::i**2)
}

fn arg(a: Complex) -> float{
    atan(a::i, a::r)
}

fn add(a: Complex,b: Complex)->Complex{
    Complex(a::r+b::r, a::i+b::i)
}

fn add(a: Complex,b: float)->Complex{
    a + complex(b)
}

fn add(b: float, a: Complex)->Complex{
    a + complex(b)
}

fn add(a: Complex,b: int)->Complex{
    a + complex(b)
}

fn add(b: int, a: Complex)->Complex{
    a + complex(b)
}

fn sub(a: Complex,b: Complex)->Complex{
    a + (-b)
}

fn sub(a: Complex,b: float)->Complex{
    a - complex(b)
}

fn sub(b: float, a: Complex)->Complex{
    complex(b) - a
}

fn sub(a: Complex,b: int)->Complex{
    a - complex(b)
}

fn sub(b: int, a: Complex)->Complex{
    complex(b) - a
}

fn mul(a: Complex, b: Complex)->Complex{
    Complex(a::r*b::r-a::i*b::i, a::r*b::i+a::i*b::r)
}

fn mul(a: Complex,b: float)->Complex{
    a * complex(b)
}

fn mul(b: float, a: Complex)->Complex{
    a * complex(b)
}

fn mul(a: Complex,b: int)->Complex{
    a * complex(b)
}

fn mul(b: int, a: Complex)->Complex{
    a * complex(b)
}

fn div(a: Complex, b: Complex)->Complex{
    let denom = b::r**2 + b::i**2;
    Complex((a::r*b::r+a::i*b::i)/denom, (a::i*b::r-a::r*b::i)/denom)
}

fn div(a: float, b: Complex)->Complex{
    complex(a) / b
}

fn div(a: int, b: Complex)->Complex{
    complex(a) / b
}

fn div(a: Complex, b: float)->Complex{
    a / complex(b)
}

fn div(a: Complex, b: int)->Complex{
    a / complex(b)
}

fn pow(a: Complex, b: float)->Complex{
    complex_from_polar(a.abs() ** b, a.arg() * b)
}

fn pow(a: Complex, b: int)->Complex{
    a**b.to_float()
}

fn conjugate(a: Complex) -> Complex{
    Complex(a::r, -a::i)
}

fn is_close(a: Complex, b: Complex, rel_tol: float ?= 1e-9, abs_tol: float ?= 1e-9)->bool{
    let diff = a-b;
    diff.abs().is_close(0.0, rel_tol, abs_tol)
}

// bool
fn bit_xor(a: bool, b: bool)->bool{
    a != b
}

fn indicator(a: bool)->int{
    a.if(1,0)
}

// string
fn mul(a: str, n: int)->str{
    repeat([a].to_generator()).take(n).join()
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

// disc distributions
fn geometric_distribution(p: float)->DiscreteDistribution{
    negative_binomial_distribution(1.0, p)
}

fn random(d: DiscreteDistribution)->int{
    d.sample(1)[0]
}

// cont distributions
fn chisq_distribution(d: int)->ContinuousDistribution{
    gamma_distribution(d/2, 0.5)
}

fn standard_distribution()->ContinuousDistribution{
    rectangular_distribution(0.0, 1.0)
}

fn random(d: ContinuousDistribution)->float{
    d.sample(1)[0]
}

fn random()->float{
    standard_distribution().random()
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
    eq(d0.members(), d1.members())
}

fn cmp(d0: Date, d1: Date)->int{
    cmp(d0.members(), d1.members())
}

// datetime
struct Datetime(date: Date, hours: int, minutes: int, seconds: float)
let __std_unix_epoch = Date(1970,1,1);

fn unix(dt: Datetime)->float{
    ((dt::date.julian_day()-__std_unix_epoch.julian_day())*86400 + dt::hours*60*60 + dt::minutes*60) + dt::seconds
}

fn datetime(unix: float)->Datetime{
    let seconds = unix % 60.0;
    let u = (unix/60.0).floor();
    let minutes = u % 60;
    let u = (u/60).floor();
    let hours = u %24;
    let u = (u/24).floor();
    let days = date(u + __std_unix_epoch.julian_day());
    Datetime(days, hours, minutes, seconds)
}

fn now()->Datetime{
    datetime(__std_unix_now())
}

fn eq(d0: Datetime, d1: Datetime)->bool{
    eq(d0.members(), d1.members())
}

fn cmp(d0: Datetime, d1: Datetime)->int{
    cmp(d0.members(), d1.members())
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

fn median<T>(a: Sequence<T>, f: (T, T)->(int))->T{
    a.nth_smallest((a.len()/2).floor(), f)
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

fn rank_sorted_eq<T0, T1>(seq: Sequence<T0>, t: T1, cmp: (T0, T1)->(int))->int{
    let idx = seq.bisect((x: T0) -> {cmp(x, t)<0});
    if(cmp(seq[idx], t) == 0, idx+1, error("item not in sequence"))
}

fn rank_sorted_avg<T0, T1>(seq: Sequence<T0>, t: T1, cmp: (T0, T1)->(int))->float{
    let bottom = seq.bisect((x: T0) -> {cmp(x, t)<0});
    let top = seq.bisect((x: T0) -> {cmp(x, t)<=0});
    if(cmp(seq[bottom], t) == 0, (bottom+top+1)/2, error("item not in sequence"))
}

fn rank_eq<T0>(seq: Sequence<T0>, t: T0, cmp: (T0, T0)->(int))->int{
    seq.sort(cmp).rank_sorted_eq(t, cmp)
}

fn rank_avg<T0>(seq: Sequence<T0>, t: T0, cmp: (T0, T0)->(int))->float{
    seq.sort(cmp).rank_sorted_avg(t, cmp)
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

fn min<T>(g: Generator<T>, lt: (T,T)->(bool)) -> T {
    g.reduce((a: T, b: T)->{min(a,b,lt)})
}

fn max<T>(g: Generator<T>, lt: (T,T)->(bool)) -> T {
    g.reduce((a: T, b: T)->{max(a,b,lt)})
}

fn sum(g: Generator<int>)->int{
    g.sum(0)
}

fn sum(g: Generator<float>)->float{
    g.sum(0.0)
}

fn product(g: Generator<int>)->int{
    g.product(1)
}

fn product(g: Generator<float>)->float{
    g.product(1.0)
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

fn update_from_keys<K,V>(m: Mapping<K,V>, s: Sequence<K>, on_empty: (K)->(V), on_occupied: (K,V)->(V))->Mapping<K,V>{
    m.update_from_keys(s.to_generator(), on_empty, on_occupied)
}

fn update_counter<K>(m: Mapping<K, int>, g: Generator<K>)->Mapping<K, int>{
    m.update_from_keys(g, (_: K)->{1}, (_: K, v: int)->{v+1})
}

fn counter_mode<K>(m: Mapping<K, int>)->(Sequence<K>, int){
    let agg = m.to_generator().reduce((stack(), 0), (agg: (Stack<K>, int), next: (K, int))->{
        if(
        next::item1 > agg::item1,
        (stack().push(next::item0), next::item1),
        if(next::item1 == agg::item1, (agg::item0.push(next::item0), next::item1), agg)
        )
    });
    (agg::item0.to_array(), agg::item1)
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

fn min<T>(g: Sequence<T>, lt: (T,T)->(bool)) -> T {
    g.reduce((a: T, b: T)->{min(a,b,lt)})
}

fn max<T>(g: Sequence<T>, lt: (T,T)->(bool)) -> T {
    g.reduce((a: T, b: T)->{max(a,b,lt)})
}

fn sum(g: Sequence<int>)->int{
    g.sum(0)
}

fn sum(g: Sequence<float>)->float{
    g.sum(0.0)
}

fn product(g: Sequence<int>)->int{
    g.product(1)
}

fn product(g: Sequence<float>)->float{
    g.product(1.0)
}

fn pearson_correlation(s: Sequence<(float, float)>)->float{
    struct Agg(sum0: float, sum1: float, sq0: float, sq1: float, prod: float)

    let agg = s.reduce(Agg(0.0,0.0,0.0,0.0,0.0), (a: Agg, item: (float, float))->{
        Agg(
            a::sum0+item::item0,
            a::sum1+item::item1,
            a::sq0+item::item0*item::item0,
            a::sq1+item::item1*item::item1,
            a::prod + item::item0*item::item1,
        )
    });
    (s.len() * agg::prod - agg::sum0 * agg::sum1)
    / (sqrt(s.len()*agg::sq0-agg::sum0**2) * sqrt(s.len()*agg::sq1-agg::sum1**2))
}
"#;
