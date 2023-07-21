pub const INCLUDE: &str = r#"
//// gen 1

/// meta 1
let xray_std_version = (0,1,0);

/// generic 1
fn max<T>(a: T, b: T, lt: (T,T)->(bool))->T{
    if(lt(a,b), b, a)
}

fn min<T>(a: T, b: T, lt: (T,T)->(bool))->T{
    if(lt(b,a), b, a)
}

fn to_cmp<T, U>(key: (T)->(U), cmp_: (U,U)->(int))->(T,T)->(int){
    (a: T, b: T)->{cmp_(key(a), key(b))}
}

fn to_eq<T, U>(key: (T)->(U), eq_: (U,U)->(bool))->(T,T)->(bool){
    (a: T, b: T)->{eq_(key(a), key(b))}
}

fn to_lt<T, U>(key: (T)->(U), lt_: (U,U)->(bool))->(T,T)->(bool){
    (a: T, b: T)->{lt_(key(a), key(b))}
}


/// bool 1
fn bit_xor(a: bool, b: bool)->bool{
    a != b
}

fn indicator(a: bool)->int{
    a.if(1,0)
}

/// float 1
let e = 2.7182818284590452353602874713526624977572;
let pi = 3.1415926535897932384626433832795028841971693;

fn abs(f: float)->float{
    if(f < 0.0, -f, f)
}

fn add(f: float, i: int)->float{
    f+i.to_float()
}

fn add(a: int, b: float)->float{
    a.to_float()+b
}

fn div(f: float, i: int)->float{
    f/i.to_float()
}

fn div(a: int, b: float)->float{
    a.to_float()/b
}

fn harmonic_mean(a: float, b:float)->float{
    2.0/(1.0/a + 1.0/b)
}

fn log(a: float, base: float)->float{
    a.ln() / base.ln()
}

fn mul(f: float, i: int)->float{
    f*i.to_float()
}

fn mul(a: int, b: float)->float{
    a.to_float()*b
}

fn pow(a: int, b: float)->float{
    a.to_float()**b
}

fn pow(b: float, a: int)->float{
    b**a.to_float()
}

fn sign(a: float)->float{
    if(a>0.0, 1.0, if(a<0.0, -1.0, 0.0))
}

fn sub(f: float, i: int)->float{
    f-i.to_float()
}

fn sub(a: int, b: float)->float{
    a.to_float()-b
}

/// int 1
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

fn harmonic_mean(a: int, b:int)->float{
    2.0/(1/a + 1/b)
}

fn lcm(a: int, b: int)->int{
    let g = gcd(a,b);
    if(g == 0, 0, trunc(a.abs()/g)*b.abs())
}

fn permutation(n: int, i: int)->Sequence<int>{
    permutation(n,i,n)
}

fn sign(a: int)->int{
    if(a>0, 1, if(a<0, -1, 0))
}

/// str 1
fn chars(a: str)->Sequence<str>{
    range(a.len()).map((i: int)->{a[i]})
}

fn contains(a: str, b: str, start_ind: int ?= 0)->bool{
    a.find(b, start_ind).has_value()
}

fn join(s: Sequence<str>, delimiter: str ?= "")->str{
    s.to_generator().join(delimiter)
}

fn substring<T>(s: str, start: int, end: Optional<int> ?= none())->str{
    s.substring(start, end || s.len())
}

/// Generator 1
fn aggregate<T>(g: Generator<T>, f: (T, T)->(T))->Generator<T>{
    g.aggregate(none(),
        (prev: Optional<T>, next: T)->{if(prev.has_value(), some(f(prev.value(), next)), some(next))}
    ).skip(1).map(value{Optional<T>})
}

fn all<T>(a: Generator<T>, f: (T)->(bool))->bool{
    !a.nth(0, (t: T) -> {!f(t)}).has_value()
}

fn any<T>(a: Generator<T>, f: (T)->(bool))->bool{
    a.nth(0, f).has_value()
}

fn chunks<T>(g: Generator<T>, n: int)->Generator<Sequence<T>>{
    struct Agg<T>(s: Stack<T>, disp: Optional<bool>)

    g.map(some).add([none()].to_generator()).aggregate(Agg(stack(), some(false)),
        (agg: Agg<T>, next: Optional<T>)->{
            next.map_or(
                (next: T)->{
                    let next_tail = if(
                        agg::s.len() < n,
                        agg::s,
                        stack()
                    );
                    Agg(next_tail.push(next), none())
                },
                Agg(agg::s, some((agg::s.len() > 0) && (agg::s.len() < n)))
            )
            
        }
    ).filter((agg: Agg<T>)->{agg::disp || (agg::s.len() == n)}).map((agg: Agg<T>)->{agg::s.to_array()})
}

fn contains<T, U>(s: Generator<T>, i: U, eq_: (T,U)->(bool))->bool{
    s.any((t: U)->{eq_(i, t)})
}

fn count<T>(s: Generator<T>, f: (T)->(bool))->int{
    s.filter(f).len()
}

fn count<T, U>(s: Generator<T>, i: U, eq_: (T,U)->(bool))->int{
    s.filter((t: U)->{eq_(i, t)}).len()
}

fn distinct<T>(g: Generator<T>, h: (T)->(int), e: (T,T)->(bool))->Generator<T>{
    g.with_count(h,e).filter((i: (T, int))->{i::item1==1}).map((i: (T, int))->{i::item0})
}

fn enumerate<T>(a: Generator<T>, start: int ?= 0, offset: int ?= 1)->Generator<(int, T)>{
    count(start, offset).zip(a)
}

fn first<T>(a: Generator<T>, f: (T)->(bool))->Optional<T>{
     a.nth(0, f)
}

fn reduce<T0, T1>(g: Generator<T0>, initial_state: T1, func: (T1, T0)->(T1)) -> T1{
    g.aggregate(initial_state, func).last()
}

fn reduce<T>(g: Generator<T>, func: (T, T)->(T)) -> T{
    g.aggregate(func).last()
}

fn successors<T>(start: T, f: (T)->(T))->Generator<T>{
    successors_until(start, (t: T) -> {some(f(t))})
}

fn sum(g: Generator<int>)->int{
    g.sum(0)
}

fn sum(g: Generator<float>, initial: float)->float{
    struct Kahan(
        sum: float,
        c: float
    )
    let k = g.reduce(Kahan(initial,0.0), (k: Kahan, x: float)->{
        let t = k::sum + x;
        let c = k::c + if(abs(k::sum) >= abs(x), (k::sum - t) + x, (x - t) + k::sum);
        Kahan(t, c)
    });
    k::sum + k::c
}

fn sum(g: Generator<float>)->float{
    g.sum(0.0)
}


/// Mapping 1
fn contains<K,V>(m: Mapping<K,V>, k: K)->bool{
    m.lookup(k).has_value()
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

fn keys<K, V>(m: Mapping<K,V>)->Generator<K>{
    m.to_generator().map((t: (K,V)) -> {t::item0})
}

fn map_values<K, V0, V1>(m: Mapping<K,V0>, f: (V0)->(V1))->Mapping<K,V1>{
    m.clear().update_from_keys(m.keys(), (k: K)->{f(m[k])}, (k: K, v: V1)->{error("unreachable")})
}

fn update<K,V>(m: Mapping<K,V>, s: Sequence<(K,V)>)->Mapping<K,V>{
    m.update(s.to_generator())
}

fn update<K,V>(m: Mapping<K,V>, s: Mapping<K,V>)->Mapping<K,V>{
    m.update(s.to_generator())
}

fn update_from_keys<K,V>(m: Mapping<K,V>, s: Sequence<K>, on_empty: (K)->(V), on_occupied: (K,V)->(V))->Mapping<K,V>{
    m.update_from_keys(s.to_generator(), on_empty, on_occupied)
}

fn values<K, V>(m: Mapping<K,V>)->Generator<V>{
    m.to_generator().map((t: (K,V)) -> {t::item1})
}

/// Regex 1
struct Match(
    _haystak: str,
    _named_groups: Mapping<str, int>,
    _groups: Sequence<Optional<(int,int)>>,
)

fn _named_groups(r: Regex)->Mapping<str, int>{
    let inner = __std_group_names(r);
    mapping<str>().update(inner.to_generator().enumerate().filter((t: (int, Optional<str>))->{t::item1.has_value()}).map((t: (int, Optional<str>))->{(t::item1.value(), t::item0)}))
}

fn get(m: Match, i: int)->Optional<str>{
    m::_groups[i].map((g: (int,int))->{
        m::_haystak.substring(g::item0,g::item1)
    })
}

fn get(m: Match, n: str)->Optional<str>{
    m[m::_named_groups[n]]
}

fn search(r: Regex, s: str, i: int ?= 0, j: Optional<int> ?= none())->Optional<Match>{
    let groups = r.__std_match(s, i, j || s.len());
    groups.map((g: Sequence<Optional<(int,int)>>)->{
        Match(s, r._named_groups(), g)
    })
}

fn search(r: Regex, s: str, i: int , j: int)->Optional<Match>{
    r.search(s, i, some(j))
}


/// Set 1
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

fn eq<T>(a: Set<T>, b: Set<T>)->bool{
    let ord = __std_xset_order_by_cardinality(a, b);
    a.len() == b.len() && ord::item0.to_generator().all((x: T)->{ord::item1.contains(x)})
}

fn ge<T>(b: Set<T>, a: Set<T>)->bool{
    a.len() <= b.len() && a.to_generator().all((x: T)->{b.contains(x)})
}

fn gt<T>(b: Set<T>, a: Set<T>)->bool{
    a.len() < b.len() && a.to_generator().all((x: T)->{b.contains(x)})
}

fn is_disjoint<T>(a: Set<T>, b: Set<T>)->bool{
    let ord = __std_xset_order_by_cardinality(a, b);
    ord::item0.to_generator().all((x:T)->{!ord::item1.contains(x)})
}

fn le<T>(a: Set<T>, b: Set<T>)->bool{
    b.ge(a)
}

fn lt<T>(a: Set<T>, b: Set<T>)->bool{
    b.gt(a)
}

fn sub<T>(a: Set<T>, b: Set<T>)->Set<T>{
    a.clear().update(a.to_generator().filter((i: T) -> {!b.contains(i)}))
}

fn to_array<T>(s: Set<T>)->Sequence<T>{
    s.to_generator().to_array()
}

fn update<T>(s: Set<T>, a: Sequence<T>)->Set<T>{
    s.update(a.to_generator())
}

/// Sequence 1
fn aggregate<T0, T1>(seq: Sequence<T0>, initial_state: T1, func: (T1, T0)->(T1))->Generator<T1>{
    seq.to_generator().aggregate(initial_state, func)
}

fn aggregate<T>(seq: Sequence<T>,  f: (T, T)->(T))->Generator<T>{
    seq.to_generator().aggregate(f)
}

fn all(a: Sequence<bool>)->bool{
    !a.nth(0, (t: bool) -> {!t}).has_value()
}

fn all<T>(a: Sequence<T>, f: (T)->(bool))->bool{
    !a.nth(0, (t: T) -> {!f(t)}).has_value()
}

fn any(a: Sequence<bool>)->bool{
    a.nth(0, (t: bool) -> {t}).has_value()
}

fn any<T>(a: Sequence<T>, f: (T)->(bool))->bool{
    a.nth(0, f).has_value()
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
    // returns the number of elements from the beginning of the sequence that satisfy the predicate
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

fn combination<T>(a: Sequence<T>, i: int, k: int)->Sequence<T>{
    let i_perm = a.len().combination(i, k);
    i_perm.map((idx: int)->{a[idx]})
}

fn combination_with_replacement<T>(a: Sequence<T>, i: int, k: int)->Sequence<T>{
    let i_perm = a.len().combination_with_replacement(i, k);
    i_perm.map((idx: int)->{a[idx]})
}

fn combinations<T>(a: Sequence<T>, k: int)->Sequence<Sequence<T>>{
    let len = binom(a.len(), k);
    range(len).map((idx: int)->{a.combination(idx, k)})
}

fn combinations_with_replacement<T>(a: Sequence<T>, k: int)->Sequence<Sequence<T>>{
    let len = binom(a.len()+k-1, k);
    range(len).map((idx: int)->{a.combination_with_replacement(idx, k)})
}

fn contains<T, U>(s: Sequence<T>, i: U, eq_: (T,U)->(bool))->bool{
    s.to_generator().contains(i, eq_)
}

fn enumerate<T>(a: Sequence<T>, start: int ?= 0, offset: int ?= 1)->Sequence<(int, T)>{
    count(start, offset).zip(a)
}

fn filter<T>(s: Sequence<T>, f: (T)->(bool))->Generator<T>{
    s.to_generator().filter(f)
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

fn permutation<T>(a: Sequence<T>, i: int)->Sequence<T>{
    let i_perm = a.len().permutation(i);
    i_perm.map((idx: int)->{a[idx]})
}

fn permutation<T>(a: Sequence<T>, i: int, k: int)->Sequence<T>{
    let i_perm = a.len().permutation(i, k);
    i_perm.map((idx: int)->{a[idx]})
}

fn random_choices<T>(a: Sequence<T>, n: int)->Sequence<T>{
    let dist = uniform_distribution(0, a.len()-1);
    dist.sample(n).map((idx: int)->{a[idx]})
}

fn random_choices<T>(a: Sequence<T>, n: int, weights: Sequence<float>)->Sequence<T>{
    fn main()->Sequence<T>{
        let dist = custom_distribution(weights.enumerate());
        dist.sample(n).map((idx: int)->{a[idx]})
    }
    if(weights.len() != a.len(),
        error("random_choices: weights and a must have the same length"),
        main()
    )
}

fn rank_sorted_avg<T0, T1>(seq: Sequence<T0>, t: T1, cmp: (T0, T1)->(int))->float{
    let bottom = seq.bisect((x: T0) -> {cmp(x, t)<0});
    let top = seq.bisect((x: T0) -> {cmp(x, t)<=0});
    if(cmp(seq[bottom], t) == 0, (bottom+top+1)/2, error("item not in sequence"))
}

fn rank_sorted_eq<T0, T1>(seq: Sequence<T0>, t: T1, cmp: (T0, T1)->(int))->int{
    let idx = seq.bisect((x: T0) -> {cmp(x, t)<0});
    if(cmp(seq[idx], t) == 0, idx+1, error("item not in sequence"))
}

fn reduce<T0, T1>(g: Sequence<T0>, initial_state: T1, func: (T1, T0)->(T1)) -> T1{
    g.to_generator().reduce(initial_state, func)
}

fn reduce<T>(g: Sequence<T>, func: (T, T)->(T)) -> T{
    g.to_generator().reduce(func)
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

fn reverse<T>(a: Sequence<T>)->Sequence<T>{
    let offset = a.len();
    range(offset).map((idx: int)->{a[offset-1-idx]})
}

fn sample<T>(a: Sequence<T>, k: int, counts: Sequence<int>)->Sequence<T>{
    fn main1(cum_counts: Sequence<int>)->Sequence<T>{
        let selections = range(cum_counts[-1]).sample(k);

        selections.map((idx: int)->{
            a[cum_counts.bisect((c: int)->{c < idx})]
        })
    }
    fn main2()->Sequence<T>{
        let cum_counts = counts.aggregate(0, add{int, int}).skip(1).to_array();
        if(cum_counts[-1] <= 0, error("sample: counts must be non-negative"),
            main1(cum_counts)
        )
    }
    if(counts.len() != a.len(),
        error("sample: counts and a must have the same length"),
        main2()
    )
}

fn shuffle<T>(a: Sequence<T>)->Sequence<T>{
    a.sample(a.len())
}

fn sort_reverse<T>(a: Sequence<T>, cmp_: (T, T)->(int))->Sequence<T>{
    a.sort((a: T, b: T)->{-cmp_(a,b)})
}

fn sum(g: Sequence<int>)->int{
    g.sum(0)
}

fn sum(s: Sequence<float>, initial: float)->float{
    s.to_generator().sum(initial)
}

fn sum(g: Sequence<float>)->float{
    g.sum(0.0)
}

/// Complex 1
struct Complex(r: float, i: float)

fn complex(r: float)->Complex{
    Complex(r, 0.0)
}

fn complex(r: int)->Complex{
    complex(r.to_float())
}

fn abs(a: Complex) -> float{
    sqrt(a::r**2 + a::i**2)
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

fn arg(a: Complex) -> float{
    atan(a::i, a::r)
}

fn complex_from_polar(r: float, a: float)->Complex{
    Complex(a.cos() * r, a.sin() * r)
}

fn conjugate(a: Complex) -> Complex{
    Complex(a::r, -a::i)
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

fn eq(a: Complex, b: Complex)->bool{
    a::r == b::r && a::i == b::i
}

fn ln(a: Complex)->Complex{
    if(a==Complex(0.0,0.0),error("undefined complex log"),ln(a.abs()) + Complex(0.0, a.arg()))
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

fn neg(a: Complex)->Complex{
    Complex(-a::r, -a::i)
}

fn pow(a: Complex, b: float)->Complex{
    complex_from_polar(a.abs() ** b, a.arg() * b)
}

fn pow(a: Complex, b: int)->Complex{
    a**b.to_float()
}

fn pow(a: Complex, b: Complex)->Complex{
    fn pow(a: float, b: Complex)->Complex{
        // a must be positive
        complex_from_polar(a**b::r, b::i * a.ln())
    }
    (a::r**2 + a::i**2) ** (b/2) * e**(Complex(0.0, 1.0) * b * a.arg())
}

fn pow(a: float, b: Complex)->Complex{
    complex(a)**b
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

/// ContinuousDistribution 1
fn chisq_distribution(d: int)->ContinuousDistribution{
    gamma_distribution(d/2, 0.5)
}

fn random(d: ContinuousDistribution)->float{
    d.sample(1)[0]
}

fn standard_uniform_distribution()->ContinuousDistribution{
    rectangular_distribution(0.0, 1.0)
}

fn std_dev(d: ContinuousDistribution)->float{
    d.variance().sqrt()
}

fn z_score(d: ContinuousDistribution, x: float)->float{
    (x-d.mean())/d.std_dev()
}

/// Date 1
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

fn cmp(d0: Date, d1: Date)->int{
    cmp(d0.members(), d1.members())
}

fn eq(d0: Date, d1: Date)->bool{
    eq(d0.members(), d1.members())
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

fn to_str(d: Date)->str{
    f"Date{d.members()}"
}

fn weekday(d: Date)->int{
    // monday is 0, sunday is 6
    (d.julian_day())%7
}

/// Datetime 1
struct Datetime(date: Date, hours: int, minutes: int, seconds: float)
let __std_unix_epoch = Date(1970,1,1);

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

fn cmp(d0: Datetime, d1: Datetime)->int{
    cmp(d0.members(), d1.members())
}

fn eq(d0: Datetime, d1: Datetime)->bool{
    eq(d0.members(), d1.members())
}

fn format(self: Datetime, fmt: str)->str{
    fn repl(s: str)->str{
        if(s=="w", to_str((self::date.weekday() + 1)%7), 
        if(s=="a", ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"][self::date.weekday()-6],
        if(s=="A", ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"][self::date.weekday()-6],
        if(s=="b", ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"][self::date::month-1],
        if(s=="B", ["January", "Febuary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"][self::date::month-1],
        if(s=="d", self::date::day.format("02"),
        if(s=="m", self::date::month.format("02"),
        if(s=="y", (self::date::year%100).format("02"),
        if(s=="Y", self::date::year.format("04"),
        if(s=="H", self::hours.format("02"),
        if(s=="I", self::hours.sub(1).mod(12).add(1).format("02"),
        if(s=="P", self::hours.ge(12).if("PM", "AM"),
        if(s=="p", self::hours.ge(12).if("pm", "am"),
        if(s=="M", self::minutes.format("02"),
        if(s=="S", self::seconds.floor().format("02"),
        if(s=="f", self::seconds.mod(1.0).mul(1000000.0).trunc().format("06"),
        "%"
        ))))))))))))))))
    }
    fmt.format_replace(repl)
}

fn now()->Datetime{
    datetime(__std_unix_now())
}

fn to_str(d: Datetime)->str{
    f"Datetime{d.members()}"
}

fn unix(dt: Datetime)->float{
    ((dt::date.julian_day()-__std_unix_epoch.julian_day())*86400 + dt::hours*60*60 + dt::minutes*60) + dt::seconds
}

/// DiscreteDistribution 1
fn geometric_distribution(p: float)->DiscreteDistribution{
    negative_binomial_distribution(1.0, p)
}

fn random(d: DiscreteDistribution)->int{
    d.sample(1)[0]
}

fn sample_distribution(s: Sequence<int>)->DiscreteDistribution{
    custom_distribution(s.map((x: int)->{(x,1.0)}))
}

fn sample_variance(s: Sequence<int>)->float{
    sample_distribution(s).variance()*s.len()/(s.len()-1)
}

fn std_dev(d: DiscreteDistribution)->float{
    d.variance().sqrt()
}

fn z_score(d: DiscreteDistribution, x: float)->float{
    (x-d.mean())/d.std_dev()
}

/// Duration 1
struct Duration(seconds: float)

fn add(d0: Duration, d1: Duration)->Duration{
    Duration(d0::seconds + d1::seconds)
}

fn days(self: Duration)->float{
    self::seconds/86400.0
}

fn days(self: float)->Duration{
    Duration(self*86400.0)
}

fn div(self: Duration, m: float)->Duration{
    Duration(self::seconds / m)
}

fn div(self: Duration, m: int)->Duration{
    Duration(self::seconds / m)
}

fn div(d0: Duration, d1: Duration)->float{
    d0::seconds / d1::seconds
}

fn hours(self: Duration)->float{
    self::seconds/3600.0
}

fn hours(self: float)->Duration{
    Duration(self*3600.0)
}

fn minutes(self: Duration)->float{
    self::seconds/60.0
}

fn minutes(self: float)->Duration{
    Duration(self*60.0)
}

fn mul(self: Duration, m: float)->Duration{
    Duration(self::seconds * m)
}

fn mul(self: Duration, m: int)->Duration{
    Duration(self::seconds * m)
}

fn mul(m: float, self: Duration)->Duration{
    self * m
}

fn mul(m: int, self: Duration)->Duration{
    self * m
}

fn seconds(self: Duration)->float{
    self::seconds
}

fn seconds(self: float)->Duration{
    Duration(self)
}

fn sleep<T>(d: Duration)->(){
    __std_sleep(d::seconds, ())
}

fn sleep<T>(d: Duration, f: T)->T{
    __std_sleep(d::seconds, f)
}

fn years(self: Duration)->float{
    self.seconds()/31556926.0
}

fn years(self: float)->Duration{
    seconds(self*31556926.0)
}

/// Fraction 1
struct Fraction(n: int, d: int)

fn fraction(n: int)->Fraction{
    Fraction(n, 1)
}

fn fraction(n: int, d: int)->Fraction{
    let g = gcd(n, d);
    Fraction(trunc((n/g)/sign(d)), trunc(abs(d)/g))
}

fn fraction(f: float)->Fraction{
    let tpl = __std_tpl(f);
    fraction(tpl::item0, tpl::item1)
}

fn abs(a: Fraction)->Fraction{
    Fraction(abs(a::n), a::d)
}

fn add(a: Fraction, b: Fraction)->Fraction{
    fraction(a::n*b::d + b::n*a::d, a::d*b::d)
}

fn ceil(a: Fraction)->int{
    div_ceil(a::n,a::d)
}

fn cmp(a: Fraction, b: Fraction)->int{
    a::n*b::d-b::n*a::d
}

fn div(a: Fraction, b: Fraction)->Fraction{
    fraction(a::n*b::d, a::d*b::n)
}

fn eq(a: Fraction, b: Fraction)->bool{
    a::n == b::n && a::d == b::d
}

fn floor(a: Fraction)->int{
    div_floor(a::n,a::d)
}

fn hash(a: Fraction)->int{
    (a::n, a::d).hash()
}

fn mod(a: Fraction, b: Fraction)->Fraction{
    fraction((a::n * b::d) % (b::n * a::d), a::d * b::d)
}

fn mul(a: Fraction, b: Fraction)->Fraction{
    fraction(a::n*b::n, a::d*b::d)
}

fn neg(a: Fraction)->Fraction{
    Fraction(-a::n, a::d)
}

fn pow(a: Fraction, b: int)->Fraction{
    if(b >= 0, fraction(a::n**b, a::d**b), fraction(a::d**-b, a::n**-b))
}

fn sign(a: Fraction)->int{
    sign(a::n)
}

fn sub(a: Fraction, b: Fraction)->Fraction{
    fraction(a::n*b::d - b::n*a::d, a::d*b::d)
}

fn trunc(a: Fraction)->int{
    if(a::n >= 0, floor(a), ceil(a))
}

/// JSON 1
/*
union JSON(
    number: float,
    bool: bool,
    string: str,
    null: (),
    array: Sequence<JSON>,
    object: Mapping<str, JSON>,
)
*/

fn json(a: Sequence<JSON>)->JSON{
    JSON::array(a)
}

fn json(a: Mapping<str, JSON>)->JSON{
    JSON::object(a)
}

fn json(s: str)->JSON{
    JSON::string(s)
}

fn json(b: bool)->JSON{
    JSON::bool(b)
}

fn json(n: float)->JSON{
    JSON::number(n)
}

fn json(n: int)->JSON{
    JSON::number(n.to_float())
}

fn json(n: ())->JSON{
    JSON::null(())
}

fn eq(a: JSON, b: JSON)->bool{
    a.members() == b.members()
}

fn json_deserialize(s: str)->JSON{
    __std_json_deserialize(s, hash{str}, eq{str, str})
}

fn serialize(a: JSON)->str{
    fn serialize_array(s: Sequence<JSON>)->str{
        "[" + s.map(serialize).join(",") + "]"
    }

    fn serialize_object(m: Mapping<str, JSON>)->str{
        "{" + m.to_generator().map((item: (str, JSON))->{[__std_json_serialize_str(item::item0), ":", serialize(item::item1)].join()}).join(",") + "}"
    }

    a?:number.map((n:float)->{n.to_str()})
    || a?:bool.map((b:bool)->{b.to_str()})
    || a?:string.map(__std_json_serialize_str)
    || a?:null.map((n:())->{"null"})
    || a?:array.map(serialize_array)
    || serialize_object(a!:object)

}

/// LinearRegression 1
struct LinearRegression(
    slope: float,
    intercept: float,
)

fn eq(a: LinearRegression, b: LinearRegression)->bool{
    a::slope == b::slope && a::intercept == b::intercept
}

fn linear_regression_least_squares(x: Generator<float>, y: Generator<float>)->LinearRegression{
    let s = zip(x, y);

    let agg = (n: (int,float,float), i: (float, float))->{(n::item0+1, n::item1+i::item0, n::item2+i::item1)};
    let agg = s.reduce((0,0.0,0.0), agg);
    let mean_x = agg::item1/agg::item0;
    let mean_y = agg::item2/agg::item0;

    let beta = s.map((t:(float, float))->{(t::item0-mean_x)*(t::item1-mean_y)}).sum() / x.map((t:float)->{(t-mean_x)**2}).sum();
    LinearRegression(beta, mean_y - beta*mean_x)
}

fn predict(self: LinearRegression, x: float)->float{
    self::slope*x + self::intercept
}

/// Matrix 1
/*
struct Matrix<T>(_cols: int, _data: Sequence<T>)
*/

fn columns<T>(self: Matrix<T>)->int{
    self::_cols
}

fn full<T>(rows: int, cols: int, value: T)->Matrix<T>{
    Matrix(cols, range(rows*cols).map((i: int)->{value}))
}

fn get<T>(self: Matrix<T>, i: int, j: int)->T{
    self::_data[i*self::_cols + j]
}

fn get<T>(self: Matrix<T>, c: (int, int))->T{
    self::_data[c::item0*self::_cols + c::item1]
}

fn manifest<T>(self: Matrix<T>)->Matrix<T>{
    Matrix(self::_cols, self::_data.to_array())
}

fn matrix<T>(rows: int, cols: int, data: Sequence<T>)->Matrix<T>{
    if(rows*cols != data.len(), error("matrix: rows*cols must equal data.len()"), Matrix(cols, data))
}

fn matrix<T>(rows: int, cols: int, f: (int, int)->(T))->Matrix<T>{
    matrix(rows, cols, range(rows*cols).map((i: int)->{f(i.div_floor(cols), i%cols)}))
}

fn matrix<T>(rows: int, cols: int, f: ((int, int))->(T))->Matrix<T>{
    matrix(rows, cols, range(rows*cols).map((i: int)->{f((i.div_floor(cols), i%cols))}))
}

fn matrix<T>(data: Sequence<Sequence<T>>)->Matrix<T>{
    fn main1()->Matrix<T>{
        let cols = data[0].len();
        if(data.any((row: Sequence<T>)->{row.len() != cols}),
            error("matrix: all rows must have the same length"),
            matrix(data.len(), cols, (i: int, j:int)->{data[i][j]})
        )
    }

    if(data.len() == 0, error("data must be non-empty"),
        main1()
    )
}

fn rows<T>(self: Matrix<T>)->int{
    self::_data.len().div_floor(self::_cols)
}

fn to_sequence<T>(self: Matrix<T>)->Sequence<Sequence<T>>{
    range(self.rows()).map((i: int)->{range(self.columns()).map((j: int)->{self[i,j]})})
}

//// gen 2
/// int 2
fn factorial(n: int, step: int ?= 1)->int{
    if(n < 0, error("cannot get factorial of negative number"),
        range(n,0,-step).to_generator().reduce(1, mul{int, int}))
}

fn floor_root(a: int, b: int ?= 2)->int{
    if(a<0, error("a must be non-negative"), range(1, a).bisect((x:int)->{x**b <= a}))
}

/// float 2
fn covariance(s: Generator<(float, float)>)->float{
    let agg = (n: (int,float,float), i: (float, float))->{(n::item0+1, n::item1+i::item0, n::item2+i::item1)};
    let agg = s.reduce((0,0.0,0.0), agg);
    let mean_x = agg::item1/agg::item0;
    let mean_y = agg::item2/agg::item0;
    s.map((t:(float, float))->{(t::item0-mean_x)*(t::item1-mean_y)}).mean()
}

/// str 2
fn ends_with(s: str, suffix: str)->bool{
    suffix.len() <= s.len() && s.substring(s.len()-suffix.len()) == suffix
}

fn lstrip(s: str, predicate: (str)->(bool))->str{
    let c = s.chars().to_generator().take_while(predicate).len();
    s.substring(c)
}

fn lstrip(s: str)->str{
    s.lstrip(is_whitespace)
}

fn partition(s: str, n: str)->(str,str){
    s.find(n).map((i:int)->{(s.substring(0,i), s.substring(i+len(n)))}) || (s, "")
}

fn remove_suffix(s: str, suffix: str)->str{
    if(s.ends_with(suffix), s.substring(0, len(s)-len(suffix)), s)
}

fn reverse(s: str)->str{
    s.chars().reverse().join()
}

fn rpartition(s: str, n: str)->(str,str){
    s.rfind(n).map((i:int)->{(s.substring(0,i), s.substring(i+len(n)))}) || ("", s)
}

fn rsplit(s: str, n: str, count:int)->Sequence<str>{
    fn rfind_helper(s: str, strp: int)->int{
        s.rfind(n, strp).map((i:int)->{i+len(n)}) || 0
    }

    fn next_bound(prev_bounds: (int, int, int))->Optional<(int, int, int)>{
        (prev_bounds::item0 != 0).then((
            if(prev_bounds::item2==count, 0, s.rfind_helper(prev_bounds::item0-len(n))),
            prev_bounds::item0-len(n),
            prev_bounds::item2+1
        ))
    }
    fn bounds_to_string(bounds: (int, int))->str{
        s.substring(bounds::item0, bounds::item1)
    }

    fn nontrivial()->Sequence<str>{
        let first_match = s.rfind_helper(len(s));
        successors_until((first_match, len(s), 1), next_bound)
        .map((b: (int, int, int)) -> {(b::item0, b::item1)})
        .to_array()
        .reverse()
        .map(bounds_to_string)
    }
    if(count==0, [s], nontrivial())
}

fn rstrip(s: str, predicate: (str)->(bool))->str{
    let c = s.chars().reverse().to_generator().take_while(predicate).len();
    s.substring(0, s.len()-c)
}

fn rstrip(s: str)->str{
    s.rstrip(is_whitespace)
}

fn split(s: str, n: str)->Generator<str>{
    fn next_bound(prev_bounds: (int, Optional<int>))->Optional<(int, Optional<int>)>{
        fn from_prev_end(prev_end: int)->(int, Optional<int>){
            let next_start = prev_end+n.len();
            (next_start, s.find(n,next_start))
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

fn split(s: str, n: str, count:int)->Generator<str>{
    fn next_bound(prev_bounds: (int, Optional<int>, int))->Optional<(int, Optional<int>, int)>{
        fn from_prev_end(prev_end: int)->(int, Optional<int>, int){
            let next_start = prev_end+n.len();
            (
                next_start, 
                if(prev_bounds::item2==count, none(), s.find(n,next_start)),
                prev_bounds::item2+1
            )
        }
        prev_bounds::item1.map(from_prev_end)
    }
    fn bounds_to_string(bounds: (int, Optional<int>, int))->str{
        s.substring(bounds::item0, bounds::item1)
    }

    fn nontrivial()->Generator<str>{
        let first_match = s.find(n);
        successors_until((0, first_match, 1), next_bound)
        .map(bounds_to_string)
    }
    if(count==0, [s].to_generator(), nontrivial())
}

fn starts_with(s: str, prefix: str)->bool{
    s.substring(0, prefix.len()) == prefix
}

fn strip(s: str, predicate: (str)->(bool))->str{
    let chrs = s.chars();
    let skip_from_start = chrs.to_generator().take_while(predicate).len();
    let skip_from_end = chrs.skip(skip_from_start).reverse().to_generator().take_while(predicate).len();
    s.substring(skip_from_start, s.len()-skip_from_end)
}

fn strip(s: str)->str{
    s.strip(is_whitespace)
}

/// Generator 2
fn flatten<T>(g: Generator<Generator<T>>)->Generator<T>{
    g.reduce([].to_generator(), add{Generator<T>, Generator<T>})
}

fn max<T>(g: Generator<T>, lt: (T,T)->(bool)) -> T {
    g.reduce((a: T, b: T)->{max(a,b,lt)})
}

fn min<T>(g: Generator<T>, lt: (T,T)->(bool)) -> T {
    g.reduce((a: T, b: T)->{min(a,b,lt)})
}

fn product(g: Generator<int>)->int{
    g.product(1)
}

fn product(g: Generator<float>)->float{
    g.product(1.0)
}

fn repeat<T>(g: Generator<T>, n: int)->Generator<T>{
    [g].to_generator().repeat().take(n).flatten()
}

/// Mapping 2
fn update_counter<K>(m: Mapping<K, int>, g: Generator<K>)->Mapping<K, int>{
    m.update_from_keys(g, (_: K)->{1}, (_: K, v: int)->{v+1})
}

/// Regex 2
fn match(r: Regex, s: str, i: int ?= 0)->Optional<Match>{
    r.search(s,i,i)
}

/// Set 2
fn bit_xor<T>(a: Set<T>, b: Set<T>)->Set<T>{
    (a-b) | (b-a)
}

/// Sequence 2
fn count<T>(s: Sequence<T>, f: (T)->(bool))->int{
    s.filter(f).len()
}

fn count<T, U>(s: Sequence<T>, i: U, eq_: (T,U)->(bool))->int{
    s.filter((t: U)->{eq_(i, t)}).len()
}

fn flatten<T>(g: Sequence<Generator<T>>)->Generator<T>{
    g.reduce([].to_generator(), add{Generator<T>, Generator<T>})
}

fn max<T>(g: Sequence<T>, lt: (T,T)->(bool)) -> T {
    g.reduce((a: T, b: T)->{max(a,b,lt)})
}

fn min<T>(g: Sequence<T>, lt: (T,T)->(bool)) -> T {
    g.reduce((a: T, b: T)->{min(a,b,lt)})
}

fn mul<T>(a: Sequence<T>, b: int)->Sequence<T>{
    a.repeat(b)
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

fn permutations<T>(a: Sequence<T>)->Sequence<Sequence<T>>{
    let n = a.len().factorial();
    range(n).map((i: int)->{permutation(a, i)})
}

fn product(g: Sequence<int>)->int{
    g.product(1)
}

fn product(g: Sequence<float>)->float{
    g.product(1.0)
}

fn rank_avg<T0>(seq: Sequence<T0>, t: T0, cmp: (T0, T0)->(int))->float{
    seq.sort(cmp).rank_sorted_avg(t, cmp)
}

fn rank_eq<T0>(seq: Sequence<T0>, t: T0, cmp: (T0, T0)->(int))->int{
    seq.sort(cmp).rank_sorted_eq(t, cmp)
}

/// Complex 2
fn is_close(a: Complex, b: Complex, rel_tol: float ?= 1e-6, abs_tol: float ?= 1e-6)->bool{
    let diff = a-b;
    diff.abs() <= max(max(a.abs(), b.abs())*rel_tol, abs_tol)
}

/// ContinuousDistribution 2
fn random()->float{
    standard_uniform_distribution().random()
}

/// Date 2
fn add(d0: Date, dur: Duration)->Date{
    date(d0.julian_day() + dur.days().trunc())
}

fn format(self: Date, fmt: str)->str{
    fn repl(s: str)->str{
        if(s=="w", to_str((self.weekday() + 1)%7), 
        if(s=="a", ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"][self.weekday()-6],
        if(s=="A", ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"][self.weekday()-6],
        if(s=="b", ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"][self::month-1],
        if(s=="B", ["January", "Febuary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"][self::month-1],
        if(s=="d", self::day.format("02"),
        if(s=="m", self::month.format("02"),
        if(s=="y", (self::year%100).format("02"),
        if(s=="Y", self::year.format("04"), "%"
        )))))))))
    }
    fmt.format_replace(repl)
}

fn sub(d0: Date, d1: Date)->Duration{
    (d0.julian_day() - d1.julian_day()).to_float().days()
}

/// Datetime 2
fn add(d0: Datetime, d1: Duration)->Datetime{
    datetime(d0.unix()+d1.seconds())
}

fn add(d0: Duration, d1: Datetime)->Datetime{
    d1 + d0
}

fn sub(d0: Datetime, d1: Datetime)->Duration{
    seconds(d0.unix() - d1.unix())
}

/// DiscreteDistribution 2
fn sample_standard_deviation(s: Sequence<int>)->float{
    sample_variance(s).sqrt()
}

/// Matrix 2
fn add<T0, T1, U>(a: Matrix<T0>, b: Matrix<T1>, add_: (T0, T1)->(U))->Matrix<U>{
    if(a.columns() != b.columns() || a.rows() != b.rows(),
        error("matrix dimensions must match"), 
        matrix(a.rows(), a.columns(), (c: (int, int))->{add_(a[c], b[c])})
    )
}

fn eye<T>(n: int, one: T, zero: T)->Matrix<T>{
    matrix(n, n, (i: int, j: int)->{if(i==j, one, zero)})
}

fn eye(n: int)->Matrix<float>{
    eye(n, 1.0, 0.0)
}

//// gen 3
/// int 3
fn ceil_root(a: int, b: int ?= 2)->int{
    if(a==0, 0, 1+floor_root(a-1, b))
}

/// str 3
fn mul(a: str, n: int)->str{
    ([a]*n).join()
}

fn remove_prefix(s: str, prefix: str)->str{
    if(s.starts_with(prefix), s.substring(len(prefix)), s)
}

fn replace(s: str, old: str, new: str)->str{
    s.split(old).join(new)
}

fn replace(s: str, old: str, new: str, count: int)->str{
    let parts = s.split(old, count).to_array();
    parts.take(count+1).join(new)
    + parts.skip(count+1).map((s: str)->{old + s}).join()
}

/// Sequence 3
fn permutations<T>(a: Sequence<T>, k: int)->Sequence<Sequence<T>>{
    let n = range(a.len()-k+1, a.len()+1).product();
    range(n).map((i: int)->{permutation(a, i, k)})
}
"#;
