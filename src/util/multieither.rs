use either::Either;

type Either2<T0, T1> = Either<T0, T1>;
type Either3<T0, T1, T2> = Either<T0, Either2<T1, T2>>;
type Either4<T0, T1, T2, T3> = Either<T0, Either3<T1, T2, T3>>;
type Either5<T0, T1, T2, T3, T4> = Either<T0, Either4<T1, T2, T3, T4>>;
type Either6<T0, T1, T2, T3, T4, T5> = Either<T0, Either5<T1, T2, T3, T4, T5>>;
type Either7<T0, T1, T2, T3, T4, T5, T6> = Either<T0, Either6<T1, T2, T3, T4, T5, T6>>;
type Either8<T0, T1, T2, T3, T4, T5, T6, T7> = Either<T0, Either7<T1, T2, T3, T4, T5, T6, T7>>;
type Either9<T0, T1, T2, T3, T4, T5, T6, T7, T8> =
    Either<T0, Either8<T1, T2, T3, T4, T5, T6, T7, T8>>;
type Either10<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9> =
    Either<T0, Either9<T1, T2, T3, T4, T5, T6, T7, T8, T9>>;
type Either11<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> =
    Either<T0, Either10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>>;

pub(crate) fn either_a<T0, T1>(v: T0) -> Either2<T0, T1> {
    Either::Left(v)
}

pub(crate) fn either_b_last<T0, T1>(v: T1) -> Either2<T0, T1> {
    Either::Right(v)
}

pub(crate) fn either_b<T0, T1, T2>(v: T1) -> Either3<T0, T1, T2> {
    Either::Right(either_a(v))
}

pub(crate) fn either_c_last<T0, T1, T2>(v: T2) -> Either3<T0, T1, T2> {
    Either::Right(either_b_last(v))
}

pub(crate) fn either_c<T0, T1, T2, T3>(v: T2) -> Either4<T0, T1, T2, T3> {
    Either::Right(either_b(v))
}

pub(crate) fn either_d_last<T0, T1, T2, T3>(v: T3) -> Either4<T0, T1, T2, T3> {
    Either::Right(either_c_last(v))
}

pub(crate) fn either_d<T0, T1, T2, T3, T4>(v: T3) -> Either5<T0, T1, T2, T3, T4> {
    Either::Right(either_c(v))
}

pub(crate) fn either_e_last<T0, T1, T2, T3, T4>(v: T4) -> Either5<T0, T1, T2, T3, T4> {
    Either::Right(either_d_last(v))
}

pub(crate) fn either_e<T0, T1, T2, T3, T4, T5>(v: T4) -> Either6<T0, T1, T2, T3, T4, T5> {
    Either::Right(either_d(v))
}

pub(crate) fn either_f_last<T0, T1, T2, T3, T4, T5>(v: T5) -> Either6<T0, T1, T2, T3, T4, T5> {
    Either::Right(either_e_last(v))
}

pub(crate) fn either_f<T0, T1, T2, T3, T4, T5, T6>(v: T5) -> Either7<T0, T1, T2, T3, T4, T5, T6> {
    Either::Right(either_e(v))
}

pub(crate) fn either_g_last<T0, T1, T2, T3, T4, T5, T6>(
    v: T6,
) -> Either7<T0, T1, T2, T3, T4, T5, T6> {
    Either::Right(either_f_last(v))
}

pub(crate) fn either_g<T0, T1, T2, T3, T4, T5, T6, T7>(
    v: T6,
) -> Either8<T0, T1, T2, T3, T4, T5, T6, T7> {
    Either::Right(either_f(v))
}

pub(crate) fn either_h_last<T0, T1, T2, T3, T4, T5, T6, T7>(
    v: T7,
) -> Either8<T0, T1, T2, T3, T4, T5, T6, T7> {
    Either::Right(either_g_last(v))
}

pub(crate) fn either_h<T0, T1, T2, T3, T4, T5, T6, T7, T8>(
    v: T7,
) -> Either9<T0, T1, T2, T3, T4, T5, T6, T7, T8> {
    Either::Right(either_g(v))
}

pub(crate) fn either_i_last<T0, T1, T2, T3, T4, T5, T6, T7, T8>(
    v: T8,
) -> Either9<T0, T1, T2, T3, T4, T5, T6, T7, T8> {
    Either::Right(either_h_last(v))
}

pub(crate) fn either_i<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9>(
    v: T8,
) -> Either10<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9> {
    Either::Right(either_h(v))
}

pub(crate) fn either_j_last<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9>(
    v: T9,
) -> Either10<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9> {
    Either::Right(either_i_last(v))
}

pub(crate) fn either_j<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>(
    v: T9,
) -> Either11<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> {
    Either::Right(either_i(v))
}

pub(crate) fn either_k_last<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10>(
    v: T10,
) -> Either11<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> {
    Either::Right(either_j_last(v))
}
