use either::Either;

type Either2<T0, T1> = Either<T0, T1>;
type Either3<T0, T1, T2> = Either<T0, Either2<T1, T2>>;
type Either4<T0, T1, T2, T3> = Either<T0, Either3<T1, T2, T3>>;
type Either5<T0, T1, T2, T3, T4> = Either<T0, Either4<T1, T2, T3, T4>>;
type Either6<T0, T1, T2, T3, T4, T5> = Either<T0, Either5<T1, T2, T3, T4, T5>>;
type Either7<T0, T1, T2, T3, T4, T5, T6> = Either<T0, Either6<T1, T2, T3, T4, T5, T6>>;
type Either8<T0, T1, T2, T3, T4, T5, T6, T7> = Either<T0, Either7<T1, T2, T3, T4, T5, T6, T7>>;

pub(crate) fn either_1<T0, T1>(v: T0) ->Either2<T0, T1>{
    Either::Left(v)
}

pub(crate) fn either_2_last<T0, T1>(v: T1) ->Either2<T0, T1>{
    Either::Right(v)
}

pub(crate) fn either_2<T0, T1, T2>(v: T1) ->Either3<T0, T1, T2>{
    Either::Right(either_1(v))
}

pub(crate) fn either_3_last<T0, T1, T2>(v: T2) ->Either3<T0, T1, T2>{
    Either::Right(either_2_last(v))
}

pub(crate) fn either_3<T0, T1, T2, T3>(v: T2) ->Either4<T0, T1, T2, T3>{
    Either::Right(either_2(v))
}

pub(crate) fn either_4_last<T0, T1, T2, T3>(v: T3) ->Either4<T0, T1, T2, T3>{
    Either::Right(either_3_last(v))
}

pub(crate) fn either_4<T0, T1, T2, T3, T4>(v: T3) ->Either5<T0, T1, T2, T3, T4>{
    Either::Right(either_3(v))
}

pub(crate) fn either_5_last<T0, T1, T2, T3, T4>(v: T4) ->Either5<T0, T1, T2, T3, T4>{
    Either::Right(either_4_last(v))
}

pub(crate) fn either_5<T0, T1, T2, T3, T4, T5>(v: T4) ->Either6<T0, T1, T2, T3, T4, T5>{
    Either::Right(either_4(v))
}

pub(crate) fn either_6_last<T0, T1, T2, T3, T4, T5>(v: T5) ->Either6<T0, T1, T2, T3, T4, T5>{
    Either::Right(either_5_last(v))
}

pub(crate) fn either_6<T0, T1, T2, T3, T4, T5, T6>(v: T5) ->Either7<T0, T1, T2, T3, T4, T5, T6>{
    Either::Right(either_5(v))
}

pub(crate) fn either_7_last<T0, T1, T2, T3, T4, T5, T6>(v: T6) ->Either7<T0, T1, T2, T3, T4, T5, T6>{
    Either::Right(either_6_last(v))
}

pub(crate) fn either_7<T0, T1, T2, T3, T4, T5, T6, T7>(v: T6) ->Either8<T0, T1, T2, T3, T4, T5, T6, T7>{
    Either::Right(either_6(v))
}

pub(crate) fn either_8_last<T0, T1, T2, T3, T4, T5, T6, T7>(v: T7) ->Either8<T0, T1, T2, T3, T4, T5, T6, T7>{
    Either::Right(either_7_last(v))
}