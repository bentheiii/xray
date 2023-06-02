use either::Either;

macro_rules! mk_either_types {
    (type [Either2,]<T1, T0>) => {
        type Either2<T0, T1> = Either<T0, T1>;
    };
    (type [$t0:ident, $t1:ident $(,)? $($t:ident),*]<$g0:ident, $($g:ident),+>)=>{
        mk_either_types!(type [$t1, $($t),*]<$($g),+>);
        type $t0<$g0, $($g),+> = Either<$g0, $t1<$($g),+>>;
    };
}

mk_either_types!(
    type [
        Either17,
        Either16,
        Either15,
        Either14,
        Either13,
        Either12,
        Either11,
        Either10,
        Either9,
        Either8,
        Either7,
        Either6,
        Either5,
        Either4,
        Either3,
        Either2
    ]<
        T16,
        T15,
        T14,
        T13,
        T12,
        T11,
        T10,
        T9,
        T8,
        T7,
        T6,
        T5,
        T4,
        T3,
        T2,
        T1,
        T0
    >
);

macro_rules! mk_eithers {
    ([$fn_name: ident/$fn_name_last:ident,]<$t0:ident, $t1:ident> => {$out_t: ident})=>{
        pub(crate) fn $fn_name<$t0, $t1>(v: $t0) -> $out_t<$t0, $t1> {
            Either::Left(v)
        }

        pub(crate) fn $fn_name_last<$t0, $t1>(v: $t1) -> $out_t<$t0, $t1> {
            Either::Right(v)
        }
    };
    ([$fn_name_0:ident/$fn_name_last_0:ident, $fn_name_1:ident/$fn_name_last_1:ident $(,)? $($fn_name: ident/$fn_name_last:ident),*] <$t_name_0:ident, $t_name_1:ident, $($t_name:ident),+> => {$t_out_0:ident, $($out_t:ident),+}) => {
        mk_eithers!(
            [$fn_name_1/$fn_name_last_1, $($fn_name/$fn_name_last),*]<$t_name_1, $($t_name),*> => {$($out_t),*}
        );

        pub(crate) fn $fn_name_0<$($t_name),*, $t_name_1, $t_name_0>(v: $t_name_1) -> $t_out_0<$($t_name),*, $t_name_1, $t_name_0> {
            Either::Right($fn_name_1(v))
        }

        pub(crate) fn $fn_name_last_0<$($t_name),*, $t_name_1, $t_name_0>(v: $t_name_0) -> $t_out_0<$($t_name),*, $t_name_1, $t_name_0> {
            Either::Right($fn_name_last_1(v))
        }
    };
}

mk_eithers!(
    [
        either_p/either_q_last,
        either_o/either_p_last,
        either_n/either_o_last,
        either_m/either_n_last,
        either_l/either_m_last,
        either_k/either_l_last,
        either_j/either_k_last,
        either_i/either_j_last,
        either_h/either_i_last,
        either_g/either_h_last,
        either_f/either_g_last,
        either_e/either_f_last,
        either_d/either_e_last,
        either_c/either_d_last,
        either_b/either_c_last,
        either_a/either_b_last
    ]<
        T16,
        T15,
        T14,
        T13,
        T12,
        T11,
        T10,
        T9,
        T8,
        T7,
        T6,
        T5,
        T4,
        T3,
        T2,
        T1,
        T0
    > => {
        Either17,
        Either16,
        Either15,
        Either14,
        Either13,
        Either12,
        Either11,
        Either10,
        Either9,
        Either8,
        Either7,
        Either6,
        Either5,
        Either4,
        Either3,
        Either2
    }
);
