use crate::builtin::core::{eval, search, ufunc_ref, xcmp, xerr};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::root_runtime_scope::RuntimeResult;
use crate::util::xformatter::XFormatting;
use crate::xtype::{XFuncSpec, X_BOOL, X_FLOAT, X_INT, X_STRING};
use crate::xvalue::{ManagedXError, ManagedXValue, XResult, XValue};
use crate::{
    add_binfunc, forward_err, manage_native, to_native, to_primitive, ufunc, xraise, xraise_opt,
    CompilationError, XStaticFunction,
};

use num_integer::binomial;
use num_traits::{One, Pow, Signed, ToPrimitive, Zero};

use rc::Rc;
use std::cmp::max;

use std::convert::TryFrom;
use std::f64::consts::LOG10_2;

use std::ops::Neg;
use std::rc;

use crate::root_compilation_scope::RootCompilationScope;
use crate::runtime::{ProspectiveSize, RTCell};
use crate::util::fenced_string::FencedString;
use crate::util::lazy_bigint::LazyBigint;

pub(crate) fn add_int_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_native_type("int", X_INT.clone())
}

macro_rules! add_int_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {
        add_binfunc!($fn_name, $name, X_INT, Int, X_INT, $func);
    };
}

add_int_binop!(
    add_int_add,
    add,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        rt.can_allocate(max(a.prospective_size(), b.prospective_size()))?;
        Ok(Ok(XValue::Int(a.clone() + b.clone())))
    }
);
add_int_binop!(
    add_int_sub,
    sub,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        rt.can_allocate(max(a.prospective_size(), b.prospective_size()))?;
        Ok(Ok(XValue::Int(a.clone() - b.clone())))
    }
);
add_int_binop!(
    add_int_mul,
    mul,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        rt.can_allocate(a.prospective_size() + b.prospective_size())?;
        Ok(Ok(XValue::Int(a.clone() * b.clone())))
    }
);
add_int_binop!(
    add_int_mod,
    mod,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        Ok(if b.is_zero() {
            Err(String::from("Modulo by zero"))
        } else {
            rt.can_afford(b)?;
            Ok(XValue::Int(a.clone() % b.clone()))
        })
    }
);
add_int_binop!(
    add_int_bit_or,
    bit_or,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        rt.can_allocate(max(a.prospective_size(), b.prospective_size()))?;
        Ok(Ok(XValue::Int(a.clone() | b.clone())))
    }
);
add_int_binop!(
    add_int_bit_and,
    bit_and,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        rt.can_allocate(max(a.prospective_size(), b.prospective_size()))?;
        Ok(Ok(XValue::Int(a.clone() & b.clone())))
    }
);
add_int_binop!(
    add_int_bit_xor,
    bit_xor,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        rt.can_allocate(max(a.prospective_size(), b.prospective_size()))?;
        Ok(Ok(XValue::Int(a.clone() ^ b.clone())))
    }
);
add_binfunc!(add_int_div, div, X_INT, Int, X_FLOAT, |a: &LazyBigint,
                                                     b: &LazyBigint,
                                                     rt: &RTCell<
    W,
    R,
    T,
>| {
    Ok(if b.is_zero() {
        Err(String::from("Division by zero"))
    } else {
        rt.can_allocate(a.prospective_size() - b.prospective_size())?;
        Ok(XValue::Float(a.clone().true_div(b.clone())))
    })
});
add_binfunc!(
    add_int_div_floor,
    div_floor,
    X_INT,
    Int,
    X_INT,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        Ok(if b.is_zero() {
            Err(String::from("Division by zero"))
        } else {
            rt.can_allocate(a.prospective_size() - b.prospective_size())?;
            Ok(XValue::Int(a.clone().div_floor(b.clone())))
        })
    }
);

add_binfunc!(
    add_int_div_ceil,
    div_ceil,
    X_INT,
    Int,
    X_INT,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        Ok(if b.is_zero() {
            Err(String::from("Division by zero"))
        } else {
            rt.can_allocate(a.prospective_size() - b.prospective_size())?;
            Ok(XValue::Int(a.clone().div_ceil(b.clone())))
        })
    }
);

add_binfunc!(add_int_pow, pow, X_INT, Int, X_INT, |a: &LazyBigint,
                                                   b: &LazyBigint,
                                                   rt: &RTCell<
    W,
    R,
    T,
>| Ok(
    if b.is_negative() {
        Err(String::from("cannot raise integer to a negative power"))
    } else if b.is_zero() && a.is_zero() {
        Err(String::from("cannot raise zero to a zero power"))
    } else {
        rt.can_allocate_by(|| {
            b.to_usize()
                .zip(a.bits().to_usize())
                .map(|(b, a_bits)| (a_bits / 8) * b)
        })?;
        Ok(XValue::Int(a.clone().pow(b.clone())))
    }
));
add_binfunc!(add_int_lt, lt, X_INT, Int, X_BOOL, |a, b, _rt| Ok(Ok(
    XValue::Bool(a < b)
)));
add_binfunc!(add_int_gt, gt, X_INT, Int, X_BOOL, |a, b, _rt| Ok(Ok(
    XValue::Bool(a > b)
)));
add_binfunc!(add_int_eq, eq, X_INT, Int, X_BOOL, |a, b, _rt| Ok(Ok(
    XValue::Bool(a == b)
)));
add_binfunc!(add_int_ne, ne, X_INT, Int, X_BOOL, |a, b, _rt| Ok(Ok(
    XValue::Bool(a != b)
)));
add_binfunc!(add_int_le, le, X_INT, Int, X_BOOL, |a, b, _rt| Ok(Ok(
    XValue::Bool(a <= b)
)));
add_binfunc!(add_int_ge, ge, X_INT, Int, X_BOOL, |a, b, _rt| Ok(Ok(
    XValue::Bool(a >= b)
)));

pub(crate) fn add_int_neg<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "neg",
        XFuncSpec::new(&[&X_INT], X_INT.clone()),
        ufunc!(Int, |a: &LazyBigint, rt: RTCell<W, R, T>| {
            rt.can_afford(a)?;
            Ok(Ok(XValue::Int(a.clone().neg())))
        }),
    )
}

pub(crate) fn add_int_to_str<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "to_str",
        XFuncSpec::new(&[&X_INT], X_STRING.clone()),
        ufunc!(Int, |a: &LazyBigint, rt: RTCell<W, R, T>| {
            rt.can_allocate_by(|| ((a.bits() / 8) as f64 * LOG10_2).ceil().to_usize())?;
            Ok(Ok(XValue::String(Box::new(FencedString::from_string(
                a.to_string(),
            )))))
        }),
    )
}

pub(crate) fn add_int_to_float<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "to_float",
        XFuncSpec::new(&[&X_INT], X_FLOAT.clone()),
        ufunc!(Int, |a: &LazyBigint, _rt: RTCell<W, R, T>| {
            let Some(ret) = a.to_f64() else {return Ok(Err("value is too large to fit in a float".to_string()))};
            Ok(Ok(XValue::Float(
                ret
            )))
        }),
    )
}

pub(crate) fn add_int_digits<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "digits",
        XFuncSpec::new_with_optional(&[&X_INT], &[&X_INT], XSequenceType::xtype(X_INT.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let n = to_primitive!(a0, Int);
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            let b = to_primitive!(a1, Int, LazyBigint::from(10));
            let mut digits = Vec::new();
            let mut total_bits = 0;
            let mut n = n.clone();
            while !n.is_zero() {
                let next_digit = n.clone() % b.clone().into_owned();
                total_bits += next_digit.bits();
                rt.can_allocate_by(|| (total_bits / 8).to_usize())?;
                digits.push(next_digit);
                n = n / b.clone().into_owned();
            }
            Ok(manage_native!(
                XSequence::array(
                    digits
                        .into_iter()
                        .map(|v| ManagedXValue::new(XValue::Int(v), rt.clone()))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .collect()
                ),
                rt
            ))
        }),
    )
}

pub(crate) fn add_int_hash<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "hash",
        XFuncSpec::new(&[&X_INT], X_INT.clone()),
        ufunc_ref(|a: Rc<ManagedXValue<W, R, T>>, rt| {
            let v = to_primitive!(a, Int);
            if v.to_u64().is_some() {
                // the number is already within the bounds
                return Ok(a.into());
            }
            Ok(ManagedXValue::new(XValue::Int(v.first_u64_digit()), rt)?.into())
        }),
    )
}

add_binfunc!(add_int_cmp, cmp, X_INT, Int, X_INT, |a, b, _rt| Ok(Ok(
    xcmp(a, b)
)));

pub(crate) fn add_int_chr<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "chr",
        XFuncSpec::new(&[&X_INT], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let s = to_primitive!(a0, Int);
            let Some(ord) = s.to_u32() else { return xerr(ManagedXError::new("number too large", rt)?); };
            let Ok(chr) = char::try_from(ord) else { return xerr(ManagedXError::new("value is not a unicode char", rt)?); };
            Ok(ManagedXValue::new(XValue::String(Box::new(FencedString::from_string(chr.into()))), rt)?.into())
        }),
    )
}

add_int_binop!(
    add_int_binom,
    binom,
    |a: &LazyBigint, b: &LazyBigint, rt: &RTCell<W, R, T>| {
        if b > a {
            return Ok(Err("argument 2 must be less than argument 1".to_string()));
        }
        if b.is_negative() {
            return Ok(Err("argument 2 must be non-negative".to_string()));
        }
        let mut num = LazyBigint::one();
        let mut denum = LazyBigint::one();
        for (i, s) in search(b.range(), rt.clone()) {
            s?;
            num *= a - &i;
            denum *= &i + &LazyBigint::one();
            rt.can_allocate_by(|| Some(num.prospective_size() + denum.prospective_size()))?;
        }
        Ok(Ok(XValue::Int(num / denum)))
    }
);

pub(crate) fn add_int_multinom<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "multinom",
        XFuncSpec::new(&[&XSequenceType::xtype(X_INT.clone())], X_INT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let Some(s) = to_native!(a0, XSequence::<W, R, T>).diter(ns, rt.clone()) else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };

            let mut s = xraise!(s.map(|v|->XResult<LazyBigint, W, R, T>{
                Ok(Ok(to_primitive!(forward_err!(v?), Int).clone()))
            }).collect::<XResult<Vec<_>, W, R, T>>()?);
            if s.len() <= 1{
                return Ok(ManagedXValue::new(XValue::Int(LazyBigint::one()), rt)?.into())
            }
            s.sort_unstable_by(|a,b| LazyBigint::cmp(a,b).reverse());
            if s.last().unwrap().is_negative(){
                return xerr(ManagedXError::new("sequence cannot have negative values", rt)?);
            }
            let mut num_ctr = s[0].clone() + LazyBigint::one();
            let mut num = LazyBigint::one();
            let mut denum = LazyBigint::one();
            let mut search =  rt.limits.search_iter();
            for item in s.into_iter().skip(1).take_while(|i| i.is_positive()){
                for i in item.range(){
                    search.next().unwrap()?;
                    rt.can_allocate_by(|| Some(num.prospective_size() + denum.prospective_size()))?;
                    num *= &i + &num_ctr;
                    denum *= &i + &LazyBigint::one();
                }
                num_ctr += item;
            }
            Ok(ManagedXValue::new(XValue::Int(num / denum), rt)?.into())
        }),
    )
}

pub(crate) fn add_int_permutation<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "permutation",
        XFuncSpec::new(&[&X_INT, &X_INT, &X_INT], XSequenceType::xtype(X_INT.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            
            let Some(n) = to_primitive!(a0, Int).to_usize() else { return xerr(ManagedXError::new("n out of bounds", rt)?); };
            let Some(mut i) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("k out of bounds", rt)?); };
            let Some(k) = to_primitive!(a2, Int).to_usize() else { return xerr(ManagedXError::new("i out of bounds", rt)?); };

            if k > n{
                return xerr(ManagedXError::new("k cannot be greater than n", rt)?);
            }
            let total = (n-k+1..=n).product();
            if i >= total{
                return xerr(ManagedXError::new("i too large", rt)?);
            }
            rt.can_allocate(k)?;
            let mut ret = Vec::with_capacity(k);
            for j in (0..k).rev(){
                ret.push(i % (n-j));
                i /= n-j;
            }
            ret.reverse();
            for t0 in (1..k).rev(){
                for t1 in (0..t0).rev(){
                    if ret[t1] <= ret[t0]{
                        ret[t0] += 1;
                    }
                }
            }
            let arr = ret.into_iter().map(|i| ManagedXValue::new(XValue::Int(LazyBigint::from(i)), rt.clone())).collect::<RuntimeResult<Vec<_>>>()?;
            Ok(manage_native!(XSequence::Array(arr), rt))
        }),
    )
}

pub(crate) fn add_int_combination<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "combination",
        XFuncSpec::new(&[&X_INT, &X_INT, &X_INT], XSequenceType::xtype(X_INT.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            
            let Some(n) = to_primitive!(a0, Int).to_usize() else { return xerr(ManagedXError::new("n out of bounds", rt)?); };
            let Some(mut i) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("k out of bounds", rt)?); };
            let Some(mut k) = to_primitive!(a2, Int).to_usize() else { return xerr(ManagedXError::new("i out of bounds", rt)?); };

            if k > n{
                return xerr(ManagedXError::new("k cannot be greater than n", rt)?);
            }
            let mut s_cutoff = binomial(n-1,k-1);
            let total = s_cutoff*n/k;
            if i >= total{
                return xerr(ManagedXError::new("i too large", rt)?);
            }
            let mut s = 0;
            rt.can_allocate(k)?;
            let mut ret = Vec::with_capacity(k);
            while k > 0{
                if i < s_cutoff{
                    ret.push(s);
                    if k > 1{
                        s_cutoff = s_cutoff*(k-1)/(n-s-1);
                    }
                    k -= 1;
                    s+=1;
                } else {
                    i -= s_cutoff;
                    s_cutoff = s_cutoff*(n-s-k)/(n-s-1);
                    s+=1;
                }
            }
            let arr = ret.into_iter().map(|i| ManagedXValue::new(XValue::Int(LazyBigint::from(i)), rt.clone())).collect::<RuntimeResult<Vec<_>>>()?;
            Ok(manage_native!(XSequence::Array(arr), rt))
        }),
    )
}

pub(crate) fn add_int_combination_with_replacement<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "combination_with_replacement",
        XFuncSpec::new(&[&X_INT, &X_INT, &X_INT], XSequenceType::xtype(X_INT.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            
            let Some(n) = to_primitive!(a0, Int).to_usize() else { return xerr(ManagedXError::new("n out of bounds", rt)?); };
            let Some(mut i) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("k out of bounds", rt)?); };
            let Some(mut k) = to_primitive!(a2, Int).to_usize() else { return xerr(ManagedXError::new("i out of bounds", rt)?); };

            if k > n{
                return xerr(ManagedXError::new("k cannot be greater than n", rt)?);
            }
            let mut s_cutoff = binomial(n+k-2,k-1);
            let total = (s_cutoff*(n+k-1))/k;
            if i >= total{
                return xerr(ManagedXError::new("i too large", rt)?);
            }
            let mut s = 0;
            rt.can_allocate(k)?;
            let mut ret = Vec::with_capacity(k);
            while k > 0{
                if i < s_cutoff{
                    ret.push(s);
                    if k > 1{
                        s_cutoff = (s_cutoff*(k-1))/(k+n-s-2);
                    }
                    k -= 1;
                } else {
                    i -= s_cutoff;
                    s_cutoff = (s_cutoff*(n-s-1))/(k+n-s-2);
                    s+=1;
                }
            }
            let arr = ret.into_iter().map(|i| ManagedXValue::new(XValue::Int(LazyBigint::from(i)), rt.clone())).collect::<RuntimeResult<Vec<_>>>()?;
            Ok(manage_native!(XSequence::Array(arr), rt))
        }),
    )
}

pub(crate) fn add_int_format<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "format",
        XFuncSpec::new(&[&X_INT, &X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let i0 = to_primitive!(a0, Int);
            let s1 = to_primitive!(a1, String);
            let Some(specs) = XFormatting::from_str(s1.as_str()) else {return xerr(ManagedXError::new("invalid format spec", rt)?);};

            if specs.precision.is_some(){
                return xerr(ManagedXError::new("int cannot be formatted with precision", rt)?);
            }

            rt.can_allocate_by(|| {
                Some(max(
                    ((i0.bits() / 8) as f64
                        * LOG10_2
                        * (3.0 + specs.grouping.map(|g| g.len() as f64).unwrap_or_default())
                        / 3.0)
                        .ceil() as usize,
                    specs.min_width(),
                ))
            })?;
            let radix = match specs.ty.type_ {
                None => 10,
                Some("x") | Some("X") => 16,
                Some("o") | Some("O") => 8,
                Some("b") | Some("B") => 2,
                Some(other) => {return xerr(ManagedXError::new(format!("unrecognized int type: {other}"), rt)?);}
            };
            let s = i0.magnitude_to_str(radix);
            let body = specs.group(&s);
            let mut sign_parts = specs.sign(i0.is_negative());

            if specs.ty.alternative {
                if let Some(type_) = specs.ty.type_ {
                    sign_parts.extend(["0", type_]);
                } else {
                    return xerr(ManagedXError::new("missing type for alt", rt)?);
                }
            }
            let (prefix, infix, postfix) = specs
                .fill_specs
                .map(|f| f.fillers(body.len() + sign_parts.len()))
                .unwrap_or_default();
            let ret = XValue::String(Box::new(FencedString::from_string(format!(
                "{prefix}{sign_parts}{infix}{body}{postfix}"
            ))));
            Ok(ManagedXValue::new(ret, rt)?.into())
        }),
    )
}
