use std::io::Write;
use regex::{Regex};
use crate::CompilationError;
use std::convert::TryFrom;

pub(crate) fn apply_escapes(origin: &str) -> Result<String, CompilationError> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"\\(u\{.+?\}|.)").unwrap();
        static ref BYTECODE: Regex = Regex::new("[a-fA-F0-9]{1,6}$").unwrap();
    }
    // invariant, the last character in the string cannot be a slash
    let mut ret = String::with_capacity(origin.len());
    let mut next_start = 0usize;
    for caps in RE.captures_iter(origin) {
        // copy everything from the original string up to the end of this match
        ret.push_str(&origin[next_start..caps.get(0).unwrap().start()]);
        next_start = caps.get(0).unwrap().end();

        ret.push({
            if let Some(bytecode) = caps[1].strip_prefix("u{").and_then(|s| s.strip_suffix('}')) {
                if !BYTECODE.is_match_at(bytecode, 0) {
                    return Err(CompilationError::BadEscapeSequence { sequence: caps[0].to_string() });
                }
                char::try_from(u32::from_str_radix(bytecode, 16).unwrap()).map_err(|_| {
                    CompilationError::BadEscapeSequence { sequence: caps[0].to_string() }
                })?
            } else {
                match &caps[1] {
                    "n" => '\n',
                    "t" => '\t',
                    "r" => '\r',
                    "0" => '\0',
                    "\\" => '\\',
                    "\"" => '"',
                    "'" => '\'',
                    _ => { return Err(CompilationError::BadEscapeSequence { sequence: caps[0].to_string() }); }
                }
            }
        })
    }
    ret.push_str(&origin[next_start..]);
    ret.shrink_to_fit();
    Ok(ret)
}

pub(crate) fn apply_brace_escape(origin: &str)->String{
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(\{)\{|(\})\}").unwrap();
    }
    RE.replace_all(&origin, r"$1$2").into_owned()
}

#[cfg(test)]
mod tests{
    use std::io::Stdout;
    use super::*;

    #[test]
    fn test_simple() {
        assert_eq!(apply_escapes::<Stdout>("abc").unwrap(), "abc")
    }

    #[test]
    fn test_complex() {
        assert_eq!(apply_escapes::<Stdout>(r"a\nb\'c").unwrap(), "a\nb\'c")
    }

    #[test]
    fn test_slashes() {
        assert_eq!(apply_escapes::<Stdout>(r"a\\b\\c").unwrap(), "a\\b\\c")
    }

    #[test]
    fn test_unicode() {
        assert_eq!(apply_escapes::<Stdout>(r"a\u{1Ab}b\u{0}c").unwrap(), "a\u{1Ab}b\0c")
    }

    #[test]
    fn test_braces_simple() {
        assert_eq!(apply_brace_escape("abc"), "abc")
    }

    #[test]
    fn test_braces_complex() {
        assert_eq!(apply_brace_escape("1 2 {{ a b }} 3 4"), "1 2 { a b } 3 4")
    }
}