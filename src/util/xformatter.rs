use std::borrow::Cow;

use itertools::intersperse;
use regex::Regex;

use super::str_parts::StrParts;

#[derive(Clone, Copy)]
pub(crate) enum Alignment {
    Left,
    Center,
    Right,
    RightWithSign,
}

impl From<&str> for Alignment {
    fn from(s: &str) -> Self {
        match s {
            "<" => Self::Left,
            ">" => Self::Right,
            "^" => Self::Center,
            "=" => Self::RightWithSign,
            _ => panic!(),
        }
    }
}

pub(crate) enum SignMode {
    Positive,
    Negative,
    Whitespace,
}

impl From<&str> for SignMode {
    fn from(s: &str) -> Self {
        match s {
            "+" => Self::Positive,
            "-" => Self::Negative,
            " " => Self::Whitespace,
            _ => panic!(),
        }
    }
}

pub(crate) struct FillSpecs<'a> {
    pub(crate) filler: Option<&'a str>,
    pub(crate) alignment: Option<Alignment>,
    pub(crate) zero_pad: bool,
    pub(crate) width: usize,
}

impl<'a> FillSpecs<'a> {
    fn get_filler(&self) -> &'a str {
        self.filler.unwrap_or(if self.zero_pad { "0" } else { " " })
    }
    fn get_alignment(&self) -> Alignment {
        self.alignment.unwrap_or(if self.zero_pad {
            Alignment::RightWithSign
        } else {
            Alignment::Right
        })
    }

    pub(crate) fn fillers(&self, current_len: usize) -> (String, String, String) {
        let mut prefix = String::new();
        let mut infix = String::new();
        let mut postfix = String::new();
        if let Some(chars_to_pad) = self.width.checked_sub(current_len) {
            let fill_char = self.get_filler();
            match self.get_alignment() {
                Alignment::Left => {
                    postfix = fill_char.repeat(chars_to_pad);
                }
                Alignment::Right => {
                    prefix = fill_char.repeat(chars_to_pad);
                }
                Alignment::RightWithSign => {
                    infix = fill_char.repeat(chars_to_pad);
                }
                Alignment::Center => {
                    let mid = chars_to_pad / 2;
                    prefix = fill_char.repeat(mid);
                    postfix = fill_char.repeat(chars_to_pad - mid);
                }
            }
        }
        (prefix, infix, postfix)
    }
}

#[derive(Default)]
pub(crate) struct FormattingType<'a> {
    pub(crate) type_: Option<&'a str>,
    pub(crate) alternative: bool,
}

#[derive(Default)]
pub(crate) struct XFormatting<'a> {
    pub(crate) fill_specs: Option<FillSpecs<'a>>,
    pub(crate) precision: Option<usize>,
    pub(crate) sign_mode: Option<SignMode>,
    pub(crate) grouping: Option<&'a str>,
    pub(crate) ty: FormattingType<'a>,
}

pub(crate) fn group_str<'a>(s: &'a str, g: &'a str) -> StrParts<'a> {
    intersperse(
        s.as_bytes()
            .rchunks(3)
            .rev()
            .map(std::str::from_utf8)
            .collect::<Result<Vec<&str>, _>>()
            .unwrap()
            .into_iter(),
        g,
    )
    .collect()
}

pub(crate) fn group_string(s: String, g: &str) -> StrParts {
    intersperse(
        s.as_bytes()
            .rchunks(3)
            .rev()
            .map(|s| Cow::Owned(std::str::from_utf8(s).unwrap().to_string())),
        Cow::Borrowed(g),
    )
    .collect()
}

impl<'a> XFormatting<'a> {
    pub(crate) fn from_str(s: &'a str) -> Option<Self> {
        lazy_static! {
            static ref RE: Regex = Regex::new(
                r"(?x)
                ^
                ((?P<fill>.)?(?P<align>[<>=^]))?
                (?P<sign>[-+\ ])?
                (?P<alt>\#)?
                (?P<zero_pad>0)?
                (?P<width>[1-9][0-9]*)?
                (?P<grouping>[,_])?
                (?:\.(?P<precision>[0-9]*))?
                (?P<type>.)?
                $
                "
            )
            .unwrap();
        }

        let captures = RE.captures(s)?;
        let fill_specs = {
            captures
                .name("width")
                .and_then(|m| m.as_str().parse().ok())
                .map(|width| {
                    let filler = captures.name("fill").map(|m| m.as_str());
                    let alignment = captures.name("align").map(|m| m.as_str().into());
                    let zero_pad = captures.name("zero_pad").is_some();
                    FillSpecs {
                        filler,
                        alignment,
                        zero_pad,
                        width,
                    }
                })
        };
        if fill_specs
            .as_ref()
            .and_then(|f| f.filler)
            .map_or(false, |f| !f.is_ascii())
        {
            return None;
        }
        let precision = captures
            .name("precision")
            .and_then(|m| m.as_str().parse().ok());
        let sign_mode = captures.name("sign").map(|m| m.as_str().into());
        let grouping = captures.name("grouping").map(|m| m.as_str());
        if grouping.as_ref().map_or(false, |f| !f.is_ascii()) {
            return None;
        }
        let ty = {
            let type_ = captures.name("type").map(|m| m.as_str());
            let alternative = captures.name("alt").is_some();
            FormattingType { type_, alternative }
        };
        Some(Self {
            fill_specs,
            precision,
            sign_mode,
            grouping,
            ty,
        })
    }

    pub(crate) fn min_width(&self) -> usize {
        self.fill_specs
            .as_ref()
            .map(|s| s.width)
            .unwrap_or_default()
    }

    pub(crate) fn group(&self, s: &'a str) -> StrParts<'a> {
        if let Some(grouping) = self.grouping {
            group_str(s, grouping)
        } else {
            StrParts::from(s)
        }
    }

    pub(crate) fn sign(&self, is_negative: bool) -> StrParts<'a> {
        if is_negative {
            StrParts::from("-")
        } else if let Some(sign_mode) = &self.sign_mode {
            match sign_mode {
                SignMode::Positive => StrParts::from("+"),
                SignMode::Whitespace => StrParts::from(" "),
                _ => StrParts::default(),
            }
        } else {
            StrParts::default()
        }
    }
}
