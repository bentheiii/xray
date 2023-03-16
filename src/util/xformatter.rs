use regex::Regex;

#[derive(Clone, Copy)]
pub(crate) enum Alignment{
    Left,
    Center, 
    Right,
    RightWithSign,
}

impl From<&str> for Alignment{
    fn from(s: &str)->Self{
        match s{
            "<"=>Self::Left,
            ">"=>Self::Right,
            "^"=>Self::Center,
            "="=>Self::RightWithSign,
            _ => panic!()
        }
    }
}

pub(crate) enum SignMode{
    Positive,
    Negative,
    Whitespace,
}

impl From<&str> for SignMode{
    fn from(s: &str)->Self{
        match s{
            "+"=>Self::Positive,
            "-"=>Self::Negative,
            " "=>Self::Whitespace,
            _ => panic!()
        }
    }
}

pub(crate) struct FillSpecs<'a>{
    pub(crate) filler: Option<&'a str>,
    pub(crate) alignment: Option<Alignment>,
    pub(crate) zero_pad: bool,
    pub(crate) width: usize,
}

impl<'a> FillSpecs<'a>{
    pub(crate) fn get_filler(&self)->&'a str{
        self.filler.unwrap_or_else(|| if self.zero_pad{
            "0"
        } else {
            " "
        })
    }
    pub(crate) fn get_alignment(&self)->Alignment{
        self.alignment.unwrap_or_else(|| if self.zero_pad{
            Alignment::RightWithSign
        } else {
            Alignment::Right
        })
    }
}

#[derive(Default)]
pub(crate) struct FormattingType<'a>{
    pub(crate) type_: Option<&'a str>,
    pub(crate) alternative: bool,
}

#[derive(Default)]
pub(crate) struct XFormatting<'a>{
    pub(crate) fill_specs: Option<FillSpecs<'a>>,
    pub(crate) precision: Option<usize>,
    pub(crate) sign_mode: Option<SignMode>,
    pub(crate) grouping: Option<&'a str>,
    pub(crate) ty: FormattingType<'a>
}

impl<'a> XFormatting<'a>{
    pub(crate) fn from_str(s: &'a str)->Option<Self>{
        lazy_static! {
            static ref RE: Regex = Regex::new(r"(?x)
            ^
            ((?P<fill>.)?(?P<align>[<>=^]))?
            (?P<sign>[-+\ ])?
            (?P<alt>\#)?
            (?P<zero_pad>0)?
            (?P<width>[1-9][0-9]*)?
            (?P<grouping>[,_])?
            (?:\.(?P<precision>[1-9][0-9]*))?
            (?P<type>.)?
            $
            ").unwrap();
        }

        let captures = RE.captures(s)?;
        let fill_specs = {
            captures.name("width").and_then(|m| m.as_str().parse().ok()).map(|width|{
                let filler = captures.name("fill").map(|m| m.as_str());
                let alignment = captures.name("align").map(|m| m.as_str().into());
                let zero_pad = captures.name("zero_pad").is_some();
                FillSpecs{filler, alignment, zero_pad, width}
            })
        };
        let precision = captures.name("precision").and_then(|m| m.as_str().parse().ok());
        let sign_mode = captures.name("sign").map(|m| m.as_str().into());
        let grouping = captures.name("grouping").map(|m|m.as_str());
        let ty = {
            let type_ = captures.name("type").map(|m|m.as_str());
            let alternative = captures.name("alt").is_some();
            FormattingType { type_, alternative}
        };
        Some(Self{
            fill_specs,
            precision,
            sign_mode,
            grouping,
            ty
        })
    }

    pub(crate) fn min_width(&self)->usize{
        self.fill_specs.as_ref().map(|s| s.width).unwrap_or_default()
    }
}