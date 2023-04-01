use either::Either;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::mem::size_of;
use std::ops::Add;

#[derive(Default, Clone)]
pub struct FencedString {
    buffer: String,
    // as a special case, if the vec is empty, then all the bytes are different chars
    char_starts: Vec<usize>,
}

impl FencedString {
    pub(crate) fn from_string(s: String) -> Self {
        let mut char_starts = Vec::with_capacity(s.len());
        let mut char_indices = s.char_indices();
        char_starts.push(match char_indices.next() {
            None => return Default::default(),
            Some((first, _)) => first,
        });
        let mut is_not_ascii = false;
        for (start_ind, _) in char_indices {
            if char_starts[char_starts.len() - 1] + 1 < start_ind {
                is_not_ascii = true;
            }
            char_starts.push(start_ind);
        }
        Self {
            buffer: s,
            char_starts: if is_not_ascii {
                char_starts
            } else {
                Default::default()
            },
        }
    }

    pub(crate) fn from_str(s: &str) -> Self {
        Self::from_string(s.to_string())
    }

    pub(crate) fn substring(&self, start: usize, end: Option<usize>) -> Self {
        if self.char_starts.is_empty() {
            Self {
                buffer: (match end {
                    Some(end) if end < self.len()=> self.buffer[start..end].to_string(),
                    _ => self.buffer[start..].to_string(),
                }),
                char_starts: Vec::new(),
            }
        } else {
            let start_byte = self.char_starts[start];
            let end_byte = end.and_then(|e| self.char_starts.get(e)).cloned();
            if let Some(end_byte) = end_byte {
                Self {
                    buffer: self.buffer[start_byte..end_byte].to_string(),
                    char_starts: self.char_starts[start..end.unwrap()]
                        .iter()
                        .map(|i| i - start_byte)
                        .collect(),
                }
            } else {
                Self {
                    buffer: self.buffer[start_byte..].to_string(),
                    char_starts: self.char_starts[start..]
                        .iter()
                        .map(|i| i - start_byte)
                        .collect(),
                }
            }
        }
    }

    pub(crate) fn substr(&self, start: usize, end: Option<usize>) -> &str {
        if self.char_starts.is_empty() {
            match end {
                Some(end) if end < self.len() => &self.buffer[start..end],
                _ => &self.buffer[start..],
            }
        } else {
            let start_byte = self.char_starts[start];
            let end_byte = end.and_then(|e| self.char_starts.get(e)).cloned();
            if let Some(end_byte) = end_byte {
                &self.buffer[start_byte..end_byte]
            } else {
                &self.buffer[start_byte..]
            }
        }
    }

    pub(crate) fn len(&self) -> usize {
        if self.char_starts.is_empty() {
            self.buffer.len()
        } else {
            self.char_starts.len()
        }
    }

    pub(crate) fn bytes(&self) -> usize {
        self.buffer.len()
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.buffer
    }

    pub(crate) fn iter(&self) -> impl DoubleEndedIterator<Item = char> + '_ {
        self.buffer.chars()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    pub(crate) fn size(&self) -> usize {
        size_of::<Self>() + self.buffer.len() + self.char_starts.len() * size_of::<usize>()
    }

    pub(crate) fn push(&mut self, other: &Self) {
        let offset = self.buffer.len();
        self.buffer.push_str(other.as_str());
        if self.char_starts.is_empty() && other.char_starts.is_empty() {
            return;
        }
        let extension = if other.char_starts.is_empty() {
            Either::Left(offset..other.buffer.len() + offset)
        } else {
            Either::Right(other.char_starts.iter().map(|s| s + offset))
        };

        if self.char_starts.is_empty() {
            self.char_starts = (0..offset).chain(extension).collect()
        } else {
            self.char_starts.extend(extension)
        }
    }

    pub(crate) fn push_ascii(&mut self, other: &str) {
        let offset = self.buffer.len();
        self.buffer.push_str(other);
        if self.char_starts.is_empty() {
            return;
        }
        self.char_starts.extend(offset..other.len() + offset)
    }

    pub(crate) fn shrink_to_fit(&mut self) {
        self.buffer.shrink_to_fit();
        self.char_starts.shrink_to_fit();
    }

    pub(crate) fn to_lowercase(&self) -> Option<Self> {
        if self.buffer.chars().all(char::is_lowercase) {
            None
        } else {
            Some(Self {
                buffer: self.buffer.to_lowercase(),
                char_starts: self.char_starts.clone(),
            })
        }
    }

    pub(crate) fn to_uppercase(&self) -> Option<Self> {
        if self.buffer.chars().all(char::is_uppercase) {
            None
        } else {
            Some(Self::from_str(&self.buffer.to_uppercase()))
        }
    }
}

impl Debug for FencedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FencedString ({:?})", self.buffer)
    }
}

impl Display for FencedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.buffer, f)
    }
}

impl Hash for FencedString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.buffer.hash(state)
    }
}

impl PartialEq for FencedString {
    fn eq(&self, other: &Self) -> bool {
        self.buffer.eq(&other.buffer)
    }
}

impl Eq for FencedString {}

impl PartialOrd for FencedString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.buffer.partial_cmp(&other.buffer)
    }
}

impl Ord for FencedString {
    fn cmp(&self, other: &Self) -> Ordering {
        self.buffer.cmp(&other.buffer)
    }
}

impl Add for &FencedString {
    type Output = FencedString;
    fn add(self, rhs: Self) -> Self::Output {
        let mut ret = self.clone();
        ret.push(rhs);
        ret.shrink_to_fit();
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_str(s: &str) -> FencedString {
        let f = FencedString::from_string(s.to_string());
        let c = s.chars();

        assert_eq!(f.len(), c.clone().count());
        for start in 0..f.len() {
            assert_eq!(
                f.substr(start, None),
                c.clone().skip(start).collect::<String>()
            );
            assert_eq!(f.substring(start, None).as_str(), f.substr(start, None));
            for end in start..f.len()+10 {
                assert_eq!(
                    f.substr(start, Some(end)),
                    c.clone().take(end).skip(start).collect::<String>()
                );
                assert_eq!(
                    f.substring(start, Some(end)).as_str(),
                    f.substr(start, Some(end))
                );
            }
        }
        f
    }

    #[test]
    fn test_empty() {
        test_str("");
    }

    #[test]
    fn test_simple() {
        test_str("mary had a little lamb");
    }

    #[test]
    fn test_complex() {
        let f = test_str("rs🧡s💛dfdd💚d💙d💜d");
        assert_eq!(f.len(), 15);
        assert_eq!(f.substr(1, Some(4)), "s🧡s");
        assert_eq!(f.substr(12, Some(100)), "d💜d");
    }

    #[test]
    fn test_glyphs() {
        let f = test_str("y̆es");
        assert_eq!(f.len(), 4);
        assert_eq!(f.substr(1, Some(4)), "\u{0306}es")
    }

    #[test]
    fn test_add() {
        let arr = [
            "",
            "mary had a little lamb",
            "rs🧡s💛dfdd💚d💙d💜d",
            "🧡s💛dfdd💚d💙d💜d",
            "rs🧡s💛dfdd💚d💙d💜",
            "🧡s💛dfdd💚d💙d💜",
            "y̆es",
        ];
        for x in arr.iter() {
            for y in arr.iter() {
                let z1 = FencedString::from_string(format!("{x}{y}"));
                let x = FencedString::from_string(x.to_string());
                let y = FencedString::from_string(y.to_string());

                let z0 = &x + &y;

                assert_eq!(z0.buffer, z1.buffer);
                assert_eq!(z0.char_starts, z1.char_starts);
            }
        }
    }

    #[test]
    fn test_add_ascii() {
        let arr = [
            "",
            "mary had a little lamb",
            "rs🧡s💛dfdd💚d💙d💜d",
            "🧡s💛dfdd💚d💙d💜d",
            "rs🧡s💛dfdd💚d💙d💜",
            "🧡s💛dfdd💚d💙d💜",
            "y̆es",
        ];
        let ascii_arr = ["hi", ""];
        for x in arr.iter() {
            for y in ascii_arr.iter() {
                let z1 = FencedString::from_string(format!("{x}{y}"));
                let mut z0 = FencedString::from_string(x.to_string());
                z0.push_ascii(y);

                assert_eq!(z0.buffer, z1.buffer);
                assert_eq!(z0.char_starts, z1.char_starts);
            }
        }
    }
}
