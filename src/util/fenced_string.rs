use either::Either;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Deref, Index};
use std::slice::SliceIndex;

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

    pub(crate) fn substring(&self, start: usize, end: Option<usize>) -> Self {
        if self.char_starts.is_empty() {
            Self {
                buffer: (if let Some(end) = end {
                    self.buffer[start..end].to_string()
                } else {
                    self.buffer[start..].to_string()
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
            (if let Some(end) = end {
                &self.buffer[start..end]
            } else {
                &self.buffer[start..]
            })
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

    pub(crate) fn to_string(&self) -> String {
        self.buffer.clone()
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
        let mut s = String::with_capacity(self.bytes() + rhs.bytes());
        s.push_str(self.as_str());
        s.push_str(rhs.as_str());

        let starts = if self.char_starts.is_empty() && rhs.char_starts.is_empty() {
            Vec::new()
        } else {
            let offset = self.buffer.len();
            (if self.char_starts.is_empty() {
                Either::Left(0..self.buffer.len())
            } else {
                Either::Right(self.char_starts.iter().cloned())
            })
            .chain(if rhs.char_starts.is_empty() {
                Either::Left(offset..rhs.buffer.len() + offset)
            } else {
                Either::Right(rhs.char_starts.iter().map(|s| s + offset))
            })
            .collect()
        };
        FencedString {
            buffer: s,
            char_starts: starts,
        }
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
            for end in start..f.len() {
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
        assert_eq!(f.substr(1, Some(4)), "s🧡s")
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
}