use core::fmt;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Location {
    pub source: Option<PathBuf>,
    pub start: Span,
    pub end: Option<Span>,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:{}",
            self.source
                .as_ref()
                .map_or("<stdin>".to_string(), |p| p.to_string_lossy().to_string()),
            self.start
        )
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Span(i32, i32);

impl Span {
    pub fn new(begin: i32, end: i32) -> Self {
        Self(begin, end)
    }

    pub fn begin(&self) -> i32 {
        self.0
    }

    pub fn end(&self) -> i32 {
        self.1
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl From<Span> for (i32, i32) {
    fn from(value: Span) -> Self {
        (value.begin(), value.end())
    }
}

impl From<(i32, i32)> for Span {
    fn from(value: (i32, i32)) -> Self {
        Self(value.0, value.1)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use rstest::rstest;

    use crate::diagnostics::{Location, Span};

    #[rstest]
    #[case::start(Location {
                start: Span::new(1, 2),
                source: None,
                end: None,
            }, "<stdin>:1:2")]
    #[case::start_source(Location {
                start: Span::new(1, 2),
                source: Some(PathBuf::from("foo")),
                end: None,
            }, "foo:1:2")]
    #[case::start_source_end(Location {
                start: Span::new(1, 2),
                source: Some(PathBuf::from("foo")),
                end: Some(Span::new(3, 4)),
            }, "foo:1:2")]
    fn test_location_display(#[case] location: Location, #[case] expected: &str) {
        assert_eq!(location.to_string(), expected);
    }

    #[test]
    fn test_span_begin() {
        assert_eq!(Span::new(1, 2).begin(), 1);
    }

    #[test]
    fn test_span_end() {
        assert_eq!(Span::new(1, 2).end(), 2);
    }

    #[test]
    fn test_span_from_tuple() {
        let s: Span = (1, 2).into();
        assert_eq!(s, Span::new(1, 2));
    }

    #[test]
    fn test_span_into_tuple() {
        let t: (i32, i32) = Span(1, 2).into();
        assert_eq!(t, (1, 2));
    }
}
