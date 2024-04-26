use core::fmt;
use std::{fmt::Display, path::PathBuf};

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Default, Debug)]
pub struct FilePosition(u32, u32);

impl From<(u32, u32)> for FilePosition {
    fn from(value: (u32, u32)) -> Self {
        Self(value.0, value.1)
    }
}

impl From<FilePosition> for (u32, u32) {
    fn from(value: FilePosition) -> Self {
        (value.0, value.1)
    }
}

impl FilePosition {
    pub fn new(line: u32, column: u32) -> Self {
        Self(line, column)
    }

    pub fn line(&self) -> u32 {
        self.0
    }

    pub fn column(&self) -> u32 {
        self.1
    }

    pub fn get_offset(&self, source: &str) -> usize {
        let mut offset = 0usize;

        if self.line() == 0 {
            return 0;
        }

        for (line_number, line) in source.lines().enumerate() {
            if line_number + 1 == self.line() as usize {
                offset += self.column() as usize;
                return offset;
            }

            offset += line.len() + 1;
        }

        unreachable!("File position is not in the file: {self:?}")
    }
}

impl Display for FilePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line(), self.column())
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Default)]
pub struct Location {
    pub source: Option<PathBuf>,
    pub start: FilePosition,
    pub end: Option<FilePosition>,
}

impl Location {
    /// Retrive a `Range<usize>` representing the location to annotate in an error.
    ///
    /// # Panics
    /// This function is called for a location that references no source file.
    pub fn to_file_offset(&self, source: &str) -> std::ops::Range<usize> {
        let start_offset = self.start.get_offset(source);
        let end_offset = self.end.as_ref().unwrap_or(&self.start).get_offset(source);

        std::ops::Range {
            start: if start_offset > 0 {
                start_offset - 1
            } else {
                start_offset
            },
            end: if end_offset > 0 {
                end_offset - 1
            } else {
                end_offset
            },
        }
    }

    pub fn has_source(&self) -> bool {
        self.source.is_some()
    }
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

#[cfg(test)]
mod tests {
    use std::{ops::Range, path::PathBuf, str::FromStr};

    use bincode::{deserialize, serialize};
    use rstest::rstest;

    use crate::diagnostics::{FilePosition, Location};

    #[test]
    #[should_panic(
        expected = "internal error: entered unreachable code: File position is not in the file"
    )]
    fn test_file_position_get_offset_outside_file() {
        FilePosition::new(42, 42).get_offset("This source file is too short");
    }

    #[rstest]
    #[case::file_position_offset_zero(FilePosition::new(0, 0), "foo code", 0)]
    #[case::file_position_offset_line_zero(FilePosition::new(0, 2), "foo\ncode", 0)]
    #[case::file_position_offset_column_zero_first_line(FilePosition::new(1, 0), "foo\ncode", 0)]
    #[case::file_position_offset_column_zero_second_line(
        FilePosition::new(2, 0),
        "foo\nbar\nbaz",
        4
    )]
    #[case::file_position_single_line_offset(FilePosition::new(1, 5), "foo code", 5)]
    #[case::file_position_multiline_offset(
        FilePosition::new(2, 1),
        r"First
Second
Third",
        7
    )]
    fn test_file_position_get_offset(
        #[case] position: FilePosition,
        #[case] source_code: &str,
        #[case] expected_offset: usize,
    ) {
        assert_eq!(position.get_offset(source_code), expected_offset);
    }

    #[rstest]
    #[case::location_offset_zero(
        Location {
            source: None,
            start: FilePosition::new(0, 0),
            end: None
        },
        "foo code",
        0usize..0usize
    )]
    #[case::location_with_start_and_end(
        Location {
            source: None,
            start: FilePosition::new(1, 1),
            end: Some(FilePosition::new(1, 3))
        },
        "foo code",
        0usize..2usize
    )]
    fn test_location_to_file_offset(
        #[case] location: Location,
        #[case] source_code: &str,
        #[case] expected_offset: Range<usize>,
    ) {
        assert_eq!(location.to_file_offset(source_code), expected_offset);
    }

    #[rstest]
    #[case::location_start(Location {
                start: FilePosition::new(1, 2),
                source: None,
                end: None,
            }, "<stdin>:1:2")]
    #[case::location_start_source(Location {
                start: FilePosition::new(1, 2),
                source: Some(PathBuf::from("foo")),
                end: None,
            }, "foo:1:2")]
    #[case::location_start_source_end(Location {
                start: FilePosition::new(1, 2),
                source: Some(PathBuf::from("foo")),
                end: Some(FilePosition::new(3, 4)),
            }, "foo:1:2")]
    fn test_location_display(#[case] location: Location, #[case] expected: &str) {
        assert_eq!(location.to_string(), expected);
    }

    #[rstest]
    #[case::location_with_source(
        Location {
            source: Some(PathBuf::from_str("foo.rflx").expect("failed to create path")),
            start: FilePosition::new(1, 1),
            end: None
        },
        true
    )]
    #[case::location_without_source(
        Location {
            source: None,
            start: FilePosition::new(1, 1),
            end: None
        },
        false
    )]
    fn test_location_has_source(#[case] location: Location, #[case] has_source: bool) {
        assert_eq!(location.has_source(), has_source);
    }

    #[test]
    fn test_file_position_from_tuple() {
        let t: FilePosition = (1, 2).into();
        assert_eq!(t, FilePosition::new(1, 2));
    }

    #[test]
    fn test_file_position_into_tuple() {
        let t: (u32, u32) = FilePosition::new(1, 2).into();
        assert_eq!(t, (1, 2));
    }

    #[test]
    fn test_file_position_display() {
        assert_eq!(FilePosition::new(1, 2).to_string().as_str(), "1:2");
    }

    #[test]
    fn test_file_position_serde() {
        let position = FilePosition::new(1, 2);
        let ser_bytes = serialize(&position).expect("failed to serialize file position");
        let de_position = deserialize(&ser_bytes).expect("failed to deserialize file position");

        assert_eq!(position, de_position);
    }
}
