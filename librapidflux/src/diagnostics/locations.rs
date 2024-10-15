use core::fmt;
use std::{fmt::Display, path::PathBuf};

use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};

lazy_static! {
    pub static ref UNKNOWN_LOCATION: Location = Location::new(
        FilePosition(1, 1),
        FilePosition(1, 1),
        Some(PathBuf::from("<unknown>"))
    );
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Default, Debug)]
pub struct FilePosition(u32, u32);

impl From<(u32, u32)> for FilePosition {
    fn from(value: (u32, u32)) -> Self {
        assert_ne!(value.0, 0);
        assert_ne!(value.1, 0);

        Self(value.0, value.1)
    }
}

impl From<FilePosition> for (u32, u32) {
    fn from(value: FilePosition) -> Self {
        assert_ne!(value.0, 0);
        assert_ne!(value.1, 0);

        (value.0, value.1)
    }
}

impl FilePosition {
    /// Creates a new instance with the specified line and column numbers.
    ///
    /// # Panics
    ///
    /// The function will panic if either `line` or `column` is zero.
    ///
    pub fn new(line: u32, column: u32) -> Self {
        assert_ne!(line, 0);
        assert_ne!(column, 0);

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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Default)]
pub struct Location {
    pub start: FilePosition,
    pub end: FilePosition,
    pub source: Option<PathBuf>,
}

impl Location {
    /// Creates a new location.
    ///
    /// # Panics
    ///
    /// This function will panic if the end is before the start.
    pub fn new(start: FilePosition, end: FilePosition, source: Option<PathBuf>) -> Self {
        assert!(start <= end);
        Location { start, end, source }
    }

    /// Merges a list of locations into a single location.
    ///
    /// This function takes a slice of locations and calculates the smallest starting location
    /// and the largest ending location among them. It then returns a new location that spans
    /// from the smallest starting location to the largest ending location, including
    /// the source file information from the smallest starting location. In case locations refer
    /// to different source files, only locations referring to the same source file as the first
    /// location in the slice are considered.
    ///
    /// # Panics
    ///
    /// This function panics if not all locations refer to the same source file.
    pub fn merge(locations: &[Self]) -> Option<Self> {
        assert!(
            locations
                .iter()
                .all(|l| l == &*UNKNOWN_LOCATION || l.source.is_none())
                || locations.iter().all(|l| l.source.is_some()),
            "attempted to merge locations with and without source file"
        );
        let first_location = locations.first()?;
        let filter_first_path = |l: &&Location| l.source.as_ref() == first_location.source.as_ref();

        let min_loc = locations
            .iter()
            .filter(filter_first_path)
            .map(|l| l.start)
            .chain(locations.iter().filter(filter_first_path).map(|l| l.end))
            .min()?;

        let max_loc = locations
            .iter()
            .filter(filter_first_path)
            .map(|l| l.start)
            .chain(locations.iter().filter(filter_first_path).map(|l| l.end))
            .max()
            .expect("unreachable");

        Some(Self {
            start: min_loc,
            end: max_loc,
            source: locations.first()?.source.clone(),
        })
    }

    /// Retrieve a `Range<usize>` representing the location to annotate in an error.
    ///
    /// # Panics
    /// This function is called for a location that references no source file.
    pub fn to_file_offset(&self, source: &str) -> std::ops::Range<usize> {
        let start_offset = self.start.get_offset(source);
        let end_offset = self.end.get_offset(source);

        std::ops::Range {
            start: start_offset - 1,
            end: end_offset - 1,
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

    use crate::diagnostics::{FilePosition, Location, UNKNOWN_LOCATION};

    #[test]
    #[should_panic(
        expected = "internal error: entered unreachable code: File position is not in the file"
    )]
    fn test_file_position_get_offset_outside_file() {
        FilePosition::new(42, 42).get_offset("This source file is too short");
    }

    #[rstest]
    #[case::file_position_offset_column_one_first_line(FilePosition::new(1, 1), "foo\ncode", 1)]
    #[case::file_position_offset_column_one_second_line(
        FilePosition::new(2, 1),
        "foo\nbar\nbaz",
        5
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
    #[case::location_with_start_and_end(
        Location {
            source: None,
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 3)
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
    #[case::location_start(
        Location::new(FilePosition::new(1, 2), FilePosition::new(1, 2), None),
        "<stdin>:1:2"
    )]
    #[case::location_start_source(
        Location::new(
            FilePosition::new(1, 2),
            FilePosition::new(1, 2),
            Some(PathBuf::from("foo")),
        ),
        "foo:1:2"
    )]
    #[case::location_start_source_end(
        Location::new(
            FilePosition::new(1, 2),
            FilePosition::new(3, 4),
            Some(PathBuf::from("foo")),
        ),
        "foo:1:2"
    )]
    fn test_location_display(#[case] location: Location, #[case] expected: &str) {
        assert_eq!(location.to_string(), expected);
    }

    #[rstest]
    #[case::location_with_source(
        Location::new(
            FilePosition::new(1, 1),
            FilePosition::new(1, 1),
            Some(PathBuf::from_str("foo.rflx").expect("failed to create path")),
        ),
        true
    )]
    #[case::location_without_source(
        Location::new(FilePosition::new(1, 1), FilePosition::new(1, 1), None),
        false
    )]
    fn test_location_has_source(#[case] location: Location, #[case] has_source: bool) {
        assert_eq!(location.has_source(), has_source);
    }

    #[rstest]
    #[case::location_different_line_with_end(
        &[
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                ..Location::default()
            },
            Location {
                start: FilePosition::new(3, 1),
                end: FilePosition::new(3, 10),
                ..Default::default()
            }
        ],
        Some(Location {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(3, 10),
            ..Default::default()
        }),
    )]
    #[case::location_same_line_with_end(
        &[
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                ..Location::default()
            },
            Location {
                start: FilePosition::new(1, 4),
                end: FilePosition::new(1, 27),
                ..Default::default()
            }
        ],
        Some(Location {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 27),
            ..Default::default()
        }),
    )]
    #[case::location_overlap(
        &[
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                ..Location::default()
            },
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(28, 4),
                ..Location::default()
            },
            Location {
                start: FilePosition::new(1, 4),
                end: FilePosition::new(1, 27),
                ..Default::default()
            }
        ],
        Some(Location {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(28, 4),
            ..Default::default()
        }),
    )]
    #[case::location_one_element(
        &[
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                ..Location::default()
            },
        ],
        Some(Location {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 10),
            ..Default::default()
        }),
    )]
    #[case::location_with_source(
        &[
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                source: Some(PathBuf::from_str("foo.rflx").expect("failed to create path"))
            },
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                source: Some(PathBuf::from_str("foo.rflx").expect("failed to create path"))
            },
        ],
        Some(Location {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 10),
            source: Some(PathBuf::from_str("foo.rflx").expect("failed to create path"))
        }),
    )]
    #[case::location_merge_first_filename(
        &[
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 17),
                source: Some(PathBuf::from_str("foo.rflx").expect("failed to create path"))
            },
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                source: Some(PathBuf::from_str("bar.rflx").expect("failed to create path"))
            },
        ],
        Some(Location {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 17),
            source: Some(PathBuf::from_str("foo.rflx").expect("failed to create path"))
        }),
    )]
    #[should_panic(expected = "attempted to merge locations with and without source file")]
    #[case::location_merge_source_present_and_absent(
        &[
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 17),
                source: None,
            },
            Location {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                source: Some(PathBuf::from_str("bar.rflx").expect("failed to create path"))
            },
        ],
        None,
    )]
    #[case::location_no_elements(&[], None)]
    fn test_location_merge(#[case] locations: &[Location], #[case] expected: Option<Location>) {
        assert_eq!(Location::merge(locations), expected);
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

    #[rstest]
    #[case::file_position_lt(FilePosition::new(1, 2) < FilePosition::new(2, 3))]
    #[case::file_position_gt(FilePosition::new(3, 2) > FilePosition::new(1, 3))]
    fn test_file_position_comparison(#[case] comparison: bool) {
        assert!(comparison);
    }
}
