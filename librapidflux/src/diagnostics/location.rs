use core::fmt;
use std::{fmt::Display, path::PathBuf};

use serde::{Deserialize, Serialize};

pub const NO_SOURCE: &str = "<unknown>";
pub const STDIN_SOURCE: &str = "<stdin>";

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
pub enum Location {
    #[default]
    None,
    Stdin {
        start: FilePosition,
        end: FilePosition,
    },
    File {
        start: FilePosition,
        end: FilePosition,
        source: PathBuf,
    },
}

impl Location {
    /// Creates a new location.
    ///
    /// # Panics
    ///
    /// This function will panic if the end is before the start.
    pub fn new(start: FilePosition, end: FilePosition, source: Option<PathBuf>) -> Self {
        assert!(start <= end);
        assert!(
            source.is_none()
                || matches!(source, Some(ref s) if s.to_string_lossy() != STDIN_SOURCE && s.to_string_lossy() != NO_SOURCE)
        );
        if let Some(source) = source {
            Location::File { start, end, source }
        } else {
            Location::Stdin { start, end }
        }
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
    /// This function will panic if attempting to merge locations with and without a source file.
    pub fn merge(locations: &[Self]) -> Self {
        || -> Option<_> {
            let known_locations = locations
                .iter()
                .filter(|l| **l != Location::None)
                .collect::<Vec<_>>();

            assert!(
                known_locations
                    .iter()
                    .all(|l| matches!(l, Location::Stdin { .. }))
                    || known_locations
                        .iter()
                        .all(|l| matches!(l, Location::File { .. })),
                "attempted to merge locations with and without source file"
            );

            let first_location = known_locations.first()?;
            let filter_first_path = |l: &&&Location| match (l, first_location) {
                (Location::Stdin { .. }, Location::Stdin { .. }) => true,
                (
                    Location::File {
                        source: l_source, ..
                    },
                    Location::File {
                        source: first_source,
                        ..
                    },
                ) => l_source == first_source,
                _ => unreachable!(),
            };
            let filtered_locations = known_locations
                .iter()
                .filter(filter_first_path)
                .collect::<Vec<_>>();

            let get_start = |l: &&&Location| match l {
                Location::Stdin { start, .. } | Location::File { start, .. } => *start,
                Location::None => unreachable!(),
            };
            let get_end = |l: &&&Location| match l {
                Location::Stdin { end, .. } | Location::File { end, .. } => *end,
                Location::None => unreachable!(),
            };

            let min_loc = filtered_locations
                .iter()
                .map(get_start)
                .chain(filtered_locations.iter().map(get_end))
                .min()?;
            let max_loc = filtered_locations
                .iter()
                .map(get_start)
                .chain(filtered_locations.iter().map(get_end))
                .max()
                .expect("unreachable");

            match first_location {
                Location::Stdin { .. } => Some(Location::Stdin {
                    start: min_loc,
                    end: max_loc,
                }),
                Location::File { source, .. } => Some(Location::File {
                    start: min_loc,
                    end: max_loc,
                    source: source.clone(),
                }),
                Location::None => unreachable!(),
            }
        }()
        .unwrap_or(Location::None)
    }

    /// Retrieve a `Range<usize>` representing the location to annotate in an error.
    ///
    /// # Panics
    ///
    /// This function will panic if it is called for `Location::None`.
    pub fn to_file_offset(&self, source: &str) -> std::ops::Range<usize> {
        match self {
            Location::Stdin { start, end, .. } | Location::File { start, end, .. } => {
                let start_offset = start.get_offset(source);
                let end_offset = end.get_offset(source);

                std::ops::Range {
                    start: start_offset - 1,
                    end: end_offset - 1,
                }
            }
            Location::None => {
                panic!("attempted to get file offset without location")
            }
        }
    }

    pub fn has_source(&self) -> bool {
        matches!(self, Location::File { .. })
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Location::None => write!(f, "{NO_SOURCE}"),
            Location::Stdin { start, .. } => write!(f, "{STDIN_SOURCE}:{start}"),
            Location::File { start, source, .. } => {
                write!(f, "{}:{start}", source.to_string_lossy())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{ops::Range, path::PathBuf};

    use bincode::{deserialize, serialize};
    use rstest::rstest;

    use crate::diagnostics::location::{FilePosition, Location};

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
        Location::Stdin {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 3)
        },
        "foo code",
        0usize..2usize
    )]
    #[should_panic(expected = "attempted to get file offset without location")]
    #[case::no_location(
        Location::None,
        "foo code",
        0usize..0usize
    )]
    fn test_location_to_file_offset(
        #[case] location: Location,
        #[case] source_code: &str,
        #[case] expected_offset: Range<usize>,
    ) {
        assert_eq!(location.to_file_offset(source_code), expected_offset);
    }

    #[rstest]
    #[case::location_none(Location::None, "<unknown>")]
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
            Some(PathBuf::from("foo.rflx")),
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
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
            },
            Location::Stdin {
                start: FilePosition::new(3, 1),
                end: FilePosition::new(3, 10),
            }
        ],
        Location::Stdin {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(3, 10),
        },
    )]
    #[case::location_same_line_with_end(
        &[
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
            },
            Location::Stdin {
                start: FilePosition::new(1, 4),
                end: FilePosition::new(1, 27),
            }
        ],
        Location::Stdin {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 27),
        },
    )]
    #[case::location_overlap(
        &[
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
            },
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(28, 4),
            },
            Location::Stdin {
                start: FilePosition::new(1, 4),
                end: FilePosition::new(1, 27),
            }
        ],
        Location::Stdin {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(28, 4),
        },
    )]
    #[case::one_element(
        &[
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
            },
        ],
        Location::Stdin {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 10),
        },
    )]
    #[case::files(
        &[
            Location::File {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                source: PathBuf::from("foo.rflx")
            },
            Location::File {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                source: PathBuf::from("foo.rflx")
            },
        ],
        Location::File {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 10),
            source: PathBuf::from("foo.rflx")
        },
    )]
    #[case::files_with_mixed_sources(
        &[
            Location::File {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 17),
                source: PathBuf::from("foo.rflx")
            },
            Location::File {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                source: PathBuf::from("bar.rflx")
            },
        ],
        Location::File {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(1, 17),
            source: PathBuf::from("foo.rflx")
        },
    )]
    #[should_panic(expected = "attempted to merge locations with and without source file")]
    #[case::stdin_and_file(
        &[
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 17),
            },
            Location::File {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 10),
                source: PathBuf::from("bar.rflx")
            },
        ],
        Location::None,
    )]
    #[case::none_and_file(
        &[
            Location::None,
            Location::File {
                start: FilePosition::new(1, 4),
                end: FilePosition::new(1, 10),
                source: PathBuf::from("bar.rflx")
            },
            Location::None,
        ],
        Location::File {
            start: FilePosition::new(1, 4),
            end: FilePosition::new(1, 10),
            source: PathBuf::from("bar.rflx")
        },
    )]
    #[case::none(
        &[
            Location::None,
        ],
        Location::None,
    )]
    #[case::no_elements(&[], Location::None)]
    fn test_location_merge(#[case] locations: &[Location], #[case] expected: Location) {
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
