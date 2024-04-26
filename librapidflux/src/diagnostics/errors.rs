use std::{
    fmt::{Debug, Display},
    fs,
    io::{self, Write},
    path::PathBuf,
    str::FromStr,
};

#[cfg(not(test))]
use annotate_snippets::renderer::{Color, RgbColor, Style};
use serde::{Deserialize, Serialize};

use super::Location;

#[cfg(not(test))]
const RENDERER: annotate_snippets::Renderer = annotate_snippets::Renderer::styled()
    .error(Style::new().fg_color(Some(Color::Rgb(RgbColor(225, 0, 0)))))
    .help(Style::new().fg_color(Some(Color::Rgb(RgbColor(0, 80, 200)))))
    .note(Style::new().fg_color(Some(Color::Rgb(RgbColor(180, 180, 0)))));

#[cfg(test)]
const RENDERER: annotate_snippets::Renderer = annotate_snippets::Renderer::plain();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum Severity {
    #[default]
    Info,
    Warning,
    Error,
    Help,
    Note,
}

impl Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Info => write!(f, "info"),
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
            Severity::Help => write!(f, "help"),
            Severity::Note => write!(f, "note"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Annotation {
    severity: Severity,
    location: Location,
    label: Option<String>,
}

impl Display for Annotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {}: {}",
            self.location,
            self.severity,
            self.label.as_ref().map_or("", std::string::String::as_str)
        )
    }
}

impl Annotation {
    pub fn new(severity: Severity, location: Location, label: Option<String>) -> Self {
        Self {
            severity,
            location,
            label,
        }
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn location(&self) -> &Location {
        &self.location
    }

    pub fn label(&self) -> Option<&str> {
        self.label.as_deref()
    }

    fn to_annotation<'a>(&'a self, source: &'a str) -> annotate_snippets::Annotation<'a> {
        let file_offset = self.location.to_file_offset(source);
        let annotation = match self.severity {
            Severity::Info => annotate_snippets::Level::Info.span(file_offset),
            Severity::Warning => annotate_snippets::Level::Warning.span(file_offset),
            Severity::Error => annotate_snippets::Level::Error.span(file_offset),
            Severity::Help => annotate_snippets::Level::Help.span(file_offset),
            Severity::Note => annotate_snippets::Level::Note.span(file_offset),
        };

        if let Some(label) = self.label() {
            annotation.label(label)
        } else {
            annotation
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct ErrorEntry {
    message: String,
    severity: Severity,
    location: Option<Location>,
    annotations: Vec<Annotation>,
}

impl Display for ErrorEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(location) = self.location.as_ref() {
            write!(f, "{location}: ")?;
        }

        write!(f, "{}: {}", self.severity, self.message)?;

        for annotation in self.annotations.iter().filter(|a| a.label.is_some()) {
            writeln!(f)?;
            write!(f, "{annotation}")?;
        }

        Ok(())
    }
}

impl ErrorEntry {
    pub fn new(
        message: String,
        severity: Severity,
        location: Option<Location>,
        annotations: Vec<Annotation>,
    ) -> Self {
        Self {
            message,
            severity,
            location,
            annotations,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn location(&self) -> Option<&Location> {
        self.location.as_ref()
    }

    pub fn annotations(&self) -> &[Annotation] {
        &self.annotations
    }

    /// Convert an error entry to an `annotate_snippets`' snippet message
    ///
    /// # Parameters
    /// - `source`: Source code string that caused the error
    pub(crate) fn to_message_mut<'src>(
        &'src mut self,
        source: &'src str,
    ) -> Option<annotate_snippets::Message<'src>> {
        let message = match self.severity {
            Severity::Info => annotate_snippets::Level::Info.title(&self.message),
            Severity::Warning => annotate_snippets::Level::Warning.title(&self.message),
            Severity::Error => annotate_snippets::Level::Error.title(&self.message),
            Severity::Help => annotate_snippets::Level::Help.title(&self.message),
            Severity::Note => annotate_snippets::Level::Note.title(&self.message),
        };

        if let Some(location) = self.location.as_ref() {
            let default_annotation = Annotation::new(self.severity, location.clone(), None);

            // Add squiggles below the actual error. Without this, the user won't be able to
            // see the error location (e.g. `foo.rflx:3:4`).
            self.annotations.push(default_annotation);
        }

        if self.annotations.is_empty()
            || self.location.as_ref().is_some_and(|l| {
                l.source
                    .as_ref()
                    .is_some_and(|s| s == &PathBuf::from_str("<stdin>").expect("unreachable"))
            })
        {
            return None;
        }

        let snippet = annotate_snippets::Snippet::source(source)
            .fold(true)
            .annotations(self.annotations.iter().map(|a| a.to_annotation(source)));

        Some(message.snippet(
            if let Some(Some(source_file)) = self.location.as_ref().map(|l| l.source.as_ref()) {
                snippet.origin(source_file.to_str().unwrap_or("<unknown>"))
            } else {
                snippet
            },
        ))
    }
}

#[derive(Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct RapidFluxError {
    entries: Vec<ErrorEntry>,
}

impl Debug for RapidFluxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.entries)
    }
}

impl Display for RapidFluxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.entries
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl From<Vec<ErrorEntry>> for RapidFluxError {
    fn from(entries: Vec<ErrorEntry>) -> Self {
        Self { entries }
    }
}

impl RapidFluxError {
    /// Push a new error entry
    pub fn push(&mut self, entry: ErrorEntry) {
        self.entries.push(entry);
    }

    /// Extend error collection from an iterator. Takes the entries' ownership
    pub fn extend<T: IntoIterator<Item = ErrorEntry>>(&mut self, entries: T) {
        self.entries.extend(entries);
    }

    pub fn clear(&mut self) {
        self.entries.clear();
    }

    pub fn entries(&self) -> &[ErrorEntry] {
        &self.entries
    }

    /// Print all messages to `stdout`
    ///
    /// # Errors
    /// Source code needs to be retrieved and error message displayed. This function
    /// might return an `io::Error` if any io operation failed.
    pub fn print_messages<T: Write>(&mut self, stream: &mut T) -> io::Result<()> {
        for entry in &mut self.entries {
            let source_code =
                if let Some(Some(source_path)) = entry.location.as_ref().map(|l| &l.source) {
                    if source_path.to_str().unwrap_or_default() == "<stdin>" {
                        String::new()
                    } else {
                        // TODO(eng/recordflux/RecordFlux#1602): Use stored source code instead of reading file
                        fs::read_to_string(source_path)?
                    }
                } else {
                    String::new()
                };

            match entry.to_message_mut(&source_code) {
                Some(msg) => writeln!(stream, "{}", RENDERER.render(msg))?,
                None => writeln!(stream, "{entry}")?,
            }
        }

        Ok(())
    }

    pub fn has_errors(&self) -> bool {
        self.entries.iter().any(|e| e.severity == Severity::Error)
    }
}

#[cfg(test)]
mod tests {
    use std::{io, path::PathBuf, str::FromStr};

    use indoc::indoc;
    use rstest::rstest;

    use crate::diagnostics::{ErrorEntry, FilePosition, Location};

    use super::{Annotation, RapidFluxError, Severity};

    #[rstest]
    #[case::severity_note(Severity::Note, "note")]
    #[case::severity_info(Severity::Info, "info")]
    #[case::severity_warning(Severity::Warning, "warning")]
    #[case::severity_error(Severity::Error, "error")]
    #[case::severity_help(Severity::Help, "help")]
    fn test_severity_display(#[case] severity: Severity, #[case] expected_str: &str) {
        assert_eq!(severity.to_string(), expected_str);
    }

    #[rstest]
    #[case::annotation_without_label(
        Annotation::new(
            Severity::Error,
            Location {
                source: Some(PathBuf::from_str("foo.rflx")
                    .expect("failed to create source path")),
                start: FilePosition::new(1, 1),
                end: None,
            },
            None,
        ),
        "foo.rflx:1:1: error: "
    )]
    #[case::annotation_with_source_path(
        Annotation::new(
            Severity::Error,
            Location {
                source: Some(PathBuf::from_str("foo.rflx")
                    .expect("failed to create source path")),
                start: FilePosition::new(1, 1),
                end: None,
            },
            Some("some. terrible. error".to_string()),
        ),
        "foo.rflx:1:1: error: some. terrible. error"
    )]
    #[case::annotation_without_source_path(
        Annotation::new(
            Severity::Error,
            Location {
                source: None,
                start: FilePosition::new(1, 1),
                end: None,
            },
            Some("some. terrible. error".to_string()),
        ),
        "<stdin>:1:1: error: some. terrible. error"
    )]
    fn test_annotation_display(#[case] annotation: Annotation, #[case] expected_str: &str) {
        assert_eq!(annotation.to_string().as_str(), expected_str,);
    }

    #[rstest]
    #[case::annotation_severity_note(Severity::Note, "Note", None)]
    #[case::annotation_severity_info(Severity::Info, "Info", None)]
    #[case::annotation_severity_warn(Severity::Warning, "Warning", None)]
    #[case::annotation_severity_error(Severity::Error, "Error", None)]
    #[case::annotation_severity_help(Severity::Help, "Help", None)]
    #[case::annotation_severity_error_with_label(
        Severity::Error,
        "Error",
        Some("label".to_string())
    )]
    fn test_annotation_to_annotate_snippets(
        #[case] severity: Severity,
        #[case] severity_str: &str,
        #[case] label: Option<String>,
    ) {
        let annotation = Annotation::new(
            severity,
            Location {
                source: Some(PathBuf::from_str("foo.rflx").expect("failed to create source path")),
                start: FilePosition::new(1, 1),
                end: Some(FilePosition::new(1, 5)),
            },
            label.clone(),
        );
        let as_annotation = annotation.to_annotation("some amazing source code");
        assert_eq!(
            format!("{as_annotation:?}"),
            format!(
                "Annotation {{ range: 0..4, label: {}, level: {severity_str} }}",
                match label {
                    Some(str) => format!("Some(\"{str}\")"),
                    None => "None".to_string(),
                }
            )
        );
    }

    #[test]
    fn test_annotation_creation() {
        let annotation = Annotation::new(
            Severity::Error,
            Location {
                source: None,
                start: FilePosition::new(1, 1),
                end: None,
            },
            Some("label".to_string()),
        );

        assert_eq!(
            annotation.location(),
            &Location {
                source: None,
                start: FilePosition::new(1, 1),
                end: None,
            }
        );

        assert_eq!(annotation.severity(), Severity::Error);
        assert_eq!(annotation.label().expect("should be present"), "label");
    }

    #[test]
    fn test_error_entry_creation() {
        let error_entry = ErrorEntry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            None,
            Vec::new(),
        );

        assert_eq!(error_entry.severity(), Severity::Error);
        assert_eq!(error_entry.message(), "Some terrible error");
        assert!(error_entry.location().is_none());
        assert!(error_entry.annotations().is_empty());
    }

    #[rstest]
    #[case::error_entry_no_location(
        ErrorEntry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            None,
            Vec::new(),
        ),
        "Some cool source code",
        "error: Some terrible error",
    )]
    #[case::error_entry_severity_info(
        ErrorEntry::new(
            "info".to_string(),
            Severity::Info,
            None,
            Vec::new(),
        ),
        "",
        "info: info",
    )]
    #[case::error_entry_severity_help(
        ErrorEntry::new(
            "help".to_string(),
            Severity::Help,
            None,
            Vec::new(),
        ),
        "",
        "help: help",
    )]
    #[case::error_entry_severity_warning(
        ErrorEntry::new(
            "warning".to_string(),
            Severity::Warning,
            None,
            Vec::new(),
        ),
        "",
        "warning: warning",
    )]
    #[case::error_entry_severity_note(
        ErrorEntry::new(
            "note".to_string(),
            Severity::Note,
            None,
            Vec::new(),
        ),
        "",
        "note: note",
    )]
    #[case::error_entry_with_source_file(
        ErrorEntry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Some(Location {
                source: None,
                start: FilePosition::new(1, 1),
                end: Some(FilePosition::new(1, 8)),
            }),
            Vec::new(),
        ),
        "package Test is end Test;",
        indoc! {
            r"error: Some terrible error
                |
              1 | package Test is end Test;
                | ^^^^^^^
                |"
        },
    )]
    #[case::error_entry_with_location_and_source_file(
        ErrorEntry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Some(Location {
                source: Some(PathBuf::from_str("test.rflx").expect("failed to create path")),
                start: FilePosition::new(1, 1),
                end: Some(FilePosition::new(1, 8)),
            }),
            Vec::new(),
        ),
        "package Test is end Test;",
        indoc! {
            r"error: Some terrible error
               --> test.rflx:1:1
                |
              1 | package Test is end Test;
                | ^^^^^^^
                |"
        },
    )]
    #[case::error_entry_with_location_and_source_file(
        ErrorEntry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Some(Location {
                source: Some(PathBuf::from_str("test.rflx").expect("failed to create path")),
                start: FilePosition::new(1, 1),
                end: Some(FilePosition::new(1, 8)),
            }),
            vec![
                Annotation::new(
                    Severity::Help,
                    Location {
                        source: None,
                        start: FilePosition::new(2, 1),
                        end: Some(FilePosition::new(2, 4)),
                    },
                Some("some help".to_string())
            )
        ]),
        indoc! {
            r"package Test is
              end Test;"
        },
        indoc! {
            r"error: Some terrible error
               --> test.rflx:2:1
                |
              1 | package Test is
                | ^^^^^^^
              2 | end Test;
                | --- help: some help
                |"
        },
    )]
    fn test_error_entry_to_message(
        #[case] mut entry: ErrorEntry,
        #[case] source_code: &str,
        #[case] expected_str: &str,
    ) {
        use crate::diagnostics::errors::RENDERER;

        match entry.to_message_mut(source_code) {
            Some(msg) => {
                let str = RENDERER.render(msg).to_string();
                assert_eq!(str.as_str(), expected_str);
            }
            None => assert_eq!(entry.to_string().as_str(), expected_str),
        };
    }

    #[rstest]
    #[case::error_entry_with_location_no_source(
        ErrorEntry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Some(Location {
                source: None,
                start: FilePosition::new(1, 1),
                end: Some(FilePosition::new(1, 8)),
            }),
            Vec::new(),
        ),
        "<stdin>:1:1: error: Some terrible error"
    )]
    #[case::error_entry_with_location_and_source(
        ErrorEntry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Some(Location {
                source: Some(PathBuf::from_str("foo.rflx").expect("failed to create path")),
                start: FilePosition::new(1, 1),
                end: Some(FilePosition::new(1, 8)),
            }),
            Vec::new(),
        ),
        "foo.rflx:1:1: error: Some terrible error"
    )]
    #[case::error_entry_with_annotations(
        ErrorEntry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Some(Location {
                source: Some(PathBuf::from_str("foo.rflx").expect("failed to create path")),
                start: FilePosition::new(1, 1),
                end: Some(FilePosition::new(1, 8)),
            }),
            vec![
                Annotation {
                    severity: Severity::Info,
                    location: Location {
                        source: Some(PathBuf::from_str("foo.rflx").expect("failed to create path")),
                        start: FilePosition::new(1, 1),
                        end: Some(FilePosition::new(1, 8)),
                    },
                    label: Some("some label".to_string())
                }
            ],
        ),
        indoc! {
            r"foo.rflx:1:1: error: Some terrible error
              foo.rflx:1:1: info: some label"
        }
    )]
    fn test_error_entry_display(#[case] error_entry: ErrorEntry, #[case] expected_str: &str) {
        assert_eq!(error_entry.to_string().as_str(), expected_str);
    }

    #[test]
    fn test_rapid_flux_error_has_error() {
        let mut error = RapidFluxError::default();
        assert!(!error.has_errors());
        error.extend([
            ErrorEntry::new("okay".to_string(), Severity::Info, None, Vec::new()),
            ErrorEntry::new("ooof".to_string(), Severity::Error, None, Vec::new()),
        ]);
        assert!(error.has_errors());
    }

    #[test]
    fn test_rapid_flux_display() {
        let error = RapidFluxError {
            entries: vec![
                ErrorEntry::new("first".to_string(), Severity::Error, None, Vec::new()),
                ErrorEntry::new("second".to_string(), Severity::Warning, None, Vec::new()),
            ],
        };

        assert_eq!(error.to_string().as_str(), "error: first\nwarning: second");
    }

    #[test]
    fn test_rapid_flux_debug() {
        let error = RapidFluxError {
            entries: vec![
                ErrorEntry::new("first".to_string(), Severity::Error, None, Vec::new()),
                ErrorEntry::new("second".to_string(), Severity::Warning, None, Vec::new()),
            ],
        };

        assert_eq!(
            format!("{error:?}").as_str(),
            "[ErrorEntry { message: \"first\", severity: Error, location: None, annotations: [] }, ErrorEntry { message: \"second\", severity: Warning, location: None, annotations: [] }]"
        );
    }

    #[test]
    fn test_rapid_flux_from_vec() {
        let vector = vec![ErrorEntry {
            message: "dummy".to_string(),
            severity: Severity::Error,
            ..ErrorEntry::default()
        }];

        let error: RapidFluxError = vector.clone().into();
        assert_eq!(error.entries, vector);
    }

    #[test]
    fn test_rapid_flux_push() {
        let entry = ErrorEntry {
            message: "dummy".to_string(),
            severity: Severity::Error,
            ..Default::default()
        };

        let mut error: RapidFluxError = RapidFluxError::default();
        error.push(entry.clone());
        assert_eq!(error.entries, vec![entry.clone()]);
        error.push(entry.clone());
        assert_eq!(error.entries, vec![entry.clone(), entry]);
    }

    #[test]
    fn test_rapid_flux_clear() {
        let vector = vec![
            ErrorEntry {
                message: "dummy".to_string(),
                severity: Severity::Error,
                ..ErrorEntry::default()
            },
            ErrorEntry {
                message: "other".to_string(),
                severity: Severity::Error,
                ..ErrorEntry::default()
            },
        ];
        let mut error = RapidFluxError::default();
        error.extend(vector.clone());
        assert_eq!(error.entries, vector);
        error.clear();
        assert_eq!(error.entries(), &[]);
    }

    #[rstest]
    #[case::rapidfluxerror_oneline_error(
        vec![ErrorEntry::new("Simple error".to_string(), Severity::Error, None, Vec::new())].into(),
        "error: Simple error\n",
    )]
    #[case::rapidfluxerror_oneline_warning(
        vec![ErrorEntry::new("Simple warning".to_string(), Severity::Warning, None, Vec::new())].into(),
        "warning: Simple warning\n",
    )]
    #[case::rapidfluxerror_oneline_note(
        vec![ErrorEntry::new("Simple note".to_string(), Severity::Note, None, Vec::new())].into(),
        "note: Simple note\n",
    )]
    #[case::rapidfluxerror_oneline_help(
        vec![ErrorEntry::new("Simple help".to_string(), Severity::Help, None, Vec::new())].into(),
        "help: Simple help\n",
    )]
    #[case::rapidfluxerror_oneline_info(
        vec![ErrorEntry::new("Simple info".to_string(), Severity::Info, None, Vec::new())].into(),
        "info: Simple info\n",
    )]
    #[case::rapidfluxerror_default_annotation(
        vec![
            ErrorEntry::new(
                "Annotated error".to_string(),
                Severity::Error,
                Some(Location {
                    start: FilePosition::new(1, 1),
                    source: Some(PathBuf::from_str("tests/data/sample.rflx").unwrap()),
                    end: Some(FilePosition::new(1, 8)),
                }),
                Vec::new(),
            )
        ].into(),
        indoc! {
            r"error: Annotated error
               --> tests/data/sample.rflx:1:1
                |
              1 | package Sample is
                | ^^^^^^^
                |
            "
        },
    )]
    #[case::rapidfluxerror_location_from_stdin(
        vec![
            ErrorEntry::new(
                "Annotated error".to_string(),
                Severity::Error,
                Some(Location {
                    start: FilePosition::new(1, 1),
                    source: Some(PathBuf::from_str("<stdin>").unwrap()),
                    end: Some(FilePosition::new(1, 8)),
                }),
                Vec::new(),
            )
        ].into(),
        "<stdin>:1:1: error: Annotated error\n",
    )]
    fn test_rapid_flux_print_messages(
        #[case] mut errors: RapidFluxError,
        #[case] expected_str: &str,
    ) {
        use std::io::{Read, Seek};

        let mut memory_stream = io::Cursor::new(Vec::new());
        let mut result = String::new();

        errors.print_messages(&mut memory_stream).unwrap();

        // Set the cursor at the beginning of the stream and retrieve its content
        memory_stream
            .seek(io::SeekFrom::Start(0))
            .expect("failed to seek at the position 0");
        memory_stream
            .read_to_string(&mut result)
            .expect("failed to read message from memory stream");

        assert_eq!(
            &result, expected_str,
            "expected:\n\t{expected_str}\nbut got:\n\t{result}"
        );
    }

    #[test]
    fn test_rapid_flux_serde() {
        let errors = RapidFluxError::from(vec![
            ErrorEntry {
                message: "some error".to_string(),
                severity: Severity::Error,
                annotations: vec![Annotation {
                    severity: Severity::Error,
                    location: Location::default(),
                    label: None,
                }],
                location: None,
            },
            ErrorEntry {
                message: "some other".to_string(),
                severity: Severity::Error,
                ..ErrorEntry::default()
            },
        ]);
        let bytes = bincode::serialize(&errors).expect("failed to serialize");
        let deser: RapidFluxError = bincode::deserialize(&bytes).expect("failed to deserialize");
        assert_eq!(errors, deser);
    }
}
