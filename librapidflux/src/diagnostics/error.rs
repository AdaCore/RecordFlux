use std::{
    fmt::{Debug, Display},
    io::{self, BufRead, Write},
    sync::atomic::{AtomicU64, Ordering},
};

#[cfg(not(test))]
use annotate_snippets::renderer::{Color, Style};
use annotate_snippets::Message;
use serde::{Deserialize, Serialize};

use super::location::{Location, NO_SOURCE};
use crate::source_code;

#[cfg(not(test))]
mod colors {
    use annotate_snippets::renderer::RgbColor;
    use owo_colors::colors::CustomColor;

    macro_rules! define_color {
        ($type_name:ident, $const_name:ident, $r:expr, $g:expr, $b:expr) => {
            pub(super) type $type_name = CustomColor<$r, $g, $b>;
            pub(super) const $const_name: RgbColor = RgbColor($r, $g, $b);
        };
    }

    define_color!(ErrorColor, ERROR_COLOR, 225, 0, 0);
    define_color!(WarningColor, WARNING_COLOR, 200, 200, 10);
    define_color!(InfoColor, INFO_COLOR, 0, 50, 200);
    define_color!(HelpColor, HELP_COLOR, 100, 160, 255);
    define_color!(NoteColor, NOTE_COLOR, 180, 180, 0);
}

#[cfg(not(test))]
pub(crate) const RENDERER: annotate_snippets::Renderer = annotate_snippets::Renderer::styled()
    .error(Style::new().fg_color(Some(Color::Rgb(colors::ERROR_COLOR))))
    .warning(Style::new().fg_color(Some(Color::Rgb(colors::WARNING_COLOR))))
    .info(Style::new().fg_color(Some(Color::Rgb(colors::INFO_COLOR))))
    .help(Style::new().fg_color(Some(Color::Rgb(colors::HELP_COLOR))))
    .note(Style::new().fg_color(Some(Color::Rgb(colors::NOTE_COLOR))));

#[cfg(test)]
pub(crate) const RENDERER: annotate_snippets::Renderer = annotate_snippets::Renderer::plain();

static MAX_ERROR_COUNT: AtomicU64 = AtomicU64::new(0);
static ERROR_COUNT: AtomicU64 = AtomicU64::new(0);

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
    #[cfg(test)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Info => write!(f, "info"),
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
            Severity::Help => write!(f, "help"),
            Severity::Note => write!(f, "note"),
        }
    }

    #[cfg(not(test))]
    #[cfg_attr(test, mutants::skip)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use owo_colors::OwoColorize;

        match self {
            Severity::Info => write!(f, "{}", "info".fg::<colors::InfoColor>()),
            Severity::Warning => write!(f, "{}", "warning".fg::<colors::WarningColor>()),
            Severity::Error => write!(f, "{}", "error".fg::<colors::ErrorColor>()),
            Severity::Help => write!(f, "{}", "help".fg::<colors::HelpColor>()),
            Severity::Note => write!(f, "{}", "note".fg::<colors::NoteColor>()),
        }
    }
}

impl From<Severity> for annotate_snippets::Level {
    fn from(value: Severity) -> Self {
        match value {
            Severity::Info => annotate_snippets::Level::Info,
            Severity::Warning => annotate_snippets::Level::Warning,
            Severity::Error => annotate_snippets::Level::Error,
            Severity::Help => annotate_snippets::Level::Help,
            Severity::Note => annotate_snippets::Level::Note,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Annotation {
    label: Option<String>,
    severity: Severity,
    location: Location,
}

impl Display for Annotation {
    #[cfg(test)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {}: {}",
            self.location(),
            self.severity,
            self.label.as_ref().map_or("", std::string::String::as_str)
        )
    }

    #[cfg(not(test))]
    #[cfg_attr(test, mutants::skip)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use owo_colors::OwoColorize;

        write!(
            f,
            "{}: {}: {}",
            self.location(),
            self.severity,
            self.label
                .as_ref()
                .map_or("", std::string::String::as_str)
                .bold(),
        )
    }
}

impl Annotation {
    pub fn new(label: Option<String>, severity: Severity, location: Location) -> Self {
        Self {
            label,
            severity,
            location,
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
pub struct Entry {
    message: String,
    severity: Severity,
    location: Location,
    annotations: Vec<Annotation>,
    generate_default: bool,
}

impl Display for Entry {
    #[cfg(test)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}: {}{}{}",
            if matches!(self.location(), Location::None) {
                String::new()
            } else {
                format!("{}: ", self.location())
            },
            self.severity,
            self.message,
            if self.annotations().iter().any(|a| a.label().is_some()) {
                "\n"
            } else {
                ""
            },
            self.annotations()
                .iter()
                .filter_map(|a| a.label().map(|_| a.to_string()))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }

    #[cfg(not(test))]
    #[cfg_attr(test, mutants::skip)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use owo_colors::OwoColorize;

        write!(
            f,
            "{}{}: {}{}{}",
            if matches!(self.location(), Location::None) {
                String::new()
            } else {
                format!("{}: ", self.location())
            },
            self.severity,
            self.message.bold(),
            if self.annotations().iter().any(|a| a.label().is_some()) {
                "\n"
            } else {
                ""
            },
            self.annotations()
                .iter()
                .filter_map(|a| a.label().map(|_| a.to_string()))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl Entry {
    pub fn new(
        message: String,
        severity: Severity,
        location: Location,
        annotations: Vec<Annotation>,
        generate_default: bool,
    ) -> Self {
        Self {
            message,
            severity,
            location,
            annotations,
            generate_default,
        }
    }

    pub fn extend<T: IntoIterator<Item = Annotation>>(&mut self, annotations: T) {
        self.annotations.extend(annotations);
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn location(&self) -> &Location {
        &self.location
    }

    pub fn annotations(&self) -> &[Annotation] {
        &self.annotations
    }

    pub fn generate_default_annotation(&self) -> bool {
        self.generate_default
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

        if self.generate_default && !matches!(self.location(), Location::None) {
            let default_annotation = Annotation::new(None, self.severity, self.location().clone());

            // Add squiggles below the actual error. Without this, the user won't be able to
            // see the error location (e.g. `foo.rflx:3:4`).
            self.annotations.insert(0, default_annotation);
        };

        if self.annotations.is_empty() || source.is_empty() {
            return None;
        }

        let snippet = annotate_snippets::Snippet::source(source)
            .fold(true)
            .annotations(self.annotations.iter().map(|a| a.to_annotation(source)));

        Some(message.snippet(match &self.location {
            Location::File { source, .. } => snippet.origin(source.to_str().unwrap_or(NO_SOURCE)),
            _ => snippet,
        }))
    }
}

#[derive(Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct Error {
    entries: Vec<Entry>,
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.entries)
    }
}

impl Display for Error {
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

impl From<Vec<Entry>> for Error {
    fn from(entries: Vec<Entry>) -> Self {
        Self { entries }
    }
}

impl Error {
    /// Push a new error entry.
    ///
    /// Return true if the push succeeded or false otherwise.
    /// A failed push indicates that we've reached the maximum number of error messages
    /// and we should stop the execution.
    pub fn push(&mut self, entry: Entry) -> bool {
        self.entries.push(entry);
        ERROR_COUNT.fetch_add(1, Ordering::Relaxed);

        Self::error_count_below_threshold()
    }

    /// Extend error collection from an iterator. Takes the entries' ownership.
    ///
    /// Return value is the same as for the `push` method.
    pub fn extend<T: IntoIterator<Item = Entry>>(&mut self, entries: T) -> bool {
        let entries_count = self.entries.len();
        self.entries.extend(entries);

        let added_entries_count = (self.entries.len() - entries_count) as u64;
        debug_assert!(added_entries_count <= self.entries.len() as u64);
        ERROR_COUNT.fetch_add(added_entries_count, Ordering::Relaxed);
        Self::error_count_below_threshold()
    }

    pub fn clear(&mut self) {
        self.entries.clear();
    }

    pub fn entries(&self) -> &[Entry] {
        &self.entries
    }

    pub fn into_entries(self) -> Vec<Entry> {
        self.entries
    }

    pub fn set_max_error(max: u64) {
        MAX_ERROR_COUNT.store(max, Ordering::Relaxed);
    }

    #[cfg(debug_assertions)]
    pub fn reset_counts() {
        ERROR_COUNT.store(0, Ordering::Relaxed);
        MAX_ERROR_COUNT.store(0, Ordering::Relaxed);
    }

    /// Print all messages to `stdout`
    ///
    /// # Errors
    ///
    /// Source code needs to be retrieved and error message displayed. This function
    /// might return an `io::Error` if any io operation failed.
    pub fn print_messages<T: Write>(&mut self, stream: &mut T) -> io::Result<()> {
        for entry in &mut self.entries {
            let source_code = match entry.location() {
                Location::File { source, .. } => source_code::retrieve(source),
                _ => None,
            };

            match entry.to_message_mut(&source_code.unwrap_or_default()) {
                Some(msg) => Self::print_without_trailing_whitespaces(stream, msg)?,
                None => writeln!(stream, "{entry}")?,
            }
        }

        Ok(())
    }

    /// Print an `annotate_snippets` message while stripping trailing whitespaces.
    ///
    /// # Errors
    ///
    /// Any `io::Error` that can occurs on the given stream.
    fn print_without_trailing_whitespaces<T: Write>(
        stream: &mut T,
        msg: Message<'_>,
    ) -> io::Result<()> {
        let mut memory_stream = io::Cursor::new(Vec::new());
        write!(&mut memory_stream, "{}", RENDERER.render(msg))?;
        memory_stream.set_position(0);

        for line in memory_stream.lines() {
            writeln!(stream, "{}", line?.trim_end())?;
        }

        Ok(())
    }

    pub fn has_errors(&self) -> bool {
        self.entries.iter().any(|e| e.severity == Severity::Error)
    }

    fn error_count_below_threshold() -> bool {
        ERROR_COUNT.load(Ordering::Relaxed) < MAX_ERROR_COUNT.load(Ordering::Relaxed)
            || MAX_ERROR_COUNT.load(Ordering::Relaxed) == 0
    }
}

#[cfg(test)]
mod tests {
    use std::{
        io::{self, Read, Seek},
        path::PathBuf,
    };

    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use serial_test::{parallel, serial};

    use super::{Annotation, Error, Severity};
    use crate::{
        diagnostics::{
            error::{Entry, Location},
            location::FilePosition,
        },
        source_code,
    };

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
            None,
            Severity::Error,
            Location::File {
                source: PathBuf::from("foo.rflx"),
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 1),
            },
        ),
        "foo.rflx:1:1: error: "
    )]
    #[case::annotation_with_source_path(
        Annotation::new(
            Some("some. terrible. error".to_string()),
            Severity::Error,
            Location::File {
                source: PathBuf::from("foo.rflx"),
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 1),
            },
        ),
        "foo.rflx:1:1: error: some. terrible. error"
    )]
    #[case::annotation_without_source_path(
        Annotation::new(
            Some("some. terrible. error".to_string()),
            Severity::Error,
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 1),
            },
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
            label.clone(),
            severity,
            Location::File {
                source: PathBuf::from("foo.rflx"),
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 5),
            },
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
            Some("label".to_string()),
            Severity::Error,
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 1),
            },
        );

        assert_eq!(
            annotation.location(),
            &Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 1),
            }
        );

        assert_eq!(annotation.severity(), Severity::Error);
        assert_eq!(annotation.label().expect("should be present"), "label");
    }

    #[test]
    fn test_error_entry_creation() {
        let error_entry = Entry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Location::None,
            vec![Annotation::new(
                Some("Look here".to_string()),
                Severity::Info,
                Location::Stdin {
                    start: FilePosition::new(1, 2),
                    end: FilePosition::new(3, 4),
                },
            )],
            true,
        );

        assert_eq!(error_entry.severity(), Severity::Error);
        assert_eq!(error_entry.message(), "Some terrible error");
        assert_eq!(*error_entry.location(), Location::None);
        assert_eq!(
            error_entry.annotations(),
            vec![Annotation::new(
                Some("Look here".to_string()),
                Severity::Info,
                Location::Stdin {
                    start: FilePosition::new(1, 2),
                    end: FilePosition::new(3, 4),
                },
            )]
        );
        assert!(error_entry.generate_default_annotation());
    }

    #[test]
    fn test_error_entry_extend_one() {
        let mut entry = Entry::new(
            "entry".to_string(),
            Severity::Error,
            Location::None,
            Vec::new(),
            false,
        );
        let annotation = Annotation::new(Some("a".to_string()), Severity::Error, location());
        assert!(entry.annotations.is_empty());
        entry.extend([annotation.clone()]);
        assert_eq!(entry.annotations, &[annotation.clone()]);
    }

    #[test]
    fn test_error_entry_extend_empty() {
        let mut entry = Entry::new(
            "entry".to_string(),
            Severity::Error,
            Location::None,
            Vec::new(),
            false,
        );
        assert!(entry.annotations.is_empty());
        entry.extend([]);
        assert!(entry.annotations.is_empty());
    }

    #[test]
    fn test_error_entry_extend_multiple() {
        let mut entry = Entry::new(
            "entry".to_string(),
            Severity::Error,
            Location::None,
            Vec::new(),
            false,
        );
        let annotation = Annotation::new(Some("a".to_string()), Severity::Error, location());
        assert!(entry.annotations.is_empty());
        entry.extend([annotation.clone(), annotation.clone()]);
        assert_eq!(entry.annotations, &[annotation.clone(), annotation.clone()]);
    }

    #[rstest]
    #[case::error_entry_default_annotation(true)]
    #[case::error_entry_default_annotation(false)]
    fn test_error_entry_generate_default_annotation(#[case] generate_default: bool) {
        let entry = Entry {
            generate_default,
            ..Default::default()
        };
        assert_eq!(entry.generate_default_annotation(), generate_default);
    }

    #[rstest]
    #[case::error_entry_no_location(
        Entry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Location::None,
            Vec::new(),
            true,
        ),
        "Some cool source code",
        "error: Some terrible error",
    )]
    #[case::error_entry_severity_info(
        Entry::new(
            "info".to_string(),
            Severity::Info,
            Location::None,
            Vec::new(),
            true,
        ),
        "",
        "info: info",
    )]
    #[case::error_entry_severity_help(
        Entry::new(
            "help".to_string(),
            Severity::Help,
            Location::None,
            Vec::new(),
            true,
        ),
        "",
        "help: help",
    )]
    #[case::error_entry_severity_warning(
        Entry::new(
            "warning".to_string(),
            Severity::Warning,
            Location::None,
            Vec::new(),
            true,
        ),
        "",
        "warning: warning",
    )]
    #[case::error_entry_severity_note(
        Entry::new(
            "note".to_string(),
            Severity::Note,
            Location::None,
            Vec::new(),
            true,
        ),
        "",
        "note: note",
    )]
    #[case::error_entry_with_source_file(
        Entry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 8),
            },
            Vec::new(),
            true,
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
        Entry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Location::File {
                source: PathBuf::from("test.rflx"),
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 8),
            },
            Vec::new(),
            true,
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
        Entry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Location::File {
                source: PathBuf::from("test.rflx"),
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 8),
            },
            vec![
                Annotation::new(
                    Some("some help".to_string()),
                    Severity::Help,
                    Location::Stdin {
                        start: FilePosition::new(2, 1),
                        end: FilePosition::new(2, 4),
                    },
                )
            ],
            true
        ),
        indoc! {
            r"package Test is
              end Test;"
        },
        indoc! {
            r"error: Some terrible error
               --> test.rflx:1:1
                |
              1 | package Test is
                | ^^^^^^^
              2 | end Test;
                | --- help: some help
                |"
        },
    )]
    fn test_error_entry_to_message(
        #[case] mut entry: Entry,
        #[case] source_code: &str,
        #[case] expected_str: &str,
    ) {
        use crate::diagnostics::error::RENDERER;

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
        Entry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Location::Stdin {
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 8),
            },
            Vec::new(),
            true,
        ),
        "<stdin>:1:1: error: Some terrible error"
    )]
    #[case::error_entry_with_location_and_source(
        Entry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Location::File {
                source: PathBuf::from("foo.rflx"),
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 8),
            },
            Vec::new(),
            true,
        ),
        "foo.rflx:1:1: error: Some terrible error"
    )]
    #[case::error_entry_with_annotations(
        Entry::new(
            "Some terrible error".to_string(),
            Severity::Error,
            Location::File {
                source: PathBuf::from("foo.rflx"),
                start: FilePosition::new(1, 1),
                end: FilePosition::new(1, 8),
            },
            vec![
                Annotation {
                    severity: Severity::Info,
                    location: Location::File {
                        source: PathBuf::from("foo.rflx"),
                        start: FilePosition::new(1, 1),
                        end: FilePosition::new(1, 8),
                    },
                    label: Some("some label".to_string())
                }
            ],
            true,
        ),
        indoc! {
            r"foo.rflx:1:1: error: Some terrible error
              foo.rflx:1:1: info: some label"
        }
    )]
    fn test_error_entry_display(#[case] error_entry: Entry, #[case] expected_str: &str) {
        assert_eq!(error_entry.to_string().as_str(), expected_str);
    }

    #[test]
    fn test_error_entries() {
        let error = Error {
            entries: vec![Entry::new(
                "first".to_string(),
                Severity::Error,
                Location::None,
                Vec::new(),
                true,
            )],
        };
        assert_eq!(
            error.entries(),
            vec![Entry::new(
                "first".to_string(),
                Severity::Error,
                Location::None,
                Vec::new(),
                true,
            )]
        );
    }

    #[rstest]
    #[case::errors(
        vec![
            Entry::new("okay".to_string(), Severity::Info, Location::None, Vec::new(), true),
            Entry::new("ooof".to_string(), Severity::Error, Location::None, Vec::new(), true),
        ],
        true,
    )]
    #[case::no_errors(
        vec![
            Entry::new("okay".to_string(), Severity::Info, Location::None, Vec::new(), true),
        ],
        false,
    )]
    #[parallel]
    fn test_error_has_error(#[case] entries: Vec<Entry>, #[case] expected: bool) {
        let mut error = Error::default();
        assert!(!error.has_errors());
        error.extend(entries);
        assert_eq!(error.has_errors(), expected);
    }

    #[test]
    fn test_error_display() {
        let error = Error {
            entries: vec![
                Entry::new(
                    "first".to_string(),
                    Severity::Error,
                    Location::None,
                    Vec::new(),
                    true,
                ),
                Entry::new(
                    "second".to_string(),
                    Severity::Warning,
                    Location::None,
                    Vec::new(),
                    true,
                ),
            ],
        };

        assert_eq!(error.to_string().as_str(), "error: first\nwarning: second");
    }

    #[test]
    fn test_error_debug() {
        let error = Error {
            entries: vec![
                Entry::new(
                    "first".to_string(),
                    Severity::Error,
                    Location::None,
                    vec![Annotation::new(None, Severity::Error, location())],
                    true,
                ),
                Entry::new(
                    "second".to_string(),
                    Severity::Warning,
                    Location::None,
                    Vec::new(),
                    true,
                ),
            ],
        };

        assert_eq!(
            format!("{error:?}").as_str(),
            "[Entry { message: \"first\", severity: Error, location: None, annotations: \
            [Annotation { label: None, severity: Error, location: File { start: \
            FilePosition(1, 1), end: FilePosition(2, 2), source: \"file\" } }], generate_default: true }, \
            Entry { message: \"second\", severity: Warning, location: None, \
            annotations: [], generate_default: true }]"
        );
    }

    #[test]
    fn test_error_from_vec() {
        let vector = vec![Entry {
            message: "dummy".to_string(),
            severity: Severity::Error,
            ..Entry::default()
        }];

        let error: Error = vector.clone().into();
        assert_eq!(error.entries, vector);
    }

    #[test]
    #[parallel]
    fn test_error_push() {
        Error::reset_counts();
        let entry = Entry {
            message: "dummy".to_string(),
            severity: Severity::Error,
            annotations: vec![Annotation::new(None, Severity::Error, location())],
            location: location(),
            generate_default: true,
        };

        let mut error: Error = Error::default();
        assert!(error.push(entry.clone()));
        assert_eq!(error.entries, vec![entry.clone()]);
        assert!(error.push(entry.clone()));
        assert_eq!(error.entries, vec![entry.clone(), entry]);
    }

    #[test]
    #[serial]
    fn test_error_push_with_limit() {
        Error::reset_counts();
        Error::set_max_error(2);
        let entry = Entry {
            message: "dummy".to_string(),
            severity: Severity::Error,
            annotations: vec![Annotation::new(None, Severity::Error, location())],
            location: location(),
            generate_default: true,
        };

        let mut error: Error = Error::default();
        assert!(error.push(entry.clone()));
        assert_eq!(error.entries, vec![entry.clone()]);
        assert!(!error.push(entry.clone()));
        Error::set_max_error(0);
    }

    #[test]
    #[parallel]
    fn test_error_extend() {
        Error::reset_counts();
        let entry = Entry {
            message: "dummy".to_string(),
            severity: Severity::Error,
            annotations: vec![Annotation::new(None, Severity::Error, location())],
            location: location(),
            generate_default: true,
        };
        let second_entry = Entry {
            message: "other dummy".to_string(),
            severity: Severity::Error,
            annotations: vec![Annotation::new(None, Severity::Error, location())],
            location: location(),
            generate_default: true,
        };

        let mut error: Error = Error::default();
        assert!(error.extend([entry, second_entry]));
    }

    #[test]
    #[serial]
    fn test_error_extend_with_limit() {
        Error::reset_counts();
        Error::set_max_error(3);
        let entries = vec![
            Entry {
                message: "dummy".to_string(),
                severity: Severity::Error,
                annotations: vec![Annotation::new(None, Severity::Error, location())],
                location: location(),
                generate_default: true,
            },
            Entry {
                message: "other dummy".to_string(),
                severity: Severity::Error,
                annotations: vec![Annotation::new(None, Severity::Error, location())],
                location: location(),
                generate_default: true,
            },
        ];

        let mut error: Error = Error::default();
        assert!(error.extend(entries.clone()));
        assert!(!error.extend(entries.clone()));
        assert!(!error.extend(entries.clone()));
        Error::set_max_error(0);
    }

    #[test]
    #[parallel]
    fn test_error_clear() {
        let vector = vec![
            Entry {
                message: "dummy".to_string(),
                severity: Severity::Error,
                ..Entry::default()
            },
            Entry {
                message: "other".to_string(),
                severity: Severity::Error,
                ..Entry::default()
            },
        ];
        let mut error = Error::default();
        error.extend(vector.clone());
        assert_eq!(error.entries, vector);
        error.clear();
        assert_eq!(error.entries(), &[]);
    }

    #[rstest]
    #[case::error_oneline_error(
        vec![Entry::new("Simple error".to_string(), Severity::Error, Location::None, Vec::new(), true)].into(),
        "error: Simple error\n",
    )]
    #[case::error_oneline_warning(
        vec![Entry::new("Simple warning".to_string(), Severity::Warning, Location::None, Vec::new(), true)].into(),
        "warning: Simple warning\n",
    )]
    #[case::error_oneline_note(
        vec![Entry::new("Simple note".to_string(), Severity::Note, Location::None, Vec::new(), true)].into(),
        "note: Simple note\n",
    )]
    #[case::error_oneline_help(
        vec![Entry::new("Simple help".to_string(), Severity::Help, Location::None, Vec::new(), true)].into(),
        "help: Simple help\n",
    )]
    #[case::error_oneline_info(
        vec![Entry::new("Simple info".to_string(), Severity::Info, Location::None, Vec::new(), true)].into(),
        "info: Simple info\n",
    )]
    #[case::error_location_from_stdin(
        vec![
            Entry::new(
                "Annotated error".to_string(),
                Severity::Error,
                Location::File {
                    start: FilePosition::new(1, 1),
                    source: PathBuf::from("<stdin>"),
                    end: FilePosition::new(1, 8),
                },
                Vec::new(),
                true,
            )
        ].into(),
        "<stdin>:1:1: error: Annotated error\n",
    )]
    fn test_error_print_messages(#[case] mut errors: Error, #[case] expected_str: &str) {
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

        assert_eq!(&result, expected_str);
    }

    #[test]
    #[allow(clippy::items_after_statements)]
    #[serial]
    fn test_error_print_message_default_annotation() {
        let file_path = PathBuf::from("tests/data/sample.rflx");
        let mut error: Error = vec![Entry::new(
            "Annotated error".to_string(),
            Severity::Error,
            Location::File {
                start: FilePosition::new(1, 1),
                source: file_path.clone(),
                end: FilePosition::new(1, 8),
            },
            Vec::new(),
            true,
        )]
        .into();
        const EXPECTED_ERROR: &str = indoc! {
            r"error: Annotated error
               --> tests/data/sample.rflx:1:1
                |
              1 | package Sample is
                | ^^^^^^^
                |
            "
        };

        let mut memory_stream = io::Cursor::new(Vec::new());
        let mut result = String::new();

        source_code::register(
            file_path,
            include_str!("../../tests/data/sample.rflx").to_string(),
        );
        error.print_messages(&mut memory_stream).unwrap();

        // Set the cursor at the beginning of the stream and retrieve its content
        memory_stream
            .seek(io::SeekFrom::Start(0))
            .expect("failed to seek at the position 0");
        memory_stream
            .read_to_string(&mut result)
            .expect("failed to read message from memory stream");

        assert_eq!(&result, EXPECTED_ERROR);
    }

    #[test]
    fn test_error_serde() {
        let errors = Error::from(vec![
            Entry {
                message: "some error".to_string(),
                severity: Severity::Error,
                annotations: vec![Annotation {
                    severity: Severity::Error,
                    location: location(),
                    label: None,
                }],
                location: Location::None,
                generate_default: true,
            },
            Entry {
                message: "some other".to_string(),
                severity: Severity::Error,
                ..Entry::default()
            },
        ]);
        let bytes = bincode::serialize(&errors).expect("failed to serialize");
        let deser: Error = bincode::deserialize(&bytes).expect("failed to deserialize");
        assert_eq!(errors, deser);
    }

    #[allow(clippy::redundant_clone)]
    #[test]
    fn test_error_clone() {
        use std::ptr::addr_of;
        let error = Error::default();
        let cloned = error.clone();

        assert_ne!(addr_of!(error), addr_of!(cloned));
    }

    fn location() -> Location {
        Location::File {
            start: FilePosition::new(1, 1),
            end: FilePosition::new(2, 2),
            source: PathBuf::from("file"),
        }
    }
}
