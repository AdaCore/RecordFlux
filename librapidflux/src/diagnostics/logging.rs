//! Logging utilities.
//!
//! This modules provides some macros for logging with the `annotate_snippets` crate.
//!
//! The following levels are available:
//!
//! - error
//! - warning
//! - note
//! - info
//! - help
//!
//! # Examples
//!
//! ```rust
//!     use librapidflux::{log, diagnostics::error::Severity};
//!
//!     log!(Severity::Error, "Some error");
//!     log!(Severity::Info, "I have {} apples", 27);
//! ```
//!
//! Shortcuts are available for each severity level:
//!
//! ```rust
//!     use librapidflux::{error, info, help, diagnostics::error::Severity};
//!
//!     error!("Oh nooo!");
//!     info!("I have {} apples", 27);
//!     help!("This is helpful");
//! ```

use std::{
    io,
    sync::atomic::{AtomicBool, Ordering},
};

use annotate_snippets::Message;

use crate::diagnostics::error::RENDERER;

#[doc(hidden)]
pub static IS_QUIET: AtomicBool = AtomicBool::new(false);

#[macro_export]
#[doc(hidden)]
macro_rules! create_snippet {
    ($stream:expr, $severity:expr, $fmt:literal, $($args:expr),*$(,)?) => {
        if !$crate::diagnostics::logging::IS_QUIET.load(::std::sync::atomic::Ordering::Relaxed) {
            $crate::diagnostics::logging::print_snippet(
                $stream,
                ::std::convert::Into::<::annotate_snippets::Level>::into($severity)
                    .title(&format!("{}", format_args!($fmt, $( $args ),* )))
            )
            .expect("failed to write to stderr");
        }
    };
    ($stream:expr, $severity:expr, $fmt:literal) => {
        if !$crate::diagnostics::logging::IS_QUIET.load(::std::sync::atomic::Ordering::Relaxed) {

            $crate::diagnostics::logging::print_snippet(
                $stream,
                ::std::convert::Into::<::annotate_snippets::Level>::into($severity)
                    .title(&format!("{}", format_args!($fmt)))
            )
            .expect("failed to write to stderr");
        }
    };
}

#[macro_export]
macro_rules! log {
    ($severity:expr, $fmt:literal$(,)? $($args:expr),*$(,)?) => {
        $crate::create_snippet!(&mut ::anstream::stderr(), $severity, $fmt, $($args)*);
    };
    ($severity:expr, $fmt:literal$(,)?) => {
        $crate::create_snippet!(&mut ::anstream::stderr(), $severity, $fmt);
    };
}

#[macro_export]
macro_rules! info {
    ($fmt:literal, $($args:expr),*$(,)?) => {
        $crate::log!($crate::diagnostics::error::Severity::Info, $fmt, $($args),*);
    };
    ($fmt:literal) => {
        $crate::log!($crate::diagnostics::error::Severity::Info, $fmt);
    };
}

#[macro_export]
macro_rules! warning {
    ($fmt:literal, $($args:expr),*$(,)?) => {
        $crate::log!($crate::diagnostics::error::Severity::Warning, $fmt, $($args),*);
    };
    ($fmt:literal) => {
        $crate::log!($crate::diagnostics::error::Severity::Warning, $fmt);
    };
}

#[macro_export]
macro_rules! error {
    ($fmt:literal, $($args:expr),*$(,)?) => {
        $crate::log!($crate::diagnostics::error::Severity::Error, $fmt, $($args),*);
    };
    ($fmt:literal) => {
        $crate::log!($crate::diagnostics::error::Severity::Error, $fmt);
    };
}

#[macro_export]
macro_rules! help {
    ($fmt:literal, $($args:expr),*$(,)?) => {
        $crate::log!($crate::diagnostics::error::Severity::Help, $fmt, $($args),*);
    };
    ($fmt:literal) => {
        $crate::log!($crate::diagnostics::error::Severity::Help, $fmt);
    };
}

#[macro_export]
macro_rules! note {
    ($fmt:literal, $($args:expr),*$(,)?) => {
        $crate::log!($crate::diagnostics::error::Severity::Note, $fmt, $($args),*);
    };
    ($fmt:literal) => {
        $crate::log!($crate::diagnostics::error::Severity::Note, $fmt);
    };
}

/// Print an `annotate_snippets`' snippet with the given stream using the application wide
/// renderer.
///
/// # Errors
///
/// An ``io::Error`` in case the write operation failed.
pub fn print_snippet<S: io::Write>(stream: &mut S, message: Message<'_>) -> io::Result<()> {
    writeln!(stream, "{}", RENDERER.render(message))
}

pub fn is_quiet() -> bool {
    IS_QUIET.load(Ordering::Relaxed)
}

pub fn set_quiet(enable: bool) {
    IS_QUIET.store(enable, Ordering::Relaxed);
}

#[cfg(test)]
mod tests {
    use std::io::{Cursor, Read};

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use serial_test::{parallel, serial};

    use super::set_quiet;
    use crate::diagnostics::{error::Severity, logging::is_quiet};

    #[rstest]
    #[case::log_error(Severity::Error, "error: Foo bar baz\n")]
    #[case::log_warning(Severity::Warning, "warning: Foo bar baz\n")]
    #[case::log_help(Severity::Help, "help: Foo bar baz\n")]
    #[case::log_note(Severity::Note, "note: Foo bar baz\n")]
    #[case::log_info(Severity::Info, "info: Foo bar baz\n")]
    #[parallel]
    fn test_log_only_str(#[case] severity: Severity, #[case] expected_str: &str) {
        let mut cursor = Cursor::new(Vec::new());
        let mut output = String::new();

        create_snippet!(&mut cursor, severity, "Foo bar baz");
        cursor.set_position(0);
        cursor
            .read_to_string(&mut output)
            .expect("failed to read output");
        assert_eq!(output.as_str(), expected_str);
    }

    #[rstest]
    #[case::log_error(Severity::Error, "error: 1 two 3.0\n")]
    #[case::log_warning(Severity::Warning, "warning: 1 two 3.0\n")]
    #[case::log_help(Severity::Help, "help: 1 two 3.0\n")]
    #[case::log_note(Severity::Note, "note: 1 two 3.0\n")]
    #[case::log_info(Severity::Info, "info: 1 two 3.0\n")]
    #[parallel]
    fn test_log_with_format(#[case] severity: Severity, #[case] expected_str: &str) {
        let mut cursor = Cursor::new(Vec::new());
        let mut output = String::new();

        create_snippet!(&mut cursor, severity, "{} {} {:.1}", 1, "two", 3.0);
        cursor.set_position(0);
        cursor
            .read_to_string(&mut output)
            .expect("failed to read output");
        assert_eq!(output.as_str(), expected_str);
    }

    #[test]
    #[serial]
    fn test_logging_quiet() {
        let mut cursor = Cursor::new(Vec::new());
        let mut output = String::new();

        set_quiet(true);
        create_snippet!(&mut cursor, Severity::Info, "This should never be printed");
        cursor.set_position(0);
        cursor
            .read_to_string(&mut output)
            .expect("failed to read output");
        assert!(output.is_empty());
        set_quiet(false);
    }

    #[test]
    #[serial]
    fn test_logging_not_quiet() {
        let mut cursor = Cursor::new(Vec::new());
        let mut output = String::new();

        set_quiet(false);
        create_snippet!(&mut cursor, Severity::Info, "This should be printed");
        cursor.set_position(0);
        cursor
            .read_to_string(&mut output)
            .expect("failed to read output");
        assert_eq!(output.as_str(), "info: This should be printed\n");
    }

    #[rstest]
    #[case::quiet_true(true)]
    #[case::quiet_false(false)]
    #[serial]
    fn test_is_quiet_true(#[case] quiet: bool) {
        set_quiet(quiet);
        assert_eq!(is_quiet(), quiet);
        set_quiet(false);
    }
}
