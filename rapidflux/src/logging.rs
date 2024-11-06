use librapidflux as lib;
use pyo3::{
    prelude::*,
    types::{PyDict, PyList, PyString},
};

use crate::register_submodule_declarations;

/// Format a Python's string with the arguments passed in `args` dict.
///
/// # Errors
///
/// Any Python exception that may be raised while formating the string.
fn format_python_args<'py>(
    py: Python<'py>,
    format_str: Bound<'py, PyString>,
    args: Bound<'py, PyDict>,
) -> PyResult<Bound<'py, PyString>> {
    const FORMAT_SOURCE_CODE: &str = "format_str.format(**args)";

    let elements_list = [
        (PyString::new_bound(py, "format_str"), format_str.into_any()),
        (PyString::new_bound(py, "args"), args.into_any()),
    ];

    let locals = PyDict::from_sequence_bound(&PyList::new_bound(py, elements_list))?;
    let result = py.eval_bound(FORMAT_SOURCE_CODE, None, Some(&locals))?;
    debug_assert!(result.is_instance_of::<PyString>());
    result
        .downcast_into::<PyString>()
        .map_err(std::convert::Into::into)
}

/// Display the log if the quiet flag is not set.
///
/// # Errors
///
/// Any Python exception that may be raised while formating the log message.
fn log_if_necessary(
    py: Python<'_>,
    format_str: Bound<'_, PyString>,
    kwargs: Option<Bound<'_, PyDict>>,
    severity: lib::diagnostics::error::Severity,
) -> PyResult<()> {
    if lib::diagnostics::logging::is_quiet() {
        return Ok(());
    }

    if let Some(args) = kwargs {
        lib::log!(severity, "{}", format_python_args(py, format_str, args)?);
    } else {
        lib::log!(severity, "{format_str}");
    }

    Ok(())
}

#[pyfunction]
#[pyo3(signature = (format_str, **kwargs))]
fn info(
    py: Python<'_>,
    format_str: Bound<'_, PyString>,
    kwargs: Option<Bound<'_, PyDict>>,
) -> PyResult<()> {
    log_if_necessary(
        py,
        format_str,
        kwargs,
        lib::diagnostics::error::Severity::Info,
    )
}

#[pyfunction]
#[pyo3(signature = (format_str, **kwargs))]
fn warning(
    py: Python<'_>,
    format_str: Bound<'_, PyString>,
    kwargs: Option<Bound<'_, PyDict>>,
) -> PyResult<()> {
    log_if_necessary(
        py,
        format_str,
        kwargs,
        lib::diagnostics::error::Severity::Warning,
    )
}

#[pyfunction]
#[pyo3(signature = (format_str, **kwargs))]
fn error(
    py: Python<'_>,
    format_str: Bound<'_, PyString>,
    kwargs: Option<Bound<'_, PyDict>>,
) -> PyResult<()> {
    log_if_necessary(
        py,
        format_str,
        kwargs,
        lib::diagnostics::error::Severity::Error,
    )
}

#[pyfunction]
#[pyo3(signature = (format_str, **kwargs))]
fn help(
    py: Python<'_>,
    format_str: Bound<'_, PyString>,
    kwargs: Option<Bound<'_, PyDict>>,
) -> PyResult<()> {
    log_if_necessary(
        py,
        format_str,
        kwargs,
        lib::diagnostics::error::Severity::Help,
    )
}

#[pyfunction]
#[pyo3(signature = (format_str, **kwargs))]
fn note(
    py: Python<'_>,
    format_str: Bound<'_, PyString>,
    kwargs: Option<Bound<'_, PyDict>>,
) -> PyResult<()> {
    log_if_necessary(
        py,
        format_str,
        kwargs,
        lib::diagnostics::error::Severity::Note,
    )
}

#[pyfunction]
fn set_quiet(value: bool) {
    lib::diagnostics::logging::set_quiet(value);
}

register_submodule_declarations!(
    logging,
    [],
    [],
    [error, help, info, note, set_quiet, warning]
);
