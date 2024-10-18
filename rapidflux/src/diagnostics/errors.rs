use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    io::Write,
};

use bincode::{deserialize, serialize};
use pyo3::{
    basic::CompareOp,
    create_exception,
    exceptions::{PyException, PyNotImplementedError, PyValueError},
    prelude::*,
    types::{PyBytes, PyType},
};

use lazy_static::lazy_static;
use librapidflux::diagnostics as lib;
use serde::{Deserialize, Serialize};

use crate::impl_states;

use super::Location;

#[pyclass(module = "rflx.rapidflux")]
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Severity(lib::Severity);

/// Severity is an enum and we want it to act like a Python enum.
/// So it's expected that the following always works in Python:
///
/// ```python
/// a = some_func()
/// if a is Severity.ERROR:
///     pass
/// ```
///
/// This behavior will only work if the instance always remains the same. This is why we always
/// return a borrowed value of a lazily initialized Python instance for each enum field.
///
/// **Every time this enum is referenced in the bindings, the associated method must be called**
/// otherwise the below property might not hold true anymore.
#[pymethods]
impl Severity {
    #[new]
    fn new(severity: &str) -> PyResult<Self> {
        Ok(match severity {
            "INFO" => Self(lib::Severity::Info),
            "WARNING" => Self(lib::Severity::Warning),
            "ERROR" => Self(lib::Severity::Error),
            "HELP" => Self(lib::Severity::Help),
            "NOTE" => Self(lib::Severity::Note),
            _ => Err(PyValueError::new_err(format!(
                "Unknown severity: {severity}"
            )))?,
        })
    }

    pub fn __getnewargs__(&self) -> (String,) {
        // Removing ANSI escapes is needed here otherwise Python will not be able to rebuild the
        // object while calling `pickle.load`
        (anstream::adapter::strip_str(&self.0.to_string())
            .to_string()
            .to_uppercase(),)
    }

    fn __str__(&self) -> String {
        anstream::adapter::strip_str(&self.0.to_string()).to_string()
    }

    fn __repr__(&self) -> String {
        format!("{:?}", self.0)
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp) -> PyResult<bool> {
        match op {
            CompareOp::Eq => Ok(self.0 == other.0),
            CompareOp::Ne => Ok(self.0 != other.0),
            _ => Err(PyNotImplementedError::new_err(format!(
                "comparison {op:?} is not implement for Severity"
            ))),
        }
    }

    #[classattr]
    #[pyo3(name = "ERROR")]
    fn error(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Severity(lib::Severity::Error).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "WARNING")]
    fn warning(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Severity(lib::Severity::Warning).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "INFO")]
    fn info(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Severity(lib::Severity::Info).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "NOTE")]
    fn note(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Severity(lib::Severity::Note).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "HELP")]
    fn help(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Severity(lib::Severity::Help).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }
}

impl From<Severity> for lib::Severity {
    fn from(value: Severity) -> Self {
        value.0
    }
}

#[pyclass(module = "rflx.rapidflux")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Annotation(pub lib::Annotation);

#[pymethods]
impl Annotation {
    #[new]
    #[pyo3(signature = (label, severity, location))]
    fn new(label: Option<String>, severity: Severity, location: Location) -> Self {
        Self(lib::Annotation::new(label, severity.into(), location.0))
    }

    fn __getnewargs__<'py>(
        &'py self,
        py: Python<'py>,
    ) -> (Option<String>, Borrowed<'py, 'py, PyAny>, Location) {
        (
            self.label().map(std::borrow::ToOwned::to_owned),
            self.severity(py),
            self.location(),
        )
    }

    fn __str__(&self) -> String {
        anstream::adapter::strip_str(&self.0.to_string()).to_string()
    }

    fn __repr__(&self) -> String {
        format!("{:?}", self.0)
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp) -> PyResult<bool> {
        Ok(match op {
            CompareOp::Eq => self.0 == other.0,
            CompareOp::Ne => self.0 != other.0,
            _ => Err(PyValueError::new_err(format!(
                "Unsupported {op:?} for RapidFluxError"
            )))?,
        })
    }

    #[getter]
    pub fn location(&self) -> Location {
        Location(self.0.location().clone())
    }

    #[getter]
    pub fn severity<'py>(&'py self, py: Python<'py>) -> Borrowed<'py, 'py, PyAny> {
        match self.0.severity() {
            lib::Severity::Info => Severity::info(py),
            lib::Severity::Warning => Severity::warning(py),
            lib::Severity::Error => Severity::error(py),
            lib::Severity::Help => Severity::help(py),
            lib::Severity::Note => Severity::note(py),
        }
    }

    #[getter]
    pub fn label(&self) -> Option<&str> {
        self.0.label()
    }
}

#[pyclass(module = "rflx.rapidflux")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ErrorEntry(lib::ErrorEntry);

#[pymethods]
impl ErrorEntry {
    #[new]
    #[pyo3(signature = (
            message,
            severity,
            location,
            annotations = Vec::new(),
            generate_default_annotation = true
        )
    )]
    pub fn new(
        message: String,
        severity: Severity,
        location: Location,
        annotations: Vec<Annotation>,
        generate_default_annotation: bool,
    ) -> Self {
        Self(lib::ErrorEntry::new(
            message,
            severity.into(),
            location.0,
            annotations.into_iter().map(|a| a.0).collect(),
            generate_default_annotation,
        ))
    }

    fn extend(&mut self, annotations: Vec<Annotation>) {
        self.0.extend(annotations.into_iter().map(|a| a.0));
    }

    fn __str__(&self) -> String {
        anstream::adapter::strip_str(&self.0.to_string()).to_string()
    }

    fn __repr__(&self) -> String {
        format!("{:?}", self.0)
    }

    fn __hash__(&self) -> u64 {
        let mut hasher = DefaultHasher::default();
        self.0.to_string().hash(&mut hasher);
        hasher.finish()
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp) -> PyResult<bool> {
        Ok(match op {
            CompareOp::Eq => self.0 == other.0,
            CompareOp::Ne => self.0 != other.0,
            _ => Err(PyNotImplementedError::new_err(format!(
                "Operation {op:?} is not implemented"
            )))?,
        })
    }

    fn __getnewargs__<'py>(
        &'py self,
        py: Python<'py>,
    ) -> (
        String,
        Borrowed<'py, 'py, PyAny>,
        Location,
        Vec<Annotation>,
        bool,
    ) {
        (
            self.0.message().to_owned(),
            self.severity(py),
            self.location(),
            self.annotations(),
            self.0.generate_default_annotation(),
        )
    }

    #[getter]
    fn message(&self) -> &str {
        self.0.message()
    }

    #[getter]
    fn severity<'py>(&'py self, py: Python<'py>) -> Borrowed<'py, 'py, PyAny> {
        match self.0.severity() {
            lib::Severity::Info => Severity::info(py),
            lib::Severity::Warning => Severity::warning(py),
            lib::Severity::Error => Severity::error(py),
            lib::Severity::Help => Severity::help(py),
            lib::Severity::Note => Severity::note(py),
        }
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location().clone())
    }

    #[getter]
    fn annotations(&self) -> Vec<Annotation> {
        self.0
            .annotations()
            .iter()
            .cloned()
            .map(Annotation)
            .collect()
    }
}

#[pyclass(module = "rflx.rapidflux", extends = PyException, subclass)]
#[derive(Clone, Serialize, Deserialize, Debug)]
#[pyo3(name = "RecordFluxError")]
pub struct RapidFluxError(pub lib::RapidFluxError);

#[pymethods]
impl RapidFluxError {
    #[new]
    #[pyo3(signature = (entries=Vec::new()))]
    fn new(entries: Vec<ErrorEntry>) -> Self {
        Self(
            entries
                .into_iter()
                .map(|e| e.0)
                .collect::<Vec<lib::ErrorEntry>>()
                .into(),
        )
    }

    fn __str__(&self) -> String {
        anstream::adapter::strip_str(&self.0.to_string()).to_string()
    }

    fn __repr__(&self) -> String {
        format!("{:?}", self.0)
    }

    fn __getnewargs__(&self) -> (Vec<ErrorEntry>,) {
        (self.entries(),)
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp) -> PyResult<bool> {
        Ok(match op {
            CompareOp::Eq => self.0 == other.0,
            CompareOp::Ne => self.0 != other.0,
            _ => Err(PyValueError::new_err(format!(
                "Unsupported {op:?} for RapidFluxError"
            )))?,
        })
    }

    #[classmethod]
    pub fn set_max_error(_cls: &Bound<'_, PyType>, max_value: u64) {
        lib::RapidFluxError::set_max_error(max_value);
    }

    #[cfg(debug_assertions)]
    #[classmethod]
    pub fn reset_errors(_cls: &Bound<'_, PyType>) {
        lib::RapidFluxError::reset_counts();
    }

    #[getter]
    fn entries(&self) -> Vec<ErrorEntry> {
        self.0.entries().iter().cloned().map(ErrorEntry).collect()
    }

    fn has_errors(&self) -> bool {
        self.0.has_errors()
    }

    pub fn push(&mut self, entry: ErrorEntry) -> PyResult<()> {
        if self.0.push(entry.0) {
            Ok(())
        } else {
            Err(self.clone().into())
        }
    }

    pub fn extend(&mut self, entries: Vec<ErrorEntry>) -> PyResult<()> {
        if self.0.extend(entries.into_iter().map(|e| e.0)) {
            Ok(())
        } else {
            Err(self.clone().into())
        }
    }

    /// Print error messages to the standard output.
    /// Raises File exception if the related source couldn't be loaded
    pub fn print_messages(&mut self) -> PyResult<()> {
        let mut stderr = anstream::stderr();
        self.0
            .print_messages(&mut stderr)
            .map_err(Into::<PyErr>::into)?;

        stderr.flush().map_err(Into::<PyErr>::into)
    }

    pub fn propagate(&mut self) -> PyResult<()> {
        if self.0.has_errors() {
            return Err(self.clone().into());
        }

        self.print_messages()?;

        Ok(())
    }
}

impl IntoPy<Py<PyAny>> for RapidFluxError {
    fn into_py(self, py: Python<'_>) -> Py<PyAny> {
        Py::new(py, self)
            .expect("Failed to create python object")
            .into_any()
    }
}

impl From<RapidFluxError> for PyErr {
    fn from(value: RapidFluxError) -> Self {
        PyErr::new::<RapidFluxError, RapidFluxError>(value)
    }
}

create_exception!(
    rflx.rapidflux,
    FatalError,
    PyException,
    "Error indicating a bug."
);

impl_states!(Annotation, ErrorEntry, Severity, RapidFluxError);
