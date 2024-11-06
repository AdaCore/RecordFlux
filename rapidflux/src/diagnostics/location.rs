use std::path::PathBuf;

use bincode::{deserialize, serialize};
use librapidflux::diagnostics::location as lib;
use pyo3::{
    prelude::*,
    types::{PyBool, PyBytes, PyNone, PyNotImplemented},
};
use serde::{Deserialize, Serialize};

use crate::impl_states;

pub const NO_LOCATION: Location = Location(lib::Location::None);

#[pyclass(module = "rflx.rapidflux")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Location(pub(crate) lib::Location);

#[pymethods]
impl Location {
    #[new]
    fn new(start: (u32, u32), source: Option<PathBuf>, end: Option<(u32, u32)>) -> Self {
        if matches!(&source, Some(s) if s.to_string_lossy() == lib::NO_SOURCE) {
            return Location(lib::Location::None);
        }
        Location(lib::Location::new(
            start.into(),
            if let Some(e) = end {
                e.into()
            } else {
                start.into()
            },
            if matches!(&source, Some(s) if s.to_string_lossy() == lib::STDIN_SOURCE) {
                None
            } else {
                source
            },
        ))
    }

    fn __hash__(&self) -> usize {
        0
    }

    pub(crate) fn __repr__(&self) -> String {
        match &self.0 {
            lib::Location::None => {
                format!("Location((1, 1), \"{}\", (1, 1))", lib::NO_SOURCE)
            }
            lib::Location::Stdin { start, end } => format!(
                "Location({:?}, None, {:?})",
                std::convert::Into::<(u32, u32)>::into(*start),
                std::convert::Into::<(u32, u32)>::into(*end)
            ),
            lib::Location::File { start, end, source } => format!(
                "Location({:?}, \"{}\", {:?})",
                std::convert::Into::<(u32, u32)>::into(*start),
                source.to_string_lossy(),
                std::convert::Into::<(u32, u32)>::into(*end)
            ),
        }
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<PyObject> {
        if other.is_instance_of::<Location>() {
            let other_location = other.extract::<Location>()?;
            Ok(PyBool::new_bound(py, *self == other_location)
                .to_owned()
                .into())
        } else {
            Ok(PyNotImplemented::get_bound(py).to_owned().into())
        }
    }

    fn __lt__(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<PyObject> {
        if other.is_instance_of::<Location>() {
            let other_location = other.extract::<Location>()?;
            Ok(
                PyBool::new_bound(py, self.get_start() < other_location.get_start())
                    .to_owned()
                    .into(),
            )
        } else {
            Ok(PyNotImplemented::get_bound(py).to_owned().into())
        }
    }

    fn __getnewargs__(&self) -> ((u32, u32), Option<PathBuf>, Option<(u32, u32)>) {
        (self.get_start(), self._get_source(), Some(self.get_end()))
    }

    #[getter]
    fn get_start(&self) -> (u32, u32) {
        match self.0 {
            lib::Location::None => (1, 1),
            lib::Location::Stdin { start, .. } | lib::Location::File { start, .. } => start.into(),
        }
    }

    #[getter]
    fn get_source(&self, py: Python<'_>) -> PyResult<PyObject> {
        match &self._get_source() {
            None => Ok(PyNone::get_bound(py).to_owned().into()),
            Some(source) => {
                let pathlib = py.import_bound("pathlib")?;
                let path_obj = pathlib
                    .getattr("Path")?
                    .call1((source.clone().into_os_string(),))?;
                Ok(path_obj.into())
            }
        }
    }

    fn _get_source(&self) -> Option<PathBuf> {
        match &self.0 {
            lib::Location::None => Some(PathBuf::from(lib::NO_SOURCE)),
            lib::Location::Stdin { .. } => None,
            lib::Location::File { source, .. } => Some(source.clone()),
        }
    }

    #[getter]
    fn get_end(&self) -> (u32, u32) {
        match self.0 {
            lib::Location::None => (1, 1),
            lib::Location::Stdin { end, .. } | lib::Location::File { end, .. } => end.into(),
        }
    }

    #[getter]
    fn short(&self) -> Location {
        Location::new(
            self.get_start(),
            self._get_source()
                .clone()
                .map(|source| PathBuf::from(source.file_name().unwrap_or_default())),
            Some(self.get_end()),
        )
    }

    #[staticmethod]
    fn merge(positions: Vec<Self>) -> Self {
        Location(lib::Location::merge(
            positions
                .into_iter()
                .map(|l| l.0)
                .collect::<Vec<lib::Location>>()
                .as_slice(),
        ))
    }
}

impl_states!(Location);
