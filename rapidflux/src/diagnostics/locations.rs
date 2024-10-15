use std::path::PathBuf;

use bincode::{deserialize, serialize};
use lazy_static::lazy_static;
use librapidflux::diagnostics as lib;
use pyo3::{
    prelude::*,
    types::{PyBool, PyBytes, PyNone, PyNotImplemented},
};
use serde::{Deserialize, Serialize};

use crate::impl_states;

lazy_static! {
    pub static ref UNKNOWN_LOCATION: Location = Location(lib::UNKNOWN_LOCATION.clone());
}

#[pyclass(module = "rflx.rapidflux")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Location(pub(crate) lib::Location);

#[pymethods]
impl Location {
    #[new]
    fn new(start: (u32, u32), source: Option<PathBuf>, end: Option<(u32, u32)>) -> Self {
        Location(lib::Location::new(
            start.into(),
            if let Some(e) = end {
                e.into()
            } else {
                start.into()
            },
            source,
        ))
    }

    fn __hash__(&self) -> usize {
        0
    }

    pub(crate) fn __repr__(&self) -> String {
        format!(
            "Location({:?}, {}, {:?})",
            std::convert::Into::<(u32, u32)>::into(self.0.start),
            self.0
                .source
                .as_ref()
                .map_or("None".to_string(), |s| format!(
                    "\"{}\"",
                    s.to_string_lossy()
                )),
            std::convert::Into::<(u32, u32)>::into(self.0.end)
        )
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
            Ok(PyBool::new_bound(py, self.0.start < other_location.0.start)
                .to_owned()
                .into())
        } else {
            Ok(PyNotImplemented::get_bound(py).to_owned().into())
        }
    }

    fn __getnewargs__(&self) -> ((u32, u32), Option<PathBuf>, Option<(u32, u32)>) {
        (
            self.0.start.into(),
            self.0.source.clone(),
            Some(self.0.end.into()),
        )
    }

    #[getter]
    fn get_start(&self) -> (u32, u32) {
        self.0.start.into()
    }

    #[getter]
    fn get_source(&self, py: Python<'_>) -> PyResult<PyObject> {
        match &self.0.source {
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

    #[getter]
    fn get_end(&self) -> (u32, u32) {
        self.0.start.into()
    }

    #[getter]
    fn short(&self) -> Location {
        Location::new(
            self.0.start.into(),
            self.0
                .source
                .clone()
                .map(|source| PathBuf::from(source.file_name().unwrap_or_default())),
            Some(self.0.end.into()),
        )
    }

    #[staticmethod]
    fn merge(positions: Vec<Option<Self>>) -> Option<Self> {
        lib::Location::merge(
            positions
                .into_iter()
                .flatten()
                .map(|l| l.0)
                .collect::<Vec<lib::Location>>()
                .as_slice(),
        )
        .map(Location)
    }
}

impl_states!(Location);
