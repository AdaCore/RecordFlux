use std::path::PathBuf;

use bincode::{deserialize, serialize};
use librapidflux::diagnostics as lib;
use pyo3::{
    prelude::*,
    types::{PyBool, PyBytes, PyNone, PyNotImplemented},
};
use serde::{Deserialize, Serialize};

use crate::impl_states;

#[pyclass(module = "rflx.rapidflux")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Location(pub(crate) lib::Location);

#[pymethods]
impl Location {
    #[new]
    fn py_new(start: (u32, u32), source: Option<PathBuf>, end: Option<(u32, u32)>) -> Self {
        Location(lib::Location {
            source,
            start: start.into(),
            end: end.map(std::convert::Into::into),
        })
    }

    fn __hash__(&self) -> usize {
        0
    }

    pub(crate) fn __repr__(&self) -> String {
        format!(
            "Location({:?}, {}, {})",
            std::convert::Into::<(u32, u32)>::into(self.0.start),
            self.0
                .source
                .as_ref()
                .map_or("None".to_string(), |s| format!(
                    "\"{}\"",
                    s.to_string_lossy()
                )),
            self.0.end.as_ref().map_or("None".to_string(), |e| {
                format!("{:?}", std::convert::Into::<(u32, u32)>::into(*e))
            })
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
            self.0.end.map(std::convert::Into::into),
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
    fn get_end(&self) -> Option<(u32, u32)> {
        self.0
            .end
            .as_ref()
            .map(|e| std::convert::Into::<(u32, u32)>::into(*e))
    }

    #[getter]
    fn short(&self) -> Location {
        Location::py_new(
            self.0.start.into(),
            self.0
                .source
                .clone()
                .map(|source| PathBuf::from(source.file_name().unwrap_or_default())),
            self.0.end.map(std::convert::Into::into),
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
