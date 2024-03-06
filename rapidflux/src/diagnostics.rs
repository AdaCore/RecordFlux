use core::fmt;
use std::path::PathBuf;

use bincode::{deserialize, serialize};
use pyo3::prelude::*;
use pyo3::types::{PyBool, PyBytes, PyNone, PyNotImplemented};
use serde::{Deserialize, Serialize};

#[pyclass(module = "rflx.rapidflux")]
#[allow(clippy::unsafe_derive_deserialize)]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Location {
    source: Option<PathBuf>,
    #[pyo3(get)]
    start: (i32, i32),
    #[pyo3(get)]
    end: Option<(i32, i32)>,
}

#[pymethods]
impl Location {
    #[new]
    #[cfg(not(tarpaulin_include))]
    fn py_new(start: (i32, i32), source: Option<PathBuf>, end: Option<(i32, i32)>) -> Self {
        Location { source, start, end }
    }

    #[allow(clippy::unused_self)]
    #[cfg(not(tarpaulin_include))]
    fn __hash__(&self) -> usize {
        0
    }

    #[cfg(not(tarpaulin_include))]
    fn __repr__(&self) -> String {
        format!(
            "Location({:?}, {}, {})",
            self.start,
            if let Some(source) = &self.source {
                format!("{:?}", source.clone().into_os_string())
            } else {
                "None".to_string()
            },
            if let Some(end) = self.end {
                format!("{end:?}")
            } else {
                "None".to_string()
            }
        )
    }

    #[cfg(not(tarpaulin_include))]
    fn __str__(&self) -> String {
        self.to_string()
    }

    #[cfg(not(tarpaulin_include))]
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

    #[cfg(not(tarpaulin_include))]
    fn __lt__(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyResult<PyObject> {
        if other.is_instance_of::<Location>() {
            let other_location = other.extract::<Location>()?;
            Ok(PyBool::new_bound(py, self.start < other_location.start)
                .to_owned()
                .into())
        } else {
            Ok(PyNotImplemented::get_bound(py).to_owned().into())
        }
    }

    #[cfg(not(tarpaulin_include))]
    fn __setstate__(&mut self, state: &Bound<'_, PyBytes>) {
        *self = deserialize(state.as_bytes()).unwrap();
    }

    #[cfg(not(tarpaulin_include))]
    fn __getstate__<'py>(&self, py: Python<'py>) -> Bound<'py, PyBytes> {
        PyBytes::new_bound(py, &serialize(&self).unwrap())
    }

    #[allow(clippy::type_complexity)]
    #[cfg(not(tarpaulin_include))]
    fn __getnewargs__(&self) -> ((i32, i32), Option<PathBuf>, Option<(i32, i32)>) {
        (self.start, self.source.clone(), self.end)
    }

    #[getter]
    #[cfg(not(tarpaulin_include))]
    fn get_source(&self, py: Python<'_>) -> PyResult<PyObject> {
        match &self.source {
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
    #[cfg(not(tarpaulin_include))]
    fn short(&self) -> Location {
        Location::py_new(
            self.start,
            self.source
                .clone()
                .map(|source| PathBuf::from(source.file_name().unwrap_or_default())),
            self.end,
        )
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn linecol_str(linecol: (i32, i32)) -> String {
            format!("{}:{}", linecol.0, linecol.1)
        }
        let start = format!(":{}", linecol_str(self.start));
        let source = if let Some(source) = &self.source {
            source.to_str().unwrap_or_default().to_string()
        } else {
            String::from("<stdin>")
        };
        write!(f, "{source}{start}")
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::diagnostics::Location;

    #[test]
    fn test_location() {
        let location = Location {
            start: (1, 2),
            source: Some(PathBuf::from("foo")),
            end: Some((3, 4)),
        };
        assert_eq!(location.start, (1, 2));
        assert_eq!(location.source, Some(PathBuf::from("foo")));
        assert_eq!(location.end, Some((3, 4)));
    }

    #[test]
    fn test_location_display() {
        assert_eq!(
            Location {
                start: (1, 2),
                source: None,
                end: None,
            }
            .to_string(),
            "<stdin>:1:2"
        );
        assert_eq!(
            Location {
                start: (1, 2),
                source: Some(PathBuf::from("foo")),
                end: None,
            }
            .to_string(),
            "foo:1:2"
        );
        assert_eq!(
            Location {
                start: (1, 2),
                source: Some(PathBuf::from("foo")),
                end: Some((3, 4)),
            }
            .to_string(),
            "foo:1:2"
        );
    }
}
