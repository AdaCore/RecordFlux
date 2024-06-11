use bincode::{deserialize, serialize};
use pyo3::{
    exceptions::PyTypeError,
    prelude::*,
    types::{PyBool, PyBytes, PyInt, PyNotImplemented},
};
use serde::{Deserialize, Serialize};

use librapidflux::ty as lib;

use crate::{impl_states, register_submodule_classes};

#[pyclass(module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Bounds(lib::Bounds);

#[pymethods]
impl Bounds {
    /// A range containing all values with `lower <= x <= upper`.
    ///
    /// # Panics
    ///
    /// Will panic if `lower > upper`.
    #[new]
    fn new(lower: i128, upper: i128) -> Self {
        Bounds(lib::Bounds::new(lower, upper))
    }

    fn __getnewargs__(&self) -> (i128, i128) {
        (self.0.lower(), self.0.upper())
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> Py<PyAny> {
        if let Ok(other_id) = other.extract::<Bounds>() {
            PyBool::new_bound(py, self.0 == other_id.0)
                .to_owned()
                .into()
        } else {
            PyNotImplemented::get_bound(py).to_owned().into()
        }
    }

    fn __repr__(&self) -> String {
        format!("Bounds({}, {})", self.0.lower(), self.0.upper())
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __contains__(&self, item: &Bound<'_, PyAny>) -> PyResult<bool> {
        if item.is_instance_of::<Self>() {
            let item_bounds: Bounds = item.extract()?;
            Ok(self.0.contains_bounds(&item_bounds.0))
        } else if item.is_instance_of::<PyInt>() {
            let item_int: i128 = item.extract()?;
            Ok(self.0.contains_int(item_int))
        } else {
            Err(PyTypeError::new_err(format!(
                "unsupported type \"{}\" for testing membership in \"Bounds\"",
                item.get_type().name().unwrap_or_default()
            )))
        }
    }

    #[getter]
    fn lower(&self) -> i128 {
        self.0.lower()
    }

    #[getter]
    fn upper(&self) -> i128 {
        self.0.upper()
    }

    fn merge(&self, other: &Bounds) -> Bounds {
        Bounds(self.0.merge(&other.0))
    }
}

impl_states!(Bounds);
register_submodule_classes!(ty, [Bounds]);
