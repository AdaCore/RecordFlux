use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use bincode::{deserialize, serialize};
use librapidflux::identifier as lib;
use pyo3::{
    exceptions::{PyAssertionError, PyTypeError},
    prelude::*,
    types::{PyBool, PyBytes, PyList, PyNotImplemented, PyString},
};
use serde::{Deserialize, Serialize};

use crate::{
    diagnostics::{
        error::FatalError,
        location::{Location, NO_LOCATION},
    },
    impl_states,
};

#[pyclass(module = "rflx.rapidflux")]
#[derive(Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ID(pub(crate) lib::ID);

#[pymethods]
impl ID {
    #[new]
    pub(crate) fn new(identifier: &Bound<'_, PyAny>, location: Option<Location>) -> PyResult<Self> {
        if identifier.is_instance_of::<PyString>() {
            Ok(_new(identifier.extract()?, location)?)
        } else if identifier.is_instance_of::<PyList>() {
            Ok(_new(
                &identifier.extract::<Vec<String>>()?.join("::"),
                location,
            )?)
        } else if identifier.is_instance_of::<ID>() {
            let identifier_id: ID = identifier.extract()?;
            Ok(_new(
                identifier_id.0.identifier(),
                if location.is_none() {
                    Some(Location(identifier_id.0.location().clone()))
                } else {
                    location
                },
            )?)
        } else {
            Err(PyAssertionError::new_err(format!(
                "unexpected identifier type \"{}\"",
                identifier.get_type().name().unwrap_or_default()
            )))
        }
    }

    fn __getnewargs__(&self) -> (&str, Option<Location>) {
        (
            self.0.identifier(),
            Some(Location(self.0.location().clone())),
        )
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> Py<PyAny> {
        if let Ok(other_id) = other.extract::<ID>() {
            PyBool::new_bound(py, self.0 == other_id.0)
                .to_owned()
                .into()
        } else if let Ok(other_str) = other.extract::<&str>() {
            PyBool::new_bound(py, self.0.as_ref() == other_str)
                .to_owned()
                .into()
        } else {
            PyNotImplemented::get_bound(py).to_owned().into()
        }
    }

    fn __lt__(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> Py<PyAny> {
        if let Ok(other_id) = other.extract::<ID>() {
            PyBool::new_bound(py, self.0.identifier() < other_id.0.identifier())
                .to_owned()
                .into()
        } else {
            PyNotImplemented::get_bound(py).to_owned().into()
        }
    }

    #[allow(clippy::unused_self)]
    #[allow(clippy::cast_possible_truncation)]
    fn __hash__(&self) -> usize {
        // Using a hash function significantly improves the performance compared to using `len()`
        // or just `0`.
        let mut hasher = DefaultHasher::new();
        let identifier = self.0.identifier();
        identifier.to_ascii_lowercase().hash(&mut hasher);
        hasher.finish() as usize
    }

    pub(crate) fn __repr__(&self) -> String {
        format!(
            "ID(\"{}\", {})",
            self.0.identifier(),
            self.location().__repr__(),
        )
    }

    fn __str__(&self) -> &str {
        self.0.identifier()
    }

    fn __add__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        if other.is_instance_of::<PyString>() {
            let other_str: &str = other.extract()?;
            return Ok(self._suffix(other_str)?);
        }
        if other.is_instance_of::<ID>() {
            let other_id: ID = other.extract()?;
            return Ok(if self.location() == NO_LOCATION {
                other_id._prefix(self.0.identifier())
            } else {
                self._suffix(other_id.0.identifier())
            }?);
        }
        Err(PyTypeError::new_err(format!(
            "unsupported type \"{}\" for adding suffix to \"ID\"",
            other.get_type().name().unwrap_or_default()
        )))
    }

    fn __radd__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        if other.is_instance_of::<PyString>() {
            let other_str: &str = other.extract()?;
            return Ok(self._prefix(other_str)?);
        }
        if other.is_instance_of::<ID>() {
            let other_id: ID = other.extract()?;
            return Ok(if self.location() == NO_LOCATION {
                other_id._suffix(self.0.identifier())
            } else {
                self._prefix(other_id.0.identifier())
            }?);
        }
        Err(PyTypeError::new_err(format!(
            "unsupported type \"{}\" for adding prefix to \"ID\"",
            other.get_type().name().unwrap_or_default()
        )))
    }

    fn __mul__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        if other.is_instance_of::<PyString>() {
            let other_str: &str = other.extract()?;
            return Ok(self._suffix(&(lib::ID_SEP.to_string() + other_str))?);
        }
        if other.is_instance_of::<ID>() {
            let other_id: ID = other.extract()?;
            return Ok(self._join(&other_id)?);
        }
        Err(PyTypeError::new_err(format!(
            "unsupported type \"{}\" for concatenating to \"ID\"",
            other.get_type().name().unwrap_or_default()
        )))
    }

    fn __rmul__(&self, other: &Bound<'_, PyAny>) -> PyResult<Self> {
        if other.is_instance_of::<PyString>() {
            let other_str: &str = other.extract()?;
            return Ok(self._prefix(&(other_str.to_string() + lib::ID_SEP))?);
        }
        if other.is_instance_of::<ID>() {
            let other_id: ID = other.extract()?;
            return Ok(other_id._join(self)?);
        }
        Err(PyTypeError::new_err(format!(
            "unsupported type \"{}\" for concatenating to \"ID\"",
            other.get_type().name().unwrap_or_default()
        )))
    }

    #[getter]
    fn parts(&self) -> Vec<&str> {
        self.0.parts()
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location().clone())
    }

    #[getter]
    fn name(&self) -> ID {
        ID(self.0.name().to_owned())
    }

    #[getter]
    fn parent(&self) -> PyResult<ID> {
        if let Some(parent) = self.0.parent() {
            Ok(ID(parent.to_owned()))
        } else {
            Err(FatalError::new_err("no parent"))
        }
    }

    #[getter]
    fn flat(&self) -> String {
        self.0.flat()
    }

    #[getter]
    fn ada_str(&self) -> String {
        self.0.to_ada_string()
    }

    fn _suffix(&self, suffix: &str) -> Result<Self, IDError> {
        Ok(Self(self.0.suffix(suffix)?))
    }

    fn _prefix(&self, prefix: &str) -> Result<Self, IDError> {
        Ok(Self(self.0.prefix(prefix)?))
    }

    fn _join(&self, id: &Self) -> Result<Self, IDError> {
        Ok(Self(self.0.join(&id.0)?))
    }
}

fn _new(identifier: &str, location: Option<Location>) -> Result<ID, IDError> {
    Ok(ID(lib::ID::new(
        identifier,
        location.map_or(librapidflux::diagnostics::location::Location::None, |l| l.0),
    )?))
}

pub(crate) struct IDError(lib::IDError);

impl From<IDError> for PyErr {
    fn from(error: IDError) -> Self {
        FatalError::new_err(error.0.to_string())
    }
}

impl From<lib::IDError> for IDError {
    fn from(other: lib::IDError) -> Self {
        Self(other)
    }
}

pub(crate) fn to_id(obj: &Bound<'_, PyAny>) -> Result<lib::ID, IDError> {
    if let Ok(s) = obj.extract::<String>() {
        lib::ID::new(&s, librapidflux::diagnostics::location::Location::None).map_err(IDError)
    } else if let Ok(id) = obj.extract::<ID>() {
        Ok(id.0)
    } else {
        Err(IDError(lib::IDError::InvalidIdentifier))
    }
}

impl_states!(ID);
