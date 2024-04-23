use pyo3::{exceptions::PyValueError, prelude::*, types::PyBytes};

#[pyclass]
pub struct Tai64n(tai64::Tai64N);

#[pymethods]
impl Tai64n {
    #[classattr]
    const UNIX_EPOCH: Self = Self(tai64::Tai64N::UNIX_EPOCH);

    #[classattr]
    const BYTE_SIZE: usize = tai64::Tai64N::BYTE_SIZE;

    #[staticmethod]
    pub fn now() -> Self {
        Self(tai64::Tai64N::now())
    }

    #[staticmethod]
    pub fn from_bytes(data: &[u8]) -> PyResult<Self> {
        Ok(Self(
            tai64::Tai64N::from_slice(data).map_err(|e| PyValueError::new_err(e.to_string()))?,
        ))
    }

    pub fn to_bytes<'py>(&self, py: Python<'py>) -> &'py PyBytes {
        PyBytes::new(py, &self.0.to_bytes())
    }
}
