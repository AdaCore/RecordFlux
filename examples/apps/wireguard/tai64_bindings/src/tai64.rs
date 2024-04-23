use pyo3::{exceptions::PyValueError, prelude::*, types::PyBytes};

#[pyclass]
pub struct Tai64(tai64::Tai64);

#[pymethods]
impl Tai64 {
    #[classattr]
    const UNIX_EPOCH: Self = Self(tai64::Tai64::UNIX_EPOCH);

    #[classattr]
    const BYTES_SIZE: usize = tai64::Tai64::BYTE_SIZE;

    #[staticmethod]
    pub fn now() -> Self {
        Self(tai64::Tai64::now())
    }

    #[staticmethod]
    pub fn from_bytes(data: &[u8]) -> PyResult<Self> {
        Ok(Self(
            tai64::Tai64::from_slice(data).map_err(|e| PyValueError::new_err(e.to_string()))?,
        ))
    }

    #[staticmethod]
    pub fn from_unix(specs: i64) -> Self {
        Self(tai64::Tai64::from_unix(specs))
    }

    pub fn to_bytes<'py>(&self, py: Python<'py>) -> &'py PyBytes {
        PyBytes::new(py, &self.0.to_bytes())
    }

    pub fn to_unix(&self) -> i64 {
        self.0.to_unix()
    }

    pub fn __add__(&self, other: u64) -> Self {
        Self(self.0 + other)
    }
}
