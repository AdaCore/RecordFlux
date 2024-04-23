use pyo3::prelude::*;

mod tai64;
mod tai64n;

/// tai64 rust crate bindings for python
#[pymodule]
fn tai64_bindings(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<tai64::Tai64>()?;
    m.add_class::<tai64n::Tai64n>()?;
    Ok(())
}
