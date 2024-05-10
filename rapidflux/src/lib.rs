#![deny(clippy::pedantic)]
#![allow(
    clippy::type_complexity,
    clippy::unsafe_derive_deserialize,
    clippy::unused_self
)]

use pyo3::prelude::*;

mod error;

#[pymodule]
fn rapidflux(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<error::Location>()?;
    Ok(())
}
