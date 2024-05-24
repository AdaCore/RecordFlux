#![deny(clippy::pedantic)]
#![allow(
    clippy::type_complexity,
    clippy::unsafe_derive_deserialize,
    clippy::unused_self
)]

use pyo3::prelude::*;

mod diagnostics;
mod utils;

#[pymodule]
fn rapidflux(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    // Locations
    m.add_class::<diagnostics::Location>()?;

    // Errors
    m.add_class::<diagnostics::Severity>()?;
    m.add_class::<diagnostics::Annotation>()?;
    m.add_class::<diagnostics::ErrorEntry>()?;
    m.add_class::<diagnostics::RapidFluxError>()?;
    Ok(())
}
