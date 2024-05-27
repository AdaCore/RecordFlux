#![deny(clippy::pedantic)]
#![allow(
    clippy::type_complexity,
    clippy::unsafe_derive_deserialize,
    clippy::unused_self
)]

use pyo3::prelude::*;

mod diagnostics;
mod identifier;
mod logging;
mod source_code;
mod utils;

#[pymodule]
fn rapidflux(py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    // Locations
    m.add_class::<diagnostics::Location>()?;

    // Errors
    m.add_class::<diagnostics::Severity>()?;
    m.add_class::<diagnostics::Annotation>()?;
    m.add_class::<diagnostics::ErrorEntry>()?;
    m.add_class::<diagnostics::RapidFluxError>()?;
    m.add("FatalError", py.get_type_bound::<diagnostics::FatalError>())?;

    // Logging module
    register_submodule!(logging, py, m);

    // Source code module
    register_submodule!(source_code, py, m);

    // Identifier
    m.add_class::<identifier::ID>()?;

    Ok(())
}
