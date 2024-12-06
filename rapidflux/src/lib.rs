#![deny(clippy::pedantic)]
#![allow(
    clippy::trivially_copy_pass_by_ref,
    clippy::type_complexity,
    clippy::unsafe_derive_deserialize,
    clippy::unused_self
)]
#![forbid(unsafe_code)]

use pyo3::prelude::*;

mod consts;
mod diagnostics;
mod expr;
mod identifier;
mod logging;
mod source_code;
mod ty;
mod utils;

#[pymodule]
fn rapidflux(py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    // Location
    m.add_class::<diagnostics::location::Location>()?;
    let _ = m.add("NO_LOCATION", diagnostics::location::NO_LOCATION);

    // Error
    m.add_class::<diagnostics::error::Severity>()?;
    m.add_class::<diagnostics::error::Annotation>()?;
    m.add_class::<diagnostics::error::Entry>()?;
    m.add_class::<diagnostics::error::Error>()?;
    m.add(
        "FatalError",
        py.get_type_bound::<diagnostics::error::FatalError>(),
    )?;

    // Constants module
    register_submodule!(consts, py, m);

    // Expr module
    register_submodule!(expr, py, m);

    // Logging module
    register_submodule!(logging, py, m);

    // Source code module
    register_submodule!(source_code, py, m);

    // Ty module
    register_submodule!(ty, py, m);

    // Identifier
    m.add_class::<identifier::ID>()?;

    Ok(())
}
