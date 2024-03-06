#![deny(clippy::pedantic)]
#![allow(
    clippy::match_wildcard_for_single_variants,
    clippy::must_use_candidate,
    clippy::too_many_lines,
    clippy::wildcard_imports
)]

use pyo3::prelude::*;

mod diagnostics;

#[pymodule]
#[cfg(not(tarpaulin_include))]
fn rapidflux(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<diagnostics::Location>()?;
    Ok(())
}
