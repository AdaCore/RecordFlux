use librapidflux::consts as lib;
use pyo3::prelude::*;

use crate::register_submodule_declarations;

register_submodule_declarations!(
    consts,
    [
        ("BUILTINS_PACKAGE", lib::BUILTINS_PACKAGE),
        ("INTERNAL_PACKAGE", lib::INTERNAL_PACKAGE),
        ("MAX_SCALAR_SIZE", lib::MAX_SCALAR_SIZE),
    ],
    [],
    []
);
