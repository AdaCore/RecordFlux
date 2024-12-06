use std::collections::HashSet;

use bincode::{deserialize, serialize};
use indexmap::IndexMap;
use librapidflux::ty as lib;
use pyo3::{
    exceptions::PyTypeError,
    prelude::*,
    sync::GILOnceCell,
    types::{PyBool, PyBytes, PyInt, PyList, PyNotImplemented, PySet, PyTuple, PyType},
    PyTypeInfo,
};
use serde::{Deserialize, Serialize};

use crate::{
    diagnostics::{
        error::{Annotation, Error},
        location::{Location, NO_LOCATION},
    },
    identifier::{to_id, ID},
    impl_states, register_submodule_declarations,
};

#[pyclass(subclass, module = "rflx.rapidflux.ty")]
pub struct Builtins;

#[pymethods]
impl Builtins {
    #[classattr]
    #[pyo3(name = "UNDEFINED")]
    fn undefined(py: Python<'_>) -> &PyObject {
        static UNDEFINED: GILOnceCell<PyObject> = GILOnceCell::new();
        UNDEFINED.get_or_init(py, || to_py(&lib::Ty::Undefined, py))
    }

    #[classattr]
    #[pyo3(name = "BOOLEAN")]
    fn boolean(py: Python<'_>) -> &PyObject {
        static BOOLEAN: GILOnceCell<PyObject> = GILOnceCell::new();
        BOOLEAN.get_or_init(py, || to_py(&lib::BOOLEAN, py))
    }

    #[classattr]
    #[pyo3(name = "INDEX")]
    fn index(py: Python<'_>) -> &PyObject {
        static INDEX: GILOnceCell<PyObject> = GILOnceCell::new();
        INDEX.get_or_init(py, || to_py(&lib::INDEX, py))
    }

    #[classattr]
    #[pyo3(name = "BIT_LENGTH")]
    fn bit_length(py: Python<'_>) -> &PyObject {
        static BIT_LENGTH: GILOnceCell<PyObject> = GILOnceCell::new();
        BIT_LENGTH.get_or_init(py, || to_py(&lib::BIT_LENGTH, py))
    }

    #[classattr]
    #[pyo3(name = "BIT_INDEX")]
    fn bit_index(py: Python<'_>) -> &PyObject {
        static BIT_INDEX: GILOnceCell<PyObject> = GILOnceCell::new();
        BIT_INDEX.get_or_init(py, || to_py(&lib::BIT_INDEX, py))
    }

    #[classattr]
    #[pyo3(name = "UNIVERSAL_INTEGER")]
    fn universal_integer(py: Python<'_>) -> &PyObject {
        static UNIVERSAL_INTEGER: GILOnceCell<PyObject> = GILOnceCell::new();
        UNIVERSAL_INTEGER.get_or_init(py, || to_py(&lib::UNIVERSAL_INTEGER, py))
    }

    #[classattr]
    #[pyo3(name = "BASE_INTEGER")]
    fn base_integer(py: Python<'_>) -> &PyObject {
        static BASE_INTEGER: GILOnceCell<PyObject> = GILOnceCell::new();
        BASE_INTEGER.get_or_init(py, || to_py(&lib::BASE_INTEGER, py))
    }

    #[classattr]
    #[pyo3(name = "OPAQUE")]
    fn opaque(py: Python<'_>) -> &PyObject {
        static OPAQUE: GILOnceCell<PyObject> = GILOnceCell::new();
        OPAQUE.get_or_init(py, || to_py(&lib::OPAQUE, py))
    }
}

#[pyclass(subclass, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Type;

#[pymethods]
impl Type {
    #[new]
    fn new() -> Self {
        Self
    }
}

#[pyclass(extends=Type, module = "rflx.rapidflux.ty")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Undefined;

#[pymethods]
impl Undefined {
    #[classattr]
    #[pyo3(name = "DESCRIPTIVE_NAME")]
    fn descriptive_name() -> String {
        lib::Ty::Undefined.to_string()
    }

    #[new]
    fn new() -> (Self, Type) {
        (Self, Type::new())
    }

    fn __getnewargs__(&self, py: Python<'_>) -> PyObject {
        PyTuple::empty_bound(py).into()
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Undefined == to_ty(other)
    }

    fn __repr__(&self) -> String {
        "Undefined()".to_string()
    }

    fn __str__(&self) -> String {
        Self::descriptive_name()
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Undefined.is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Undefined.is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(&lib::Ty::Undefined.common_type(&to_ty(other)), py)
    }
}

#[pyclass(extends=Type, subclass, module = "rflx.rapidflux.ty")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Any;

#[pymethods]
impl Any {
    #[classattr]
    #[pyo3(name = "DESCRIPTIVE_NAME")]
    fn descriptive_name() -> String {
        lib::Ty::Any.to_string()
    }

    #[new]
    fn new() -> (Self, Type) {
        (Self, Type::new())
    }

    fn __getnewargs__(&self, py: Python<'_>) -> PyObject {
        PyTuple::empty_bound(py).into()
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Any == to_ty(other)
    }

    fn __repr__(&self) -> String {
        "Any()".to_string()
    }

    fn __str__(&self) -> String {
        Self::descriptive_name()
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Any.is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Any.is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(&lib::Ty::Any.common_type(&to_ty(other)), py)
    }
}

#[pyclass(extends=Any, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Enumeration(lib::Enumeration);

#[pymethods]
impl Enumeration {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = lib::Enumeration::DESCRIPTIVE_NAME;

    #[new]
    #[pyo3(signature = (identifier, literals, always_valid = false, location = None))]
    #[allow(clippy::needless_pass_by_value)]
    fn new(
        identifier: &Bound<'_, PyAny>,
        literals: Vec<ID>,
        always_valid: bool,
        location: Option<Location>,
    ) -> PyResult<PyClassInitializer<Self>> {
        Ok(
            PyClassInitializer::from(Any::new()).add_subclass(Self(lib::Enumeration {
                id: to_id(identifier)?,
                literals: literals.iter().map(|l| l.0.clone()).collect(),
                always_valid,
                location: location.map_or(NO_LOCATION.0, |l| l.0),
            })),
        )
    }

    fn __getnewargs__(&self) -> (ID, Vec<ID>, bool, Option<Location>) {
        (
            self.identifier(),
            self.literals(),
            self.always_valid(),
            Some(self.location()),
        )
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        matches!(to_ty_opt(other), Some(lib::Ty::Enumeration(e)) if e == self.0)
    }

    fn __repr__(&self) -> String {
        format!(
            "Enumeration(\"{}\", [{}], {})",
            self.0.id,
            self.0
                .literals
                .iter()
                .map(|l| format!("ID(\"{l}\")"))
                .collect::<Vec<String>>()
                .join(", "),
            self.0.always_valid,
        )
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    #[getter]
    fn identifier(&self) -> ID {
        ID(self.0.id.clone())
    }

    #[getter]
    fn literals(&self) -> Vec<ID> {
        self.0
            .literals
            .iter()
            .map(|i| ID(i.clone()))
            .collect::<Vec<_>>()
    }

    #[getter]
    fn always_valid(&self) -> bool {
        self.0.always_valid
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location.clone())
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Enumeration(self.0.clone()).is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Enumeration(self.0.clone()).is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Ty::Enumeration(self.0.clone()).common_type(&to_ty(other)),
            py,
        )
    }
}

#[pyclass(extends=Any, subclass, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct AnyInteger;

#[pymethods]
impl AnyInteger {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = "integer type";

    #[new]
    fn new() -> PyClassInitializer<Self> {
        PyClassInitializer::from(Any::new()).add_subclass(Self)
    }
}

#[pyclass(extends=AnyInteger, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct UniversalInteger(lib::UniversalInteger);

#[pymethods]
impl UniversalInteger {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = lib::UniversalInteger::DESCRIPTIVE_NAME;

    #[new]
    fn new(bounds: Bounds) -> PyClassInitializer<Self> {
        AnyInteger::new().add_subclass(Self(lib::UniversalInteger { bounds: bounds.0 }))
    }

    fn __getnewargs__(&self) -> (Bounds,) {
        (self.bounds(),)
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        matches!(to_ty_opt(other), Some(lib::Ty::UniversalInteger(i)) if i == self.0)
    }

    fn __repr__(&self) -> String {
        format!("UniversalInteger({})", self.0.bounds)
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    #[getter]
    fn bounds(&self) -> Bounds {
        Bounds(self.0.bounds.clone())
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::UniversalInteger(self.0.clone()).is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::UniversalInteger(self.0.clone()).is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Ty::UniversalInteger(self.0.clone()).common_type(&to_ty(other)),
            py,
        )
    }
}

#[pyclass(extends=AnyInteger, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Integer(lib::Integer);

#[pymethods]
impl Integer {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = lib::Integer::DESCRIPTIVE_NAME;

    #[new]
    #[pyo3(signature = (identifier, bounds, location = None))]
    fn new(
        identifier: &Bound<'_, PyAny>,
        bounds: Bounds,
        location: Option<Location>,
    ) -> PyResult<PyClassInitializer<Self>> {
        Ok(AnyInteger::new().add_subclass(Self(lib::Integer {
            id: to_id(identifier)?,
            bounds: bounds.0,
            location: location.map_or(NO_LOCATION.0, |l| l.0),
        })))
    }

    fn __getnewargs__(&self) -> (ID, Bounds, Option<Location>) {
        (self.identifier(), self.bounds(), Some(self.location()))
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        matches!(to_ty_opt(other), Some(lib::Ty::Integer(i)) if i == self.0)
    }

    fn __repr__(&self) -> String {
        format!("Integer(\"{}\", {})", self.0.id, self.0.bounds)
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    #[getter]
    fn identifier(&self) -> ID {
        ID(self.0.id.clone())
    }

    #[getter]
    fn bounds(&self) -> Bounds {
        Bounds(self.0.bounds.clone())
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location.clone())
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Integer(self.0.clone()).is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Integer(self.0.clone()).is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Ty::Integer(self.0.clone()).common_type(&to_ty(other)),
            py,
        )
    }
}

/// Base type for any type consisting of a sequence of elements with the same type.
#[pyclass(extends=Any, subclass, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Composite;

#[pymethods]
impl Composite {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = "composite type";

    #[new]
    fn new() -> PyClassInitializer<Self> {
        PyClassInitializer::from(Any::new()).add_subclass(Self)
    }
}

#[pyclass(extends=Composite, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Aggregate(lib::Aggregate);

#[pymethods]
impl Aggregate {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = lib::Aggregate::DESCRIPTIVE_NAME;

    #[new]
    fn new(element: &Bound<'_, PyAny>) -> PyClassInitializer<Self> {
        Composite::new().add_subclass(Self(lib::Aggregate {
            element: Box::new(to_ty(element)),
        }))
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (PyObject,) {
        (self.element(py),)
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        matches!(to_ty_opt(other), Some(lib::Ty::Aggregate(a)) if a == self.0)
    }

    fn __repr__(&self) -> String {
        format!("Aggregate({})", self.0.element)
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    #[getter]
    fn element(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.element, py)
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Aggregate(self.0.clone()).is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Aggregate(self.0.clone()).is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Ty::Aggregate(self.0.clone()).common_type(&to_ty(other)),
            py,
        )
    }
}

#[pyclass(extends=Composite, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Sequence(lib::Sequence);

#[pymethods]
impl Sequence {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = lib::Sequence::DESCRIPTIVE_NAME;

    #[new]
    fn new(
        identifier: &Bound<'_, PyAny>,
        element: &Bound<'_, PyAny>,
    ) -> PyResult<PyClassInitializer<Self>> {
        Ok(Composite::new().add_subclass(Self(lib::Sequence {
            id: to_id(identifier)?,
            element: Box::new(to_ty(element)),
        })))
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (ID, PyObject) {
        (self.identifier(), self.element(py))
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        matches!(to_ty_opt(other), Some(lib::Ty::Sequence(s)) if s == self.0)
    }

    fn __repr__(&self) -> String {
        format!("Sequence(\"{}\", {})", self.0.id, self.0.element)
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    #[getter]
    fn identifier(&self) -> ID {
        ID(self.0.id.clone())
    }

    #[getter]
    fn element(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.element, py)
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Sequence(self.0.clone()).is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Sequence(self.0.clone()).is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Ty::Sequence(self.0.clone()).common_type(&to_ty(other)),
            py,
        )
    }
}

/// Base type for any type consisting of multiple fields of different types.
#[pyclass(extends=Any, subclass, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Compound;

#[pymethods]
impl Compound {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = "compound type";

    #[new]
    fn new() -> PyClassInitializer<Self> {
        PyClassInitializer::from(Any::new()).add_subclass(Self)
    }
}

#[pyclass(extends=Compound, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Structure(lib::Structure);

#[pymethods]
impl Structure {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = lib::Structure::DESCRIPTIVE_NAME;

    #[new]
    #[pyo3(signature = (identifier, field_combinations = None, parameter_types = None, field_types = None))]
    fn new(
        identifier: &Bound<'_, PyAny>,
        field_combinations: Option<HashSet<Vec<String>>>,
        parameter_types: Option<IndexMap<ID, PyObject>>,
        field_types: Option<IndexMap<ID, PyObject>>,
        py: Python<'_>,
    ) -> PyResult<PyClassInitializer<Self>> {
        Ok(Compound::new().add_subclass(Self(lib::Structure {
            id: to_id(identifier)?,
            field_combinations: field_combinations.unwrap_or_default(),
            parameter_types: parameter_types
                .unwrap_or_default()
                .iter()
                .map(|(k, v)| (k.0.clone(), Box::new(to_ty(v.bind(py)))))
                .collect(),
            field_types: field_types
                .unwrap_or_default()
                .iter()
                .map(|(k, v)| (k.0.clone(), Box::new(to_ty(v.bind(py)))))
                .collect(),
        })))
    }

    fn __getnewargs__(
        &self,
        py: Python<'_>,
    ) -> PyResult<(
        ID,
        Option<PyObject>,
        Option<IndexMap<ID, PyObject>>,
        Option<IndexMap<ID, PyObject>>,
    )> {
        Ok((
            self.identifier(),
            Some(self.field_combinations(py)?),
            Some(self.parameter_types(py)),
            Some(self.field_types(py)),
        ))
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        matches!(to_ty_opt(other), Some(lib::Ty::Structure(s)) if s == self.0)
    }

    fn __repr__(&self) -> String {
        format!("Structure(\"{}\")", self.0.id)
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    #[getter]
    fn identifier(&self) -> ID {
        ID(self.0.id.clone())
    }

    #[getter]
    fn field_combinations(&self, py: Python<'_>) -> PyResult<PyObject> {
        Ok(PySet::new_bound(
            py,
            &self
                .0
                .field_combinations
                .iter()
                .map(|c| PyTuple::new_bound(py, c))
                .collect::<Vec<_>>(),
        )?
        .into())
    }

    #[getter]
    fn parameter_types(&self, py: Python<'_>) -> IndexMap<ID, PyObject> {
        self.0
            .parameter_types
            .iter()
            .map(|(id, ty)| (ID(id.clone()), to_py(ty, py)))
            .collect::<IndexMap<_, _>>()
    }

    #[getter]
    fn field_types(&self, py: Python<'_>) -> IndexMap<ID, PyObject> {
        self.0
            .field_types
            .iter()
            .map(|(id, ty)| (ID(id.clone()), to_py(ty, py)))
            .collect::<IndexMap<_, _>>()
    }

    #[getter]
    fn parameters(&self) -> HashSet<ID> {
        self.0
            .parameter_types
            .keys()
            .map(|id| ID(id.clone()))
            .collect()
    }

    #[getter]
    fn fields(&self) -> HashSet<ID> {
        self.0.field_types.keys().map(|id| ID(id.clone())).collect()
    }

    #[getter]
    fn types(&self, py: Python<'_>) -> IndexMap<ID, PyObject> {
        self.0
            .parameter_types
            .iter()
            .chain(self.0.field_types.iter())
            .map(|(id, ty)| (ID(id.clone()), to_py(ty, py)))
            .collect()
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Structure(self.0.clone()).is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Structure(self.0.clone()).is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Ty::Structure(self.0.clone()).common_type(&to_ty(other)),
            py,
        )
    }
}

#[pyclass(extends=Compound, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Message(lib::Message);

#[pymethods]
impl Message {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = lib::Message::DESCRIPTIVE_NAME;

    #[new]
    #[pyo3(signature = (identifier, field_combinations = None, parameter_types = None, field_types = None, refinements = None, is_definite = false))]
    fn new(
        identifier: &Bound<'_, PyAny>,
        field_combinations: Option<HashSet<Vec<String>>>,
        parameter_types: Option<IndexMap<ID, PyObject>>,
        field_types: Option<IndexMap<ID, PyObject>>,
        refinements: Option<Vec<Refinement>>,
        is_definite: bool,
        py: Python<'_>,
    ) -> PyResult<PyClassInitializer<Self>> {
        Ok(Compound::new().add_subclass(Self(lib::Message {
            id: to_id(identifier)?,
            field_combinations: field_combinations.unwrap_or_default(),
            parameter_types: parameter_types
                .unwrap_or_default()
                .iter()
                .map(|(k, v)| (k.0.clone(), Box::new(to_ty(v.bind(py)))))
                .collect(),
            field_types: field_types
                .unwrap_or_default()
                .iter()
                .map(|(k, v)| (k.0.clone(), Box::new(to_ty(v.bind(py)))))
                .collect(),
            refinements: refinements
                .unwrap_or_default()
                .iter()
                .map(|r| r.0.clone())
                .collect(),
            is_definite,
        })))
    }

    fn __getnewargs__(
        &self,
        py: Python<'_>,
    ) -> PyResult<(
        ID,
        Option<PyObject>,
        Option<IndexMap<ID, PyObject>>,
        Option<IndexMap<ID, PyObject>>,
        Option<Vec<Refinement>>,
        bool,
    )> {
        Ok((
            self.identifier(),
            Some(self.field_combinations(py)?),
            Some(self.parameter_types(py)),
            Some(self.field_types(py)),
            Some(self.refinements()),
            self.is_definite(),
        ))
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        matches!(to_ty_opt(other), Some(lib::Ty::Message(m)) if m == self.0)
    }

    fn __repr__(&self) -> String {
        format!("Message(\"{}\")", self.0.id)
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    #[getter]
    fn identifier(&self) -> ID {
        ID(self.0.id.clone())
    }

    #[getter]
    fn field_combinations(&self, py: Python<'_>) -> PyResult<PyObject> {
        Ok(PySet::new_bound(
            py,
            &self
                .0
                .field_combinations
                .iter()
                .map(|c| PyTuple::new_bound(py, c))
                .collect::<Vec<_>>(),
        )?
        .into())
    }

    #[getter]
    fn parameter_types(&self, py: Python<'_>) -> IndexMap<ID, PyObject> {
        self.0
            .parameter_types
            .iter()
            .map(|(id, ty)| (ID(id.clone()), to_py(ty, py)))
            .collect::<IndexMap<_, _>>()
    }

    #[getter]
    fn field_types(&self, py: Python<'_>) -> IndexMap<ID, PyObject> {
        self.0
            .field_types
            .iter()
            .map(|(id, ty)| (ID(id.clone()), to_py(ty, py)))
            .collect::<IndexMap<_, _>>()
    }

    #[getter]
    fn refinements(&self) -> Vec<Refinement> {
        self.0
            .refinements
            .iter()
            .map(|r| Refinement(r.clone()))
            .collect::<Vec<_>>()
    }

    #[getter]
    fn is_definite(&self) -> bool {
        self.0.is_definite
    }

    #[getter]
    fn parameters(&self) -> HashSet<ID> {
        self.0
            .parameter_types
            .keys()
            .map(|id| ID(id.clone()))
            .collect()
    }

    #[getter]
    fn fields(&self) -> HashSet<ID> {
        self.0.field_types.keys().map(|id| ID(id.clone())).collect()
    }

    #[getter]
    fn types(&self, py: Python<'_>) -> IndexMap<ID, PyObject> {
        self.0
            .parameter_types
            .iter()
            .chain(self.0.field_types.iter())
            .map(|(id, ty)| (ID(id.clone()), to_py(ty, py)))
            .collect()
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Message(self.0.clone()).is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Message(self.0.clone()).is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Ty::Message(self.0.clone()).common_type(&to_ty(other)),
            py,
        )
    }
}

#[pyclass(module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Refinement(lib::Refinement);

#[pymethods]
impl Refinement {
    #[new]
    fn new(field: &Bound<'_, PyAny>, sdu: Message, package: &Bound<'_, PyAny>) -> PyResult<Self> {
        Ok(Self(lib::Refinement {
            field: to_id(field)?,
            sdu: sdu.0,
            package: to_id(package)?,
        }))
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (ID, PyObject, ID) {
        (self.field(), self.sdu(py), self.package())
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        if let Ok(other_refinement) = other.extract::<Refinement>() {
            PyBool::new_bound(py, self.0 == other_refinement.0)
                .to_owned()
                .into()
        } else {
            PyNotImplemented::get_bound(py).to_owned().into()
        }
    }

    fn __repr__(&self) -> String {
        format!(
            "Refinement({}, {}, {})",
            self.field().__repr__(),
            Message(self.0.sdu.clone()).__repr__(),
            self.package().__repr__(),
        )
    }

    #[getter]
    fn field(&self) -> ID {
        ID(self.0.field.clone())
    }

    #[getter]
    fn sdu(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Ty::Message(self.0.sdu.clone()), py)
    }

    #[getter]
    fn package(&self) -> ID {
        ID(self.0.package.clone())
    }
}

#[pyclass(extends=Any, module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Channel(lib::Channel);

#[pymethods]
impl Channel {
    #[classattr]
    const DESCRIPTIVE_NAME: &'static str = lib::Channel::DESCRIPTIVE_NAME;

    #[new]
    fn new(readable: bool, writable: bool) -> PyClassInitializer<Self> {
        PyClassInitializer::from(Any::new()).add_subclass(Self(lib::Channel { readable, writable }))
    }

    fn __getnewargs__(&self) -> (bool, bool) {
        (self.0.readable, self.0.writable)
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        matches!(to_ty_opt(other), Some(lib::Ty::Channel(c)) if c == self.0)
    }

    fn __repr__(&self) -> String {
        format!("Channel({}, {})", self.0.readable, self.0.writable)
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    #[getter]
    fn readable(&self) -> bool {
        self.0.readable
    }

    #[getter]
    fn writable(&self) -> bool {
        self.0.writable
    }

    fn is_compatible(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Channel(self.0.clone()).is_compatible(&to_ty(other))
    }

    fn is_compatible_strong(&self, other: &Bound<'_, PyAny>) -> bool {
        lib::Ty::Channel(self.0.clone()).is_compatible_strong(&to_ty(other))
    }

    fn common_type(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Ty::Channel(self.0.clone()).common_type(&to_ty(other)),
            py,
        )
    }
}

#[pyclass(module = "rflx.rapidflux.ty")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Bounds(lib::Bounds);

#[pymethods]
impl Bounds {
    /// A range containing all values with `lower <= x <= upper`.
    ///
    /// # Panics
    ///
    /// Will panic if `lower > upper`.
    #[new]
    fn new(lower: i128, upper: i128) -> Self {
        Bounds(lib::Bounds::new(lower, upper))
    }

    fn __getnewargs__(&self) -> (i128, i128) {
        (self.0.lower(), self.0.upper())
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        if let Ok(other_id) = other.extract::<Bounds>() {
            PyBool::new_bound(py, self.0 == other_id.0)
                .to_owned()
                .into()
        } else {
            PyNotImplemented::get_bound(py).to_owned().into()
        }
    }

    fn __repr__(&self) -> String {
        format!("Bounds({}, {})", self.0.lower(), self.0.upper())
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __contains__(&self, item: &Bound<'_, PyAny>) -> PyResult<bool> {
        if item.is_instance_of::<Self>() {
            let item_bounds: Bounds = item.extract()?;
            Ok(self.0.contains_bounds(&item_bounds.0))
        } else if item.is_instance_of::<PyInt>() {
            let item_int: i128 = item.extract()?;
            Ok(self.0.contains_int(item_int))
        } else {
            Err(PyTypeError::new_err(format!(
                "unsupported type \"{}\" for testing membership in \"Bounds\"",
                item.get_type().name().unwrap_or_default()
            )))
        }
    }

    #[getter]
    fn lower(&self) -> i128 {
        self.0.lower()
    }

    #[getter]
    fn upper(&self) -> i128 {
        self.0.upper()
    }

    fn merge(&self, other: &Bounds) -> Bounds {
        Bounds(self.0.merge(&other.0))
    }
}

#[pyfunction]
fn common_type(types: &Bound<'_, PyList>, py: Python<'_>) -> PyObject {
    to_py(
        &lib::common_type(&types.iter().map(|t| to_ty(&t)).collect::<Vec<_>>()),
        py,
    )
}

#[pyfunction]
#[pyo3(signature = (actual, expected, location, description = ""))]
#[allow(clippy::needless_pass_by_value)]
fn check_type(
    actual: &Bound<'_, PyAny>,
    expected: &Bound<'_, PyAny>,
    location: &Location,
    description: &str,
) -> Error {
    let expected = if let Ok(tuple) = expected.extract::<Vec<Bound<'_, PyAny>>>() {
        tuple
    } else if let Ok(ty) = expected.extract::<Bound<'_, PyAny>>() {
        Vec::from([ty])
    } else {
        panic!("unexpected argument type for expected: {expected}")
    };
    Error(lib::check_type(
        &to_ty(actual),
        &expected.iter().map(|e| to_ty(e)).collect::<Vec<_>>(),
        &location.0,
        description,
    ))
}

#[pyfunction]
#[pyo3(signature = (actual, expected, location, description = "", additional_annotations = None))]
fn check_type_instance(
    actual: &Bound<'_, PyAny>,
    expected: &Bound<'_, PyAny>,
    location: &Location,
    description: &str,
    additional_annotations: Option<Vec<Annotation>>,
    py: Python<'_>,
) -> Error {
    let expected = if let Ok(tuple) = expected.extract::<Vec<Bound<'_, PyType>>>() {
        tuple
    } else if let Ok(ty) = expected.extract::<Bound<'_, PyType>>() {
        Vec::from([ty])
    } else {
        panic!("unexpected argument type for expected: {expected}")
    };
    Error(lib::check_type_instance(
        &to_ty(actual),
        &expected
            .iter()
            .map(|e| to_ty_discriminants(e, py))
            .collect::<Vec<_>>(),
        &location.0,
        description,
        &additional_annotations
            .unwrap_or_default()
            .into_iter()
            .map(|a| a.0)
            .collect::<Vec<_>>(),
    ))
}

pub(crate) fn to_ty(obj: &Bound<'_, PyAny>) -> lib::Ty {
    to_ty_opt(obj).unwrap_or_else(|| panic!("Unknown type \"{obj:?}\""))
}

pub(crate) fn to_ty_opt(obj: &Bound<'_, PyAny>) -> Option<lib::Ty> {
    if obj.extract::<PyRef<Undefined>>().is_ok() {
        Some(lib::Ty::Undefined)
    } else if let Ok(enumeration) = obj.extract::<PyRef<Enumeration>>() {
        Some(lib::Ty::Enumeration(enumeration.0.clone()))
    } else if let Ok(universal_integer) = obj.extract::<PyRef<UniversalInteger>>() {
        Some(lib::Ty::UniversalInteger(universal_integer.0.clone()))
    } else if let Ok(integer) = obj.extract::<PyRef<Integer>>() {
        Some(lib::Ty::Integer(integer.0.clone()))
    } else if let Ok(aggregate) = obj.extract::<PyRef<Aggregate>>() {
        Some(lib::Ty::Aggregate(aggregate.0.clone()))
    } else if let Ok(sequence) = obj.extract::<PyRef<Sequence>>() {
        Some(lib::Ty::Sequence(sequence.0.clone()))
    } else if let Ok(structure) = obj.extract::<PyRef<Structure>>() {
        Some(lib::Ty::Structure(structure.0.clone()))
    } else if let Ok(message) = obj.extract::<PyRef<Message>>() {
        Some(lib::Ty::Message(message.0.clone()))
    } else if let Ok(channel) = obj.extract::<PyRef<Channel>>() {
        Some(lib::Ty::Channel(channel.0.clone()))
    } else if obj.extract::<PyRef<Any>>().is_ok() {
        Some(lib::Ty::Any)
    } else {
        None
    }
}

pub(crate) fn to_ty_discriminants(obj: &Bound<'_, PyAny>, py: Python<'_>) -> lib::TyDiscriminants {
    to_ty_discriminants_opt(obj, py).unwrap_or_else(|| panic!("Unknown type \"{obj:?}\""))
}

pub(crate) fn to_ty_discriminants_opt(
    obj: &Bound<'_, PyAny>,
    py: Python<'_>,
) -> Option<lib::TyDiscriminants> {
    if obj.eq(Undefined::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Undefined)
    } else if obj.eq(Any::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Any)
    } else if obj.eq(Enumeration::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Enumeration)
    } else if obj.eq(AnyInteger::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::AnyInteger)
    } else if obj.eq(UniversalInteger::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::UniversalInteger)
    } else if obj.eq(Integer::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Integer)
    } else if obj.eq(Composite::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Composite)
    } else if obj.eq(Aggregate::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Aggregate)
    } else if obj.eq(Sequence::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Sequence)
    } else if obj.eq(Compound::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Compound)
    } else if obj.eq(Structure::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Structure)
    } else if obj.eq(Message::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Message)
    } else if obj.eq(Channel::type_object_bound(py)).unwrap() {
        Some(lib::TyDiscriminants::Channel)
    } else {
        None
    }
}

pub(crate) fn to_py(obj: &lib::Ty, py: Python<'_>) -> PyObject {
    match obj {
        lib::Ty::Undefined => Py::new(py, Undefined::new()).unwrap().into_py(py),
        lib::Ty::Any => Py::new(py, Any::new()).unwrap().into_py(py),
        lib::Ty::Enumeration(enumeration) => Py::new(
            py,
            PyClassInitializer::from(Any::new()).add_subclass(Enumeration(enumeration.clone())),
        )
        .unwrap()
        .into_py(py),
        lib::Ty::AnyInteger => Py::new(py, AnyInteger::new()).unwrap().into_py(py),
        lib::Ty::UniversalInteger(universal_integer) => Py::new(
            py,
            AnyInteger::new().add_subclass(UniversalInteger(universal_integer.clone())),
        )
        .unwrap()
        .into_py(py),
        lib::Ty::Integer(integer) => {
            Py::new(py, AnyInteger::new().add_subclass(Integer(integer.clone())))
                .unwrap()
                .into_py(py)
        }
        lib::Ty::Composite => Py::new(py, Composite::new()).unwrap().into_py(py),
        lib::Ty::Aggregate(aggregate) => Py::new(
            py,
            Composite::new().add_subclass(Aggregate(aggregate.clone())),
        )
        .unwrap()
        .into_py(py),
        lib::Ty::Sequence(sequence) => Py::new(
            py,
            Composite::new().add_subclass(Sequence(sequence.clone())),
        )
        .unwrap()
        .into_py(py),
        lib::Ty::Compound => Py::new(py, Compound::new()).unwrap().into_py(py),
        lib::Ty::Structure(structure) => Py::new(
            py,
            Compound::new().add_subclass(Structure(structure.clone())),
        )
        .unwrap()
        .into_py(py),
        lib::Ty::Message(message) => {
            Py::new(py, Compound::new().add_subclass(Message(message.clone())))
                .unwrap()
                .into_py(py)
        }
        lib::Ty::Channel(channel) => Py::new(
            py,
            PyClassInitializer::from(Any::new()).add_subclass(Channel(channel.clone())),
        )
        .unwrap()
        .into_py(py),
    }
}

impl_states!(
    Bounds,
    Type,
    Undefined,
    Any,
    Enumeration,
    AnyInteger,
    UniversalInteger,
    Integer,
    Composite,
    Aggregate,
    Sequence,
    Refinement,
    Compound,
    Structure,
    Message,
    Channel
);
register_submodule_declarations!(
    ty,
    [],
    [
        Bounds,
        Builtins,
        Type,
        Undefined,
        Any,
        Enumeration,
        AnyInteger,
        UniversalInteger,
        Integer,
        Composite,
        Aggregate,
        Sequence,
        Refinement,
        Compound,
        Structure,
        Message,
        Channel,
    ],
    [common_type, check_type, check_type_instance]
);
