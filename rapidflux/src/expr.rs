use std::hash::{DefaultHasher, Hash, Hasher};

use bincode::{deserialize, serialize};
use lazy_static::lazy_static;
use librapidflux::{expr as lib, ty as lib_ty};
use pyo3::{
    basic::CompareOp,
    exceptions::{PyNotImplementedError, PyValueError},
    prelude::*,
    sync::GILOnceCell,
    types::PyBytes,
};

use serde::{Deserialize, Serialize};

use crate::{
    diagnostics::{
        error,
        location::{Location, NO_LOCATION},
    },
    identifier::{to_id, ID},
    impl_states, register_submodule_declarations,
    ty::{self, to_ty, Type},
};

#[pyclass(module = "rflx.rapidflux.expr")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Precedence(lib::Precedence);

#[pymethods]
impl Precedence {
    #[new]
    fn new(precedence: &str) -> PyResult<Self> {
        Ok(match precedence {
            "UNDEFINED" => Self(lib::Precedence::Undefined),
            "BOOLEAN_OPERATOR" => Self(lib::Precedence::BooleanOperator),
            "RELATIONAL_OPERATOR" => Self(lib::Precedence::RelationalOperator),
            "BINARY_ADDING_OPERATOR" => Self(lib::Precedence::BinaryAddingOperator),
            "UNARY_ADDING_OPERATOR" => Self(lib::Precedence::UnaryAddingOperator),
            "MULTIPLYING_OPERATOR" => Self(lib::Precedence::MultiplyingOperator),
            "HIGHEST_PRECEDENCE_OPERATOR" => Self(lib::Precedence::HighestPrecedenceOperator),
            "LITERAL" => Self(lib::Precedence::Literal),
            _ => Err(PyValueError::new_err(format!(
                "Unknown precedence: {precedence}"
            )))?,
        })
    }

    pub fn __getnewargs__(&self) -> (String,) {
        // Removing ANSI escapes is needed here otherwise Python will not be able to rebuild the
        // object while calling `pickle.load`
        (anstream::adapter::strip_str(&self.0.to_string())
            .to_string()
            .to_uppercase(),)
    }

    fn __str__(&self) -> String {
        anstream::adapter::strip_str(&self.0.to_string()).to_string()
    }

    fn __repr__(&self) -> String {
        format!("{:?}", self.0)
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp) -> PyResult<bool> {
        match op {
            CompareOp::Eq => Ok(self.0 == other.0),
            CompareOp::Ne => Ok(self.0 != other.0),
            _ => Err(PyNotImplementedError::new_err(format!(
                "comparison {op:?} is not implemented for Precedence"
            ))),
        }
    }

    #[classattr]
    #[pyo3(name = "UNDEFINED")]
    fn undefined(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Precedence(lib::Precedence::Undefined).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "BOOLEAN_OPERATOR")]
    fn boolean_operator(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Precedence(lib::Precedence::BooleanOperator).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "RELATIONAL_OPERATOR")]
    fn relational_operator(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Precedence(lib::Precedence::RelationalOperator).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "BINARY_ADDING_OPERATOR")]
    fn binary_adding_operator(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject = Python::with_gil(|py| Precedence(
                lib::Precedence::BinaryAddingOperator
            )
            .into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "UNARY_ADDING_OPERATOR")]
    fn unary_adding_operator(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Precedence(lib::Precedence::UnaryAddingOperator).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "MULTIPLYING_OPERATOR")]
    fn multiplying_operator(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Precedence(lib::Precedence::MultiplyingOperator).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "HIGHEST_PRECEDENCE_OPERATOR")]
    fn highest_precedence_operator(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject = Python::with_gil(|py| Precedence(
                lib::Precedence::HighestPrecedenceOperator
            )
            .into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[classattr]
    #[pyo3(name = "LITERAL")]
    fn literal(py: Python<'_>) -> Borrowed<'_, '_, PyAny> {
        lazy_static! {
            static ref VALUE: PyObject =
                Python::with_gil(|py| Precedence(lib::Precedence::Literal).into_py(py));
        };

        VALUE.bind_borrowed(py)
    }

    #[getter]
    pub fn value(&self) -> u64 {
        self.0.clone() as u64
    }
}

#[pyclass(subclass, module = "rflx.rapidflux.expr")]
#[derive(Clone, Serialize, Deserialize)]
pub struct Expr;

#[pymethods]
impl Expr {
    #[new]
    #[pyo3(signature = (*_args, **_kwargs))]
    fn new(_args: &Bound<'_, PyAny>, _kwargs: Option<&Bound<'_, PyAny>>) -> Self {
        Self
    }

    #[classattr]
    #[pyo3(name = "TRUE")]
    fn literal_true(py: Python<'_>) -> &PyObject {
        static TRUE: GILOnceCell<PyObject> = GILOnceCell::new();
        TRUE.get_or_init(py, || to_py(&lib::TRUE, py))
    }

    #[classattr]
    #[pyo3(name = "FALSE")]
    fn literal_false(py: Python<'_>) -> &PyObject {
        static FALSE: GILOnceCell<PyObject> = GILOnceCell::new();
        FALSE.get_or_init(py, || to_py(&lib::FALSE, py))
    }
}

#[pyclass(extends = Expr, module = "rflx.rapidflux.expr")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Variable(lib::Sym);

#[pymethods]
impl Variable {
    #[new]
    fn new(
        identifier: &Bound<'_, PyAny>,
        type_: Option<&Bound<'_, Type>>,
    ) -> PyResult<(Self, Expr)> {
        Ok((
            Self(lib::Sym {
                id: to_id(identifier)?,
                ty: type_.map_or(lib_ty::Ty::Undefined, |t| to_ty(t)),
            }),
            Expr,
        ))
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (ID, PyObject) {
        (self.identifier(), self.type_(py))
    }

    fn __str__(&self) -> String {
        self.0.id.to_string()
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!(
            "Variable({}, {})",
            ID(self.0.id.clone()).__repr__(),
            self.type_(py).call_method0(py, "__repr__")?
        ))
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> bool {
        if let Ok(other) = other.extract::<Self>() {
            op.matches(self.0.id.identifier().cmp(other.0.id.identifier()))
        } else {
            matches!(op, CompareOp::Ne)
        }
    }

    #[allow(clippy::unused_self)]
    #[allow(clippy::cast_possible_truncation)]
    fn __hash__(&self) -> usize {
        let mut hasher = DefaultHasher::new();
        let identifier = self.0.id.to_string();
        identifier.to_ascii_lowercase().hash(&mut hasher);
        hasher.finish() as usize
    }

    fn __neg__(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::Var(self.0.clone()).into_negated(), py)
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.id.location().clone())
    }

    #[getter]
    fn precedence(&self) -> Precedence {
        Precedence(self.0.precedence().clone())
    }

    #[getter]
    fn type_(&self, py: Python<'_>) -> PyObject {
        ty::to_py(&self.0.ty, py)
    }

    #[setter]
    fn set_type_(&mut self, type_: &Bound<'_, PyAny>) {
        self.0.ty = ty::to_ty(type_);
    }

    #[getter]
    fn identifier(&self) -> ID {
        ID(self.0.id.clone())
    }

    #[getter]
    fn name(&self) -> &str {
        self.0.id.identifier()
    }

    fn check_type(&self, expected: &Bound<'_, PyAny>) -> error::Error {
        error::Error(lib::Expr::Var(self.0.clone()).check_type(&to_ty_list(expected)))
    }

    fn check_type_instance(&self, expected: &Bound<'_, PyAny>, py: Python<'_>) -> error::Error {
        error::Error(
            lib::Expr::Var(self.0.clone())
                .check_type_instance(&to_ty_discriminants_list(expected, py)),
        )
    }

    fn variables(&self, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(&lib::Expr::Var(self.0.clone()).variables(), py)
    }

    fn findall(&self, r#match: &Bound<'_, PyAny>, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(
            &lib::Expr::Var(self.0.clone()).find_all(&to_fn_expr_bool(r#match, py)),
            py,
        )
    }

    fn substituted(&self, func: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Expr::Var(self.0.clone()).into_substituted(&to_fn_expr_expr(func, py)),
            py,
        )
    }

    fn simplified(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::Var(self.0.clone()), py)
    }
}

#[pyclass(extends = Expr, module = "rflx.rapidflux.expr")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Literal(lib::Sym);

#[pymethods]
impl Literal {
    #[new]
    fn new(
        identifier: &Bound<'_, PyAny>,
        type_: Option<&Bound<'_, Type>>,
    ) -> PyResult<(Self, Expr)> {
        Ok((
            Self(lib::Sym {
                id: to_id(identifier)?,
                ty: type_.map_or(lib_ty::Ty::Undefined, |t| to_ty(t)),
            }),
            Expr,
        ))
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (ID, PyObject) {
        (self.identifier(), self.type_(py))
    }

    fn __str__(&self) -> String {
        self.0.id.to_string()
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!(
            "Literal({}, {})",
            ID(self.0.id.clone()).__repr__(),
            self.type_(py).call_method0(py, "__repr__")?
        ))
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> bool {
        if let Ok(other) = other.extract::<Self>() {
            op.matches(self.0.id.identifier().cmp(other.0.id.identifier()))
        } else {
            matches!(op, CompareOp::Ne)
        }
    }

    #[allow(clippy::unused_self)]
    #[allow(clippy::cast_possible_truncation)]
    fn __hash__(&self) -> usize {
        let mut hasher = DefaultHasher::new();
        let identifier = self.0.id.to_string();
        identifier.to_ascii_lowercase().hash(&mut hasher);
        hasher.finish() as usize
    }

    fn __neg__(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::Lit(self.0.clone()).into_negated(), py)
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.id.location().clone())
    }

    #[getter]
    fn precedence(&self) -> Precedence {
        Precedence(self.0.precedence().clone())
    }

    #[getter]
    fn type_(&self, py: Python<'_>) -> PyObject {
        ty::to_py(&self.0.ty, py)
    }

    #[getter]
    fn identifier(&self) -> ID {
        ID(self.0.id.clone())
    }

    #[getter]
    fn name(&self) -> &str {
        self.0.id.identifier()
    }

    fn check_type(&self, expected: &Bound<'_, PyAny>) -> error::Error {
        error::Error(lib::Expr::Lit(self.0.clone()).check_type(&to_ty_list(expected)))
    }

    fn check_type_instance(&self, expected: &Bound<'_, PyAny>, py: Python<'_>) -> error::Error {
        error::Error(
            lib::Expr::Lit(self.0.clone())
                .check_type_instance(&to_ty_discriminants_list(expected, py)),
        )
    }

    fn variables(&self, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(&lib::Expr::Lit(self.0.clone()).variables(), py)
    }

    fn findall(&self, r#match: &Bound<'_, PyAny>, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(
            &lib::Expr::Lit(self.0.clone()).find_all(&to_fn_expr_bool(r#match, py)),
            py,
        )
    }

    fn substituted(&self, func: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Expr::Lit(self.0.clone()).into_substituted(&to_fn_expr_expr(func, py)),
            py,
        )
    }

    fn simplified(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::Lit(self.0.clone()), py)
    }
}

#[pyclass(extends = Expr, module = "rflx.rapidflux.expr")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Number(lib::Num);

#[pymethods]
impl Number {
    #[new]
    fn new(value: i64, base: Option<u8>, location: Option<Location>) -> (Self, Expr) {
        (
            Self(lib::Num {
                value,
                base: base.map_or(lib::NumBase::Default, |b| {
                    lib::NumBase::from_repr(b).unwrap_or_else(|| panic!("unsupported base {b}"))
                }),
                location: location.unwrap_or(NO_LOCATION).0,
            }),
            Expr,
        )
    }

    fn __getnewargs__(&self) -> (i64, u8, Location) {
        (self.value(), self.base(), self.location())
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> bool {
        if let Ok(other) = other.extract::<Self>() {
            op.matches(self.0.value.cmp(&other.0.value))
        } else {
            matches!(op, CompareOp::Ne)
        }
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __repr__(&self) -> String {
        format!("Number({}, {})", self.0.value, self.location().__repr__())
    }

    fn __int__(&self) -> i64 {
        self.0.value
    }

    #[allow(clippy::unused_self)]
    #[allow(clippy::cast_possible_truncation)]
    fn __hash__(&self) -> usize {
        let mut hasher = DefaultHasher::new();
        let identifier = self.0.value;
        identifier.hash(&mut hasher);
        hasher.finish() as usize
    }

    fn __neg__(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.clone().into_negated(), py)
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location.clone())
    }

    #[getter]
    fn precedence(&self) -> Precedence {
        Precedence(self.0.precedence().clone())
    }

    #[getter]
    fn type_(&self, py: Python<'_>) -> PyObject {
        ty::to_py(&self.0.ty(), py)
    }

    #[getter]
    fn value(&self) -> i64 {
        self.0.value
    }

    #[getter]
    fn base(&self) -> u8 {
        self.0.base as u8
    }

    fn check_type(&self, expected: &Bound<'_, PyAny>) -> error::Error {
        error::Error(lib::Expr::Num(self.0.clone()).check_type(&to_ty_list(expected)))
    }

    fn check_type_instance(&self, expected: &Bound<'_, PyAny>, py: Python<'_>) -> error::Error {
        error::Error(
            lib::Expr::Num(self.0.clone())
                .check_type_instance(&to_ty_discriminants_list(expected, py)),
        )
    }

    fn variables(&self, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(&lib::Expr::Num(self.0.clone()).variables(), py)
    }

    fn findall(&self, r#match: &Bound<'_, PyAny>, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(
            &lib::Expr::Num(self.0.clone()).find_all(&to_fn_expr_bool(r#match, py)),
            py,
        )
    }

    fn substituted(&self, func: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Expr::Num(self.0.clone()).into_substituted(&to_fn_expr_expr(func, py)),
            py,
        )
    }

    fn simplified(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::Num(self.0.clone()), py)
    }
}

#[pyclass(extends = Expr, module = "rflx.rapidflux.expr")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Neg(lib::Neg);

#[pymethods]
impl Neg {
    #[new]
    fn new(expr: &Bound<'_, Expr>, location: Option<Location>) -> (Self, Expr) {
        (
            Self(lib::Neg {
                expr: Box::new(to_expr(expr)),
                location: location.unwrap_or(NO_LOCATION).0,
            }),
            Expr,
        )
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (PyObject, Location) {
        (self.expr(py), self.location())
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!(
            "Neg({}, {})",
            to_py(&self.0.expr, py).call_method0(py, "__repr__")?,
            self.location().__repr__()
        ))
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> bool {
        if let Ok(other) = other.extract::<Self>() {
            match op {
                CompareOp::Eq => self.0.expr == other.0.expr,
                CompareOp::Ne => self.0.expr != other.0.expr,
                _ => false,
            }
        } else {
            matches!(op, CompareOp::Ne)
        }
    }

    #[allow(clippy::unused_self)]
    fn __hash__(&self) -> usize {
        0
    }

    fn __neg__(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.clone().into_negated(), py)
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location.clone())
    }

    #[getter]
    fn precedence(&self) -> Precedence {
        Precedence(self.0.precedence().clone())
    }

    #[getter]
    fn type_(&self, py: Python<'_>) -> PyObject {
        ty::to_py(&self.0.ty(), py)
    }

    #[getter]
    fn expr(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.expr, py)
    }

    fn check_type(&self, expected: &Bound<'_, PyAny>) -> error::Error {
        error::Error(lib::Expr::Neg(self.0.clone()).check_type(&to_ty_list(expected)))
    }

    fn check_type_instance(&self, expected: &Bound<'_, PyAny>, py: Python<'_>) -> error::Error {
        error::Error(
            lib::Expr::Neg(self.0.clone())
                .check_type_instance(&to_ty_discriminants_list(expected, py)),
        )
    }

    fn variables(&self, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(&lib::Expr::Neg(self.0.clone()).variables(), py)
    }

    fn findall(&self, r#match: &Bound<'_, PyAny>, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(
            &lib::Expr::Neg(self.0.clone()).find_all(&to_fn_expr_bool(r#match, py)),
            py,
        )
    }

    fn substituted(&self, func: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Expr::Neg(self.0.clone()).into_substituted(&to_fn_expr_expr(func, py)),
            py,
        )
    }

    fn simplified(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::Neg(self.0.clone()).into_simplified(), py)
    }
}

#[pyclass(extends = Expr, module = "rflx.rapidflux.expr")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Sub(lib::BinExpr);

#[pymethods]
impl Sub {
    #[new]
    fn new(
        left: &Bound<'_, Expr>,
        right: &Bound<'_, Expr>,
        location: Option<Location>,
    ) -> (Self, Expr) {
        (
            Self(lib::BinExpr {
                op: lib::BinOp::Sub,
                left: Box::new(to_expr(left)),
                right: Box::new(to_expr(right)),
                location: location.unwrap_or(NO_LOCATION).0,
            }),
            Expr,
        )
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (PyObject, PyObject, Location) {
        (self.left(py), self.right(py), self.location())
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!(
            "Sub({}, {}, {})",
            to_py(&self.0.left, py).call_method0(py, "__repr__")?,
            to_py(&self.0.right, py).call_method0(py, "__repr__")?,
            self.location().__repr__()
        ))
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> bool {
        if let Ok(other) = other.extract::<Self>() {
            match op {
                CompareOp::Eq => self.0.left == other.0.left && self.0.right == other.0.right,
                CompareOp::Ne => self.0.left != other.0.left || self.0.right != other.0.right,
                _ => false,
            }
        } else {
            matches!(op, CompareOp::Ne)
        }
    }

    #[allow(clippy::unused_self)]
    fn __hash__(&self) -> usize {
        0
    }

    fn __neg__(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.clone().into_negated(), py)
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location.clone())
    }

    #[getter]
    fn precedence(&self) -> Precedence {
        Precedence(self.0.precedence().clone())
    }

    #[getter]
    fn type_(&self, py: Python<'_>) -> PyObject {
        ty::to_py(&self.0.ty(), py)
    }

    #[getter]
    fn left(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.left, py)
    }

    #[getter]
    fn right(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.right, py)
    }

    fn check_type(&self, expected: &Bound<'_, PyAny>) -> error::Error {
        error::Error(lib::Expr::BinExpr(self.0.clone()).check_type(&to_ty_list(expected)))
    }

    fn check_type_instance(&self, expected: &Bound<'_, PyAny>, py: Python<'_>) -> error::Error {
        error::Error(
            lib::Expr::BinExpr(self.0.clone())
                .check_type_instance(&to_ty_discriminants_list(expected, py)),
        )
    }

    fn variables(&self, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(&lib::Expr::BinExpr(self.0.clone()).variables(), py)
    }

    fn findall(&self, r#match: &Bound<'_, PyAny>, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(
            &lib::Expr::BinExpr(self.0.clone()).find_all(&to_fn_expr_bool(r#match, py)),
            py,
        )
    }

    fn substituted(&self, func: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Expr::BinExpr(self.0.clone()).into_substituted(&to_fn_expr_expr(func, py)),
            py,
        )
    }

    fn simplified(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::BinExpr(self.0.clone()).into_simplified(), py)
    }
}

#[pyclass(extends = Expr, module = "rflx.rapidflux.expr")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Div(lib::BinExpr);

#[pymethods]
impl Div {
    #[new]
    fn new(
        left: &Bound<'_, Expr>,
        right: &Bound<'_, Expr>,
        location: Option<Location>,
    ) -> (Self, Expr) {
        (
            Self(lib::BinExpr {
                op: lib::BinOp::Div,
                left: Box::new(to_expr(left)),
                right: Box::new(to_expr(right)),
                location: location.unwrap_or(NO_LOCATION).0,
            }),
            Expr,
        )
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (PyObject, PyObject, Location) {
        (self.left(py), self.right(py), self.location())
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!(
            "Div({}, {}, {})",
            to_py(&self.0.left, py).call_method0(py, "__repr__")?,
            to_py(&self.0.right, py).call_method0(py, "__repr__")?,
            self.location().__repr__()
        ))
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> bool {
        if let Ok(other) = other.extract::<Self>() {
            match op {
                CompareOp::Eq => self.0.left == other.0.left && self.0.right == other.0.right,
                CompareOp::Ne => self.0.left != other.0.left || self.0.right != other.0.right,
                _ => false,
            }
        } else {
            matches!(op, CompareOp::Ne)
        }
    }

    #[allow(clippy::unused_self)]
    fn __hash__(&self) -> usize {
        0
    }

    fn __neg__(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.clone().into_negated(), py)
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location.clone())
    }

    #[getter]
    fn precedence(&self) -> Precedence {
        Precedence(self.0.precedence().clone())
    }

    #[getter]
    fn type_(&self, py: Python<'_>) -> PyObject {
        ty::to_py(&self.0.ty(), py)
    }

    #[getter]
    fn left(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.left, py)
    }

    #[getter]
    fn right(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.right, py)
    }

    fn check_type(&self, expected: &Bound<'_, PyAny>) -> error::Error {
        error::Error(lib::Expr::BinExpr(self.0.clone()).check_type(&to_ty_list(expected)))
    }

    fn check_type_instance(&self, expected: &Bound<'_, PyAny>, py: Python<'_>) -> error::Error {
        error::Error(
            lib::Expr::BinExpr(self.0.clone())
                .check_type_instance(&to_ty_discriminants_list(expected, py)),
        )
    }

    fn variables(&self, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(&lib::Expr::BinExpr(self.0.clone()).variables(), py)
    }

    fn findall(&self, r#match: &Bound<'_, PyAny>, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(
            &lib::Expr::BinExpr(self.0.clone()).find_all(&to_fn_expr_bool(r#match, py)),
            py,
        )
    }

    fn substituted(&self, func: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Expr::BinExpr(self.0.clone()).into_substituted(&to_fn_expr_expr(func, py)),
            py,
        )
    }

    fn simplified(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::BinExpr(self.0.clone()).into_simplified(), py)
    }
}

#[pyclass(extends = Expr, module = "rflx.rapidflux.expr")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Pow(lib::BinExpr);

#[pymethods]
impl Pow {
    #[new]
    fn new(
        left: &Bound<'_, Expr>,
        right: &Bound<'_, Expr>,
        location: Option<Location>,
    ) -> (Self, Expr) {
        (
            Self(lib::BinExpr {
                op: lib::BinOp::Pow,
                left: Box::new(to_expr(left)),
                right: Box::new(to_expr(right)),
                location: location.unwrap_or(NO_LOCATION).0,
            }),
            Expr,
        )
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (PyObject, PyObject, Location) {
        (self.left(py), self.right(py), self.location())
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!(
            "Pow({}, {}, {})",
            to_py(&self.0.left, py).call_method0(py, "__repr__")?,
            to_py(&self.0.right, py).call_method0(py, "__repr__")?,
            self.location().__repr__()
        ))
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> bool {
        if let Ok(other) = other.extract::<Self>() {
            match op {
                CompareOp::Eq => self.0.left == other.0.left && self.0.right == other.0.right,
                CompareOp::Ne => self.0.left != other.0.left || self.0.right != other.0.right,
                _ => false,
            }
        } else {
            matches!(op, CompareOp::Ne)
        }
    }

    #[allow(clippy::unused_self)]
    fn __hash__(&self) -> usize {
        0
    }

    fn __neg__(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.clone().into_negated(), py)
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location.clone())
    }

    #[getter]
    fn precedence(&self) -> Precedence {
        Precedence(self.0.precedence().clone())
    }

    #[getter]
    fn type_(&self, py: Python<'_>) -> PyObject {
        ty::to_py(&self.0.ty(), py)
    }

    #[getter]
    fn left(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.left, py)
    }

    #[getter]
    fn right(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.right, py)
    }

    fn check_type(&self, expected: &Bound<'_, PyAny>) -> error::Error {
        error::Error(lib::Expr::BinExpr(self.0.clone()).check_type(&to_ty_list(expected)))
    }

    fn check_type_instance(&self, expected: &Bound<'_, PyAny>, py: Python<'_>) -> error::Error {
        error::Error(
            lib::Expr::BinExpr(self.0.clone())
                .check_type_instance(&to_ty_discriminants_list(expected, py)),
        )
    }

    fn variables(&self, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(&lib::Expr::BinExpr(self.0.clone()).variables(), py)
    }

    fn findall(&self, r#match: &Bound<'_, PyAny>, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(
            &lib::Expr::BinExpr(self.0.clone()).find_all(&to_fn_expr_bool(r#match, py)),
            py,
        )
    }

    fn substituted(&self, func: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Expr::BinExpr(self.0.clone()).into_substituted(&to_fn_expr_expr(func, py)),
            py,
        )
    }

    fn simplified(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::BinExpr(self.0.clone()).into_simplified(), py)
    }
}

#[pyclass(extends = Expr, module = "rflx.rapidflux.expr")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Mod(lib::BinExpr);

#[pymethods]
impl Mod {
    #[new]
    fn new(
        left: &Bound<'_, Expr>,
        right: &Bound<'_, Expr>,
        location: Option<Location>,
    ) -> (Self, Expr) {
        (
            Self(lib::BinExpr {
                op: lib::BinOp::Mod,
                left: Box::new(to_expr(left)),
                right: Box::new(to_expr(right)),
                location: location.unwrap_or(NO_LOCATION).0,
            }),
            Expr,
        )
    }

    fn __getnewargs__(&self, py: Python<'_>) -> (PyObject, PyObject, Location) {
        (self.left(py), self.right(py), self.location())
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(format!(
            "Mod({}, {}, {})",
            to_py(&self.0.left, py).call_method0(py, "__repr__")?,
            to_py(&self.0.right, py).call_method0(py, "__repr__")?,
            self.location().__repr__()
        ))
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> bool {
        if let Ok(other) = other.extract::<Self>() {
            match op {
                CompareOp::Eq => self.0.left == other.0.left && self.0.right == other.0.right,
                CompareOp::Ne => self.0.left != other.0.left || self.0.right != other.0.right,
                _ => false,
            }
        } else {
            matches!(op, CompareOp::Ne)
        }
    }

    #[allow(clippy::unused_self)]
    fn __hash__(&self) -> usize {
        0
    }

    fn __neg__(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.clone().into_negated(), py)
    }

    #[getter]
    fn location(&self) -> Location {
        Location(self.0.location.clone())
    }

    #[getter]
    fn precedence(&self) -> Precedence {
        Precedence(self.0.precedence().clone())
    }

    #[getter]
    fn type_(&self, py: Python<'_>) -> PyObject {
        ty::to_py(&self.0.ty(), py)
    }

    #[getter]
    fn left(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.left, py)
    }

    #[getter]
    fn right(&self, py: Python<'_>) -> PyObject {
        to_py(&self.0.right, py)
    }

    fn check_type(&self, expected: &Bound<'_, PyAny>) -> error::Error {
        error::Error(lib::Expr::BinExpr(self.0.clone()).check_type(&to_ty_list(expected)))
    }

    fn check_type_instance(&self, expected: &Bound<'_, PyAny>, py: Python<'_>) -> error::Error {
        error::Error(
            lib::Expr::BinExpr(self.0.clone())
                .check_type_instance(&to_ty_discriminants_list(expected, py)),
        )
    }

    fn variables(&self, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(&lib::Expr::BinExpr(self.0.clone()).variables(), py)
    }

    fn findall(&self, r#match: &Bound<'_, PyAny>, py: Python<'_>) -> Vec<PyObject> {
        to_py_list(
            &lib::Expr::BinExpr(self.0.clone()).find_all(&to_fn_expr_bool(r#match, py)),
            py,
        )
    }

    fn substituted(&self, func: &Bound<'_, PyAny>, py: Python<'_>) -> PyObject {
        to_py(
            &lib::Expr::BinExpr(self.0.clone()).into_substituted(&to_fn_expr_expr(func, py)),
            py,
        )
    }

    fn simplified(&self, py: Python<'_>) -> PyObject {
        to_py(&lib::Expr::BinExpr(self.0.clone()).into_simplified(), py)
    }
}

fn to_expr(obj: &Bound<'_, PyAny>) -> lib::Expr {
    if let Ok(var) = obj.extract::<PyRef<Variable>>() {
        lib::Expr::Var(var.0.clone())
    } else if let Ok(lit) = obj.extract::<PyRef<Literal>>() {
        lib::Expr::Lit(lit.0.clone())
    } else if let Ok(num) = obj.extract::<PyRef<Number>>() {
        lib::Expr::Num(num.0.clone())
    } else if let Ok(neg) = obj.extract::<PyRef<Neg>>() {
        lib::Expr::Neg(neg.0.clone())
    } else if let Ok(sub) = obj.extract::<PyRef<Sub>>() {
        lib::Expr::BinExpr(sub.0.clone())
    } else if let Ok(div) = obj.extract::<PyRef<Div>>() {
        lib::Expr::BinExpr(div.0.clone())
    } else if let Ok(pow) = obj.extract::<PyRef<Pow>>() {
        lib::Expr::BinExpr(pow.0.clone())
    } else if let Ok(r#mod) = obj.extract::<PyRef<Mod>>() {
        lib::Expr::BinExpr(r#mod.0.clone())
    } else {
        panic!("unknown expression \"{obj:?}\"")
    }
}

fn to_ty_list(obj: &Bound<'_, PyAny>) -> Vec<lib_ty::Ty> {
    if let Some(obj_type) = ty::to_ty_opt(obj) {
        vec![obj_type]
    } else if let Ok(obj_types) = obj.extract::<Vec<Bound<'_, PyAny>>>() {
        obj_types.iter().map(|t| ty::to_ty(t)).collect()
    } else {
        panic!(
            "unexpected type \"{}\"",
            obj.get_type().name().unwrap_or_default()
        )
    }
}

fn to_fn_expr_bool<'a>(
    func: &'a Bound<'a, PyAny>,
    py: Python<'a>,
) -> (impl Fn(&lib::Expr) -> bool + 'a) {
    move |e: &lib::Expr| {
        func.call1((to_py(e, py),))
            .expect("invalid arguments")
            .extract::<bool>()
            .expect("invalid result type")
    }
}

fn to_fn_expr_expr<'a>(
    func: &'a Bound<'a, PyAny>,
    py: Python<'a>,
) -> (impl Fn(&lib::Expr) -> lib::Expr + 'a) {
    move |e: &lib::Expr| to_expr(&func.call1((to_py(e, py),)).expect("invalid expression"))
}

fn to_ty_discriminants_list(
    obj: &Bound<'_, PyAny>,
    py: Python<'_>,
) -> Vec<lib_ty::TyDiscriminants> {
    if let Some(obj_type) = ty::to_ty_discriminants_opt(obj, py) {
        vec![obj_type]
    } else if let Ok(obj_types) = obj.extract::<Vec<Bound<'_, PyAny>>>() {
        obj_types
            .iter()
            .map(|t| ty::to_ty_discriminants(t, py))
            .collect()
    } else {
        panic!(
            "unexpected type \"{}\"",
            obj.get_type().name().unwrap_or_default()
        )
    }
}

fn to_py(obj: &lib::Expr, py: Python<'_>) -> PyObject {
    match obj {
        lib::Expr::Var(var) => Py::new(py, (Variable(var.clone()), Expr))
            .unwrap()
            .into_py(py),
        lib::Expr::Lit(lit) => Py::new(py, (Literal(lit.clone()), Expr))
            .unwrap()
            .into_py(py),
        lib::Expr::Num(num) => Py::new(py, (Number(num.clone()), Expr))
            .unwrap()
            .into_py(py),
        lib::Expr::Neg(neg) => Py::new(py, (Neg(neg.clone()), Expr)).unwrap().into_py(py),
        lib::Expr::BinExpr(bin_expr) => match bin_expr.op {
            lib::BinOp::Sub => Py::new(py, (Sub(bin_expr.clone()), Expr))
                .unwrap()
                .into_py(py),
            lib::BinOp::Div => Py::new(py, (Div(bin_expr.clone()), Expr))
                .unwrap()
                .into_py(py),
            lib::BinOp::Pow => Py::new(py, (Pow(bin_expr.clone()), Expr))
                .unwrap()
                .into_py(py),
            lib::BinOp::Mod => Py::new(py, (Mod(bin_expr.clone()), Expr))
                .unwrap()
                .into_py(py),
        },
    }
}

fn to_py_list(obj: &[lib::Expr], py: Python<'_>) -> Vec<PyObject> {
    obj.iter().map(|e| to_py(e, py)).collect::<Vec<_>>()
}

impl_states!(Expr, Precedence, Variable, Literal, Number, Neg, Sub, Div, Pow, Mod);
register_submodule_declarations!(
    expr,
    [],
    [Expr, Precedence, Variable, Literal, Number, Neg, Sub, Div, Pow, Mod],
    []
);
