/// Enable pickling of a Python class.
///
/// This macro implements `__getstate__` and `__setstate__` for the given types.
/// The serialization is using `bincode` to serialize and deserialize Rust objects.
///
/// # Examples
///
/// ```rust
/// // in `foo.rs`
/// use pyo3::prelude::*;
///
/// #[pyclass]
/// struct Dummy;
///
/// impl_states!(Dummy);
/// ```
#[macro_export]
macro_rules! impl_states {
    ($( $name:ty ),* $(,)?) => {
        $(
            impl $name {
                pub fn __setstate__(&mut self, state: &Bound<'_, PyBytes>) {
                    *self = deserialize(state.as_bytes()).unwrap();
                }

                pub fn __getstate__<'py>(&self, py: Python<'py>) -> Bound<'py, PyBytes> {
                    PyBytes::new_bound(py, &serialize(&self).unwrap())
                }
            }
        )*
    };
}

/// Register classes in a submodule.
///
/// This macro generate a `register_<module name>_module` function that is used by the
/// `register_submodule` function later to add a submodule in `rapidflux`.
///
/// # Examples
///
/// ```rust
/// // in `foo.rs`
/// use pyo3::prelude::*;
///
/// #[pyclass]
/// pub struct Foo;
/// #[pyclass]
/// pub struct Bar;
///
/// register_submodule_classes!(foo, [Foo, Bar]);
/// ```
#[macro_export]
macro_rules! register_submodule_classes {
    ($module_name:ident, [$($class_name:ident),+ $(,)?] $(,)?) => {
        ::paste::paste! {
            pub fn [<register_ $module_name _module>]<'py>(
                py: Python<'py>,
                m: &Bound<'py, PyModule>
            ) -> PyResult<()> {
                const PY_MODULE_PATH: &str = concat!("rflx.rapidflux.", stringify!($module_name));

                $(
                    m.add_class::<$class_name>()?;
                 )*

                // Submodules need to be added manually to `sys.modules`.
                // See: https://github.com/PyO3/pyo3/issues/759#issuecomment-1208179322
                py.import_bound("sys")?
                    .getattr("modules")?
                    .set_item(PY_MODULE_PATH, m)
            }
        }
    };
}

/// Register functions in a submodule.
///
/// Works like `register_submodule_classes` but takes one or more functions instead.
///
/// # Examples
///
/// ```rust
/// // in `foo.rs`
/// use pyo3::prelude::*;
///
/// fn bar() {}
/// fn baz() {}
///
/// register_submodule_functions!(foo, [bar, baz]);
/// ```
#[macro_export]
macro_rules! register_submodule_functions {
    ($module_name:ident, [$($fn_name:ident),+ $(,)?] $(,)?) => {
        ::paste::paste! {
            pub fn [<register_ $module_name _module>]<'py>(
                py: Python<'py>,
                m: &Bound<'py, PyModule>
            ) -> PyResult<()> {
                const PY_MODULE_PATH: &str = concat!("rflx.rapidflux.", stringify!($module_name));

                $(
                    m.add_function(wrap_pyfunction!($fn_name, m)?)?;
                 )*

                // Submodules need to be added manually to `sys.modules`.
                // See: https://github.com/PyO3/pyo3/issues/759#issuecomment-1208179322
                py.import_bound("sys")?
                    .getattr("modules")?
                    .set_item(PY_MODULE_PATH, m)
            }
        }
    };
}

/// Register a submodule in `rflx.rapidflux`.
///
/// This macro call the `register_<module name>_module` function to initialize the submodule and
/// then add the module to `rflx.rapidflux` as a submodule.
///
/// # Examples
///
/// ```rust
/// // in `lib.rs`
/// use pyo3::prelude::*;
///
/// #[pymodule]
/// fn rapidflux(py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
///     // ...
///     register_submodule!(foo, py, m);
///     register_submodule!(bar, py, m);
/// }
/// ```
#[macro_export]
macro_rules! register_submodule {
    ($name:ident, $py:ident, $parent_module:ident) => {
        ::paste::paste! {
            let [<$name _module>] = PyModule::new_bound($parent_module.py(), stringify!($name))?;
            $name::[<register_ $name _module>]($py, &[<$name _module>])?;
            $parent_module.add_submodule(&[<$name _module>])?;
        }
    };
}
