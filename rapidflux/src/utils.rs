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
