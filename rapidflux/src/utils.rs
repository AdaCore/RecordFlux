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
