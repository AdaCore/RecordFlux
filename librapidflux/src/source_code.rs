use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use lazy_static::lazy_static;

type SourceCodeMap = HashMap<PathBuf, Arc<String>>;

lazy_static! {
    static ref SOURCE_CODE_MAP: Mutex<SourceCodeMap> = Mutex::new(SourceCodeMap::default());
}

/// Register a source code string to a given path in a global map.
///
/// Takes a file path and its corresponding source code as inputs and registers them in a
/// global map. The map is protected by a mutex to ensure thread-safety.
///
/// # Panics
///
/// This function will panic if:
///
/// * The mutex protecting the global source code map is poisoned (i.e., if another thread
///     panicked while holding the lock).
pub fn register(path: PathBuf, source_code: String) {
    let mut locked_map = SOURCE_CODE_MAP.lock().expect("mutex is poisoned");
    locked_map.insert(path, Arc::new(source_code));
}

/// Retrieve the source code associated with a given path from the global map.
///
/// Takes a file path as input and returns a reference-counted pointer to the source code string
/// associated with that path. The map is protected by a mutex to ensure thread-safety.
///
/// # Panics
///
/// This function will panic if:
///
/// * The mutex protecting the global map is poisoned (i.e., if another thread panicked while
///     holding the lock).
pub fn retrieve(path: &Path) -> Option<Arc<String>> {
    let locked_map = SOURCE_CODE_MAP.lock().expect("mutex is poisoned");
    locked_map.get(path).cloned()
}

/// Clear source code map entries.
///
/// # Panics
///
/// This function will panic if:
///
/// * The mutex protecting the global map is poisoned (i.e., if another thread panicked while
///     holding the lock).
pub fn clear() {
    let mut locked_map = SOURCE_CODE_MAP.lock().expect("mutex is poisoned");
    locked_map.clear();
}

#[cfg(test)]
mod tests {
    #![allow(clippy::used_underscore_binding)]

    use std::{path::PathBuf, sync::Arc};

    use rstest::{fixture, rstest};
    use serial_test::serial;

    use super::{clear, register, retrieve, SOURCE_CODE_MAP};

    #[fixture]
    fn cleanup() {
        SOURCE_CODE_MAP.clear_poison();
        let mut lock = SOURCE_CODE_MAP.lock().expect("mutex is poisoned");
        lock.clear();
    }

    #[rstest]
    #[serial]
    fn test_register_source_file(_cleanup: ()) {
        let source_path = PathBuf::from("foo.rflx");
        register(source_path.clone(), "some source code".to_string());

        let locked_map = SOURCE_CODE_MAP.lock().expect("mutex is poisoned");
        let result = locked_map.get(&source_path);

        assert!(result.is_some());
        assert_eq!(result.unwrap().as_str(), "some source code");
    }

    #[rstest]
    #[serial]
    fn test_retrieve_empty(_cleanup: ()) {
        assert!(retrieve(&PathBuf::from("foo.rflx")).is_none());
    }

    #[rstest]
    #[serial]
    fn test_retrieve_source_code(_cleanup: ()) {
        let source_path = PathBuf::from("foo.rflx");

        {
            let mut locked = SOURCE_CODE_MAP.lock().expect("mutex is poisoned");
            locked.insert(
                source_path.clone(),
                Arc::new("some source code".to_string()),
            );
        }

        let source = retrieve(&source_path);
        assert_eq!(
            source.as_ref().map(|s| s.as_str()),
            Some("some source code")
        );
    }

    #[rstest]
    #[serial]
    fn test_retrieve_source_code_non_existent(_cleanup: ()) {
        let source_path = PathBuf::from("foo.rflx");

        {
            let mut locked = SOURCE_CODE_MAP.lock().expect("mutex is poisoned");
            locked.insert(
                source_path.clone(),
                Arc::new("some source code".to_string()),
            );
        }

        assert!(retrieve(&PathBuf::from("bar.rflx")).is_none());
    }

    #[test]
    #[serial]
    fn test_clear_source_code_map() {
        let source_path = PathBuf::from("foo.rflx");

        {
            let mut locked = SOURCE_CODE_MAP.lock().expect("mutex is poisoned");
            locked.insert(
                source_path.clone(),
                Arc::new("some source code".to_string()),
            );
        }

        clear();
        assert!(SOURCE_CODE_MAP
            .lock()
            .expect("mutexs is poisoned")
            .is_empty());
    }
}
