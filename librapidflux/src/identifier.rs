use std::{fmt, string::ToString};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::diagnostics::location::Location;

pub const ID_SEP: &str = "::";
const ALT_ID_SEP: &str = ".";

pub type IDResult<T> = Result<T, IDError>;

#[must_use]
#[derive(Clone, Serialize, Deserialize, Eq, Debug)]
pub struct ID {
    identifier: String,
    location: Location,
}

impl ID {
    /// # Errors
    ///
    /// Will return `IDError::InvalidIdentifier` if identifier is invalid.
    pub fn new(identifier: &str, location: Location) -> IDResult<Self> {
        let identifier = identifier.replace(ALT_ID_SEP, ID_SEP);
        if !identifier.is_empty()
            && identifier.split(ID_SEP).all(|p| {
                !p.is_empty()
                    && !p.chars().any(char::is_whitespace)
                    && !p.contains(':')
                    && p.is_ascii()
            })
        {
            Ok(Self {
                identifier,
                location,
            })
        } else {
            Err(IDError::InvalidIdentifier)
        }
    }

    /// Create an owned `ID` with `prefix` prepended to `self`.
    ///
    /// # Errors
    ///
    /// Will return `IDError::InvalidIdentifier` if the resulting identifier is invalid.
    pub fn prefix(&self, prefix: &str) -> IDResult<Self> {
        if prefix.is_empty() || prefix == ID_SEP {
            Ok(self.clone())
        } else {
            Self::new(
                &(prefix.to_string() + &self.identifier),
                self.location.clone(),
            )
        }
    }

    /// Create an owned `ID` with `suffix` appended to `self`.
    ///
    /// # Errors
    ///
    /// Will return `IDError::InvalidIdentifier` if the resulting identifier is invalid.
    pub fn suffix(&self, suffix: &str) -> IDResult<Self> {
        if suffix.is_empty() || suffix == ID_SEP {
            Ok(self.clone())
        } else {
            Self::new(&(self.identifier.clone() + suffix), self.location.clone())
        }
    }

    /// Create an owned `ID` with `id` adjoined to `self` separated by `::`.
    ///
    /// # Errors
    ///
    /// Will return `IDError::InvalidIdentifier` if the resulting identifier is invalid.
    pub fn join(&self, id: &ID) -> IDResult<Self> {
        Self::new(
            &(self.identifier.clone() + ID_SEP + id.identifier()),
            if self.location == Location::None {
                id.location.clone()
            } else {
                self.location.clone()
            },
        )
    }

    pub fn identifier(&self) -> &str {
        &self.identifier
    }

    pub fn location(&self) -> &Location {
        &self.location
    }

    pub fn parts(&self) -> Vec<&str> {
        self.identifier.split(ID_SEP).collect()
    }

    /// Return the final part of the `ID`.
    ///
    /// # Panics
    ///
    /// Will panic if the resulting identifier is invalid. This can only be the case if the
    /// original identifier was already invalid.
    pub fn name(&self) -> IDRef<'_> {
        if self.identifier.contains(ID_SEP) {
            IDRef {
                identifier: self
                    .identifier
                    .rsplit_once(ID_SEP)
                    .map(|(_, name)| name)
                    .expect("invalid identifier"),
                location: &self.location,
            }
        } else {
            IDRef {
                identifier: &self.identifier,
                location: &self.location,
            }
        }
    }

    /// Return the `ID` without its final part, if there is one.
    ///
    /// # Panics
    ///
    /// Will panic if the resulting identifier is invalid. This can only be the case if the
    /// original identifier was already invalid.
    pub fn parent(&self) -> Option<IDRef> {
        if self.identifier.contains(ID_SEP) {
            Some(IDRef {
                identifier: self
                    .identifier
                    .rsplit_once(ID_SEP)
                    .map(|(parent, _)| parent)
                    .expect("invalid identifier"),
                location: &self.location,
            })
        } else {
            None
        }
    }

    pub fn flat(&self) -> String {
        self.identifier.replace(ID_SEP, "_")
    }

    pub fn to_ada_string(&self) -> String {
        self.identifier.replace(ID_SEP, ALT_ID_SEP)
    }
}

impl fmt::Display for ID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)
    }
}

impl std::cmp::PartialEq for ID {
    fn eq(&self, other: &Self) -> bool {
        self.identifier().eq_ignore_ascii_case(other.identifier())
    }
}

impl AsRef<str> for ID {
    fn as_ref(&self) -> &str {
        &self.identifier
    }
}

impl std::hash::Hash for ID {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.identifier.hash(state);
    }
}

#[derive(Debug, PartialEq)]
pub struct IDRef<'a> {
    identifier: &'a str,
    location: &'a Location,
}

impl IDRef<'_> {
    pub fn to_owned(&self) -> ID {
        ID {
            identifier: self.identifier.to_string(),
            location: self.location.clone(),
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum IDError {
    #[error("invalid identifier")]
    InvalidIdentifier,
}

/// Create an ID.
///
/// The parts are joined by the identifier separator `::`.
///
/// # Panics
///
/// The ID creation will panic if the given parts lead to an invalid ID.
///
/// # Examples
///
/// ```rust
/// use librapidflux::create_id;
/// use librapidflux::diagnostics::location::Location;
/// use librapidflux::identifier::ID;
///
/// let id = create_id!(["A", "B"], Location::None);
///
/// assert_eq!(id.identifier(), "A::B");
/// assert_eq!(*id.location(), Location::None);
/// ```
#[macro_export]
macro_rules! create_id {
    ([$($part:expr),* $(,)?], $location:expr $(,)?) => {
        ID::new(&[$( $part, )*].join("::"), $location).unwrap()
    };
}

#[cfg(test)]
mod tests {
    use std::hash::{DefaultHasher, Hash, Hasher};

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::{IDError, IDRef, ID};
    use crate::diagnostics::location::{FilePosition, Location};

    #[rstest]
    #[case("A", "A")]
    #[case("A.B", "A::B")]
    #[case("A::B", "A::B")]
    #[case("A.B::C", "A::B::C")]
    fn test_id_new(#[case] identifier: &str, #[case] expected: &str) {
        assert_eq!(
            ID::new(identifier, Location::None),
            Ok(ID {
                identifier: expected.to_string(),
                location: Location::None
            })
        );
    }

    #[rstest]
    #[case::empty("")]
    #[case::empty_part_1("A.")]
    #[case::empty_part_2(".B")]
    #[case::empty_part_3("A..B")]
    #[case::whitespace("A B")]
    #[case::colon_in_part_1("A:B")]
    #[case::colon_in_part_2("A:::B")]
    #[case::unicode_character("🐛")]
    fn test_id_new_invalid(#[case] identifier: &str) {
        assert_eq!(
            ID::new(identifier, Location::None),
            Err(IDError::InvalidIdentifier)
        );
    }

    #[test]
    fn test_id_identifier() {
        assert_eq!(id("A::B", Location::None).identifier(), "A::B");
    }

    #[rstest]
    #[case(Location::None, &Location::None)]
    #[case(location(1), &location(1))]
    fn test_id_location(#[case] loc: Location, #[case] expected: &Location) {
        assert_eq!(id("A::B", loc.clone()).location(), expected);
    }

    #[rstest]
    #[case("A", &["A"])]
    #[case("A::B", &["A", "B"])]
    #[case("A::B::C", &["A", "B", "C"])]
    fn test_id_parts(#[case] identifier: &str, #[case] expected: &[&str]) {
        assert_eq!(id(identifier, Location::None).parts(), expected);
    }

    #[rstest]
    #[case("A", "A")]
    #[case("A::B", "B")]
    #[case("A::B::C", "C")]
    fn test_id_name(#[case] identifier: &str, #[case] expected: &str) {
        assert_eq!(
            id(identifier, Location::None).name(),
            IDRef {
                identifier: expected,
                location: &Location::None
            }
        );
    }

    #[rstest]
    #[case("A::B", "A")]
    #[case("A::B::C", "A::B")]
    fn test_id_parent(#[case] identifier: &str, #[case] expected: &str) {
        assert_eq!(
            id(identifier, Location::None).parent().expect("no parent"),
            IDRef {
                identifier: expected,
                location: &Location::None
            }
        );
    }

    #[test]
    fn test_id_parent_none() {
        assert_eq!(id("A", Location::None).parent(), None);
    }

    #[rstest]
    #[case("A", "A")]
    #[case("A::B", "A_B")]
    #[case("A::B::C", "A_B_C")]
    fn test_id_flat(#[case] identifier: &str, #[case] expected: &str) {
        assert_eq!(id(identifier, Location::None).flat(), expected);
    }

    #[rstest]
    #[case("A", "A")]
    #[case("A::B", "A.B")]
    #[case("A::B::C", "A.B.C")]
    fn test_id_ada_str(#[case] identifier: &str, #[case] expected: &str) {
        assert_eq!(id(identifier, Location::None).to_ada_string(), expected);
    }

    #[test]
    fn test_id_serde() {
        let id = id("A::B", Location::None);
        let bytes = bincode::serialize(&id).expect("failed to serialize");
        let deserialized_id = bincode::deserialize(&bytes).expect("failed to deserialize");
        assert_eq!(id, deserialized_id);
    }

    #[rstest]
    #[case("A", "B", "AB")]
    #[case("A", "B::C", "AB::C")]
    #[case("A::B", "C", "A::BC")]
    #[case("A::B", "C::D", "A::BC::D")]
    fn test_id_prefix_suffix(#[case] left: &str, #[case] right: &str, #[case] expected: &str) {
        assert_eq!(
            id(left, Location::None).suffix(right),
            Ok(id(expected, Location::None))
        );
        assert_eq!(
            id(right, Location::None).prefix(left),
            Ok(id(expected, Location::None))
        );
    }

    #[rstest]
    #[case("A", "", "A")]
    #[case("A", "::", "A")]
    fn test_id_prefix_ident(#[case] left: &str, #[case] right: &str, #[case] expected: &str) {
        assert_eq!(
            id(left, Location::None).suffix(right),
            Ok(id(expected, Location::None))
        );
        assert_eq!(
            id(left, Location::None).prefix(right),
            Ok(id(expected, Location::None))
        );
    }

    #[rstest]
    #[case(Location::None, &Location::None)]
    #[case(location(1), &location(1))]
    fn test_id_prefix_location(#[case] loc: Location, #[case] expected: &Location) {
        assert_eq!(id("B", loc).prefix("A").unwrap().location(), expected);
    }

    #[rstest]
    #[case(Location::None, &Location::None)]
    #[case(location(1), &location(1))]
    fn test_id_suffix_location(#[case] loc: Location, #[case] expected: &Location) {
        assert_eq!(id("A", loc).suffix("B").unwrap().location(), expected);
    }

    #[rstest]
    #[case("A", "B", "A::B")]
    #[case("A", "B::C", "A::B::C")]
    #[case("A::B", "C", "A::B::C")]
    #[case("A::B", "C::D", "A::B::C::D")]
    fn test_id_join(#[case] left: &str, #[case] right: &str, #[case] expected: &str) {
        assert_eq!(
            id(left, Location::None).join(&id(right, Location::None)),
            Ok(id(expected, Location::None))
        );
    }

    #[rstest]
    #[case(location(1), location(2), &location(1))]
    #[case(location(1), Location::None, &location(1))]
    #[case(Location::None, location(2), &location(2))]
    #[case(Location::None, Location::None, &Location::None)]
    fn test_id_join_location(
        #[case] left: Location,
        #[case] right: Location,
        #[case] expected: &Location,
    ) {
        assert_eq!(
            (id("A", left).join(&id("B", right))).unwrap().location(),
            expected,
        );
    }

    #[rstest]
    #[case("A")]
    #[case("A::B")]
    fn test_id_display(#[case] identifier: &str) {
        assert_eq!(id(identifier, Location::None).to_string(), identifier);
    }

    #[test]
    fn test_id_hash() {
        let mut a1_hasher = DefaultHasher::new();
        id("A", Location::None).hash(&mut a1_hasher);
        let mut a2_hasher = DefaultHasher::new();
        id("A", Location::None).hash(&mut a2_hasher);
        assert_eq!(a1_hasher.finish(), a2_hasher.finish(),);

        let mut b_hasher = DefaultHasher::new();
        id("B", Location::None).hash(&mut b_hasher);
        assert_ne!(a1_hasher.finish(), b_hasher.finish(),);
    }

    #[rstest]
    #[case::equal_same_casing("A::B", "A::B", true)]
    #[case::equal_different_casing("A::B", "a::b", true)]
    #[case::not_equal("A::B", "A::A", false)]
    fn test_id_eq(#[case] left: &str, #[case] right: &str, #[case] expected: bool) {
        assert_eq!(
            id(left, Location::None)
                == id(
                    right,
                    Location::Stdin {
                        start: FilePosition::new(1, 1),
                        end: FilePosition::new(1, 1)
                    }
                ),
            expected
        );
    }

    #[test]
    fn test_id_as_ref() {
        assert_eq!(id("foo", Location::None).as_ref(), "foo");
    }

    #[test]
    fn test_id_ref_to_owned() {
        let identifier = "A::B".to_string();
        let location = location(1);
        let id_ref = IDRef {
            identifier: &identifier,
            location: &location,
        };
        let id = id_ref.to_owned();
        assert_eq!(id.identifier, identifier);
        assert_eq!(id.location, location);
    }

    #[test]
    fn test_iderror_display() {
        assert_eq!(IDError::InvalidIdentifier.to_string(), "invalid identifier");
    }

    #[test]
    fn test_create_id() {
        assert_eq!(
            create_id!(["A", "B"], location(1)),
            ID {
                identifier: "A::B".to_string(),
                location: location(1)
            }
        );
    }

    fn id(identifier: &str, location: Location) -> ID {
        ID::new(identifier, location).expect("invalid identifier")
    }

    fn location(value: u32) -> Location {
        Location::Stdin {
            start: FilePosition::new(value, value),
            end: FilePosition::new(value, value),
        }
    }
}
