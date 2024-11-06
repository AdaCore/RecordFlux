use std::{collections::HashSet, fmt::Display, path::PathBuf};

use indexmap::IndexMap;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use strum::EnumDiscriminants;

use crate::{
    consts, create_id,
    diagnostics::{
        error::{Annotation, Entry, Error, Severity},
        location::{FilePosition, Location},
    },
    identifier::ID,
};

#[must_use]
#[derive(EnumDiscriminants, Clone, PartialEq, Serialize, Deserialize, Debug)]
#[strum_discriminants(derive(strum::Display))]
pub enum Ty {
    #[strum_discriminants(strum(to_string = "undefined type"))]
    Undefined,
    #[strum_discriminants(strum(to_string = "any type"))]
    Any,
    #[strum_discriminants(strum(to_string = "enumeration type"))]
    Enumeration(Enumeration),
    #[strum_discriminants(strum(to_string = "integer type"))]
    AnyInteger,
    #[strum_discriminants(strum(to_string = "type universal integer"))]
    UniversalInteger(UniversalInteger),
    #[strum_discriminants(strum(to_string = "integer type"))]
    Integer(Integer),
    #[strum_discriminants(strum(to_string = "composite type"))]
    Composite,
    #[strum_discriminants(strum(to_string = "aggregate"))]
    Aggregate(Aggregate),
    #[strum_discriminants(strum(to_string = "sequence type"))]
    Sequence(Sequence),
    #[strum_discriminants(strum(to_string = "compound type"))]
    Compound,
    #[strum_discriminants(strum(to_string = "structure type"))]
    Structure(Structure),
    #[strum_discriminants(strum(to_string = "message type"))]
    Message(Message),
    #[strum_discriminants(strum(to_string = "channel"))]
    Channel(Channel),
}

impl Ty {
    /// Check if all values of `other` are compatible with `self`.
    ///
    /// A type is compatible if all values of `other` can be implicitly converted to a value of
    /// `self`. All integer types are considered to be compatible with each other. This semantics is
    /// used during model verification.
    ///
    /// # Panics
    ///
    /// Will panic if `self` or `other` is an instance of `AnyInteger`, `Composite` or `Compound`.
    pub fn is_compatible(&self, other: &Ty) -> bool {
        match (&self, other) {
            (Ty::AnyInteger | Ty::Composite | Ty::Compound, _)
            | (_, Ty::AnyInteger | Ty::Composite | Ty::Compound) => {
                panic!("unexpected type instance")
            }

            (Ty::Undefined, _) | (_, Ty::Undefined) => false,

            (Ty::Any, _)
            | (_, Ty::Any)
            | (
                Ty::UniversalInteger(..) | Ty::Integer(..),
                Ty::UniversalInteger(..) | Ty::Integer(..),
            ) => true,

            (Ty::Enumeration(enumeration), Ty::Enumeration(other)) => enumeration.id == other.id,
            (Ty::Aggregate(aggregate), Ty::Aggregate(other)) => {
                aggregate.element.is_compatible(&other.element)
            }
            (Ty::Aggregate(aggregate), Ty::Sequence(sequence))
            | (Ty::Sequence(sequence), Ty::Aggregate(aggregate)) => {
                aggregate.element.is_compatible_strong(&sequence.element)
            }
            (Ty::Sequence(sequence), Ty::Sequence(other)) => sequence.id == other.id,
            (Ty::Structure(structure), Ty::Structure(other)) => structure.id == other.id,
            (Ty::Message(message), Ty::Message(other)) => message.id == other.id,
            (Ty::Channel(channel), Ty::Channel(other)) => {
                (channel.readable || !other.readable) && (channel.writable || !other.writable)
            }
            (
                Ty::Enumeration(..)
                | Ty::UniversalInteger(..)
                | Ty::Integer(..)
                | Ty::Aggregate(..)
                | Ty::Sequence(..)
                | Ty::Structure(..)
                | Ty::Message(..)
                | Ty::Channel(..),
                _,
            ) => false,
        }
    }

    /// Check if all values of `other` are strongly compatible with `self`.
    ///
    /// Different integer types are not considered to be compatible with each other. This semantics
    /// is the same as in Ada/SPARK and is used to determine the need for explicit type conversions
    /// during code generation.
    pub fn is_compatible_strong(&self, other: &Ty) -> bool {
        match (self, other) {
            (Ty::Integer(integer), Ty::UniversalInteger(universal_integer))
            | (Ty::UniversalInteger(universal_integer), Ty::Integer(integer)) => {
                integer.bounds.contains_bounds(&universal_integer.bounds)
            }
            (Ty::Integer(_), Ty::Integer(_)) => self == other,
            _ => self.is_compatible(other),
        }
    }

    /// Determine common type.
    ///
    /// # Panics
    ///
    /// Will panic if `self` or `other` is an instance of `AnyInteger`, `Composite` or `Compound`.
    pub fn common_type(&self, other: &Ty) -> Ty {
        match (self, other) {
            (Ty::AnyInteger | Ty::Composite | Ty::Compound, _)
            | (_, Ty::AnyInteger | Ty::Composite | Ty::Compound) => {
                panic!("unexpected type instance")
            }
            (Ty::Undefined, _) | (_, Ty::Undefined) => Ty::Undefined,
            (_, Ty::Any) => self.to_owned(),
            (Ty::Any, _) => other.to_owned(),
            (Ty::UniversalInteger(universal_integer), Ty::UniversalInteger(other)) => {
                Ty::UniversalInteger(UniversalInteger {
                    bounds: universal_integer.bounds.merge(&other.bounds),
                })
            }
            (Ty::UniversalInteger(..), Ty::Integer(..))
            | (Ty::Integer(..), Ty::UniversalInteger(..))
            | (Ty::Integer(_), Ty::Integer(_)) => BASE_INTEGER.clone(),
            (Ty::Aggregate(aggregate), Ty::Aggregate(other)) => {
                if aggregate == other {
                    self.clone()
                } else {
                    let element = aggregate.element.common_type(&other.element);
                    if let Ty::UniversalInteger(universal_integer) = element {
                        Ty::Aggregate(Aggregate {
                            element: Box::new(Ty::UniversalInteger(universal_integer)),
                        })
                    } else if let Ty::Integer(integer) = element {
                        Ty::Aggregate(Aggregate {
                            element: Box::new(Ty::Integer(integer)),
                        })
                    } else {
                        Ty::Undefined
                    }
                }
            }
            (Ty::Aggregate(_), Ty::Sequence(_)) => {
                if self.is_compatible_strong(other) {
                    other.clone()
                } else {
                    Ty::Undefined
                }
            }
            (Ty::Sequence(_), Ty::Aggregate(_)) => {
                if other.is_compatible_strong(self) {
                    self.clone()
                } else {
                    Ty::Undefined
                }
            }
            (Ty::Sequence(sequence), Ty::Sequence(other)) => {
                if sequence == other {
                    self.clone()
                } else {
                    Ty::Undefined
                }
            }
            _ => {
                if self == other {
                    self.clone()
                } else {
                    Ty::Undefined
                }
            }
        }
    }
}

impl TyDiscriminants {
    pub fn is_instance(&self, other: &TyDiscriminants) -> bool {
        match (&self, other) {
            (TyDiscriminants::Undefined, TyDiscriminants::Any)
            | (TyDiscriminants::Any, TyDiscriminants::Undefined) => false,

            (_, TyDiscriminants::Any)
            | (TyDiscriminants::Any, _)
            | (
                TyDiscriminants::UniversalInteger | TyDiscriminants::Integer,
                TyDiscriminants::AnyInteger,
            )
            | (
                TyDiscriminants::Aggregate | TyDiscriminants::Sequence,
                TyDiscriminants::Composite,
            )
            | (TyDiscriminants::Structure | TyDiscriminants::Message, TyDiscriminants::Compound) => {
                true
            }

            _ => self == other,
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Ty::Enumeration(enumeration) => {
                    enumeration.to_string()
                }
                Ty::UniversalInteger(universal_integer) => universal_integer.to_string(),
                Ty::Integer(integer) => integer.to_string(),
                Ty::Aggregate(aggregate) => {
                    aggregate.to_string()
                }
                Ty::Sequence(sequence) => sequence.to_string(),
                Ty::Structure(structure) => structure.to_string(),
                Ty::Message(message) => message.to_string(),
                Ty::Channel(channel) => channel.to_string(),
                Ty::Any => {
                    TyDiscriminants::Any.to_string()
                }
                Ty::AnyInteger => {
                    TyDiscriminants::AnyInteger.to_string()
                }
                Ty::Composite => {
                    TyDiscriminants::Composite.to_string()
                }
                Ty::Compound => {
                    TyDiscriminants::Compound.to_string()
                }
                Ty::Undefined => {
                    TyDiscriminants::Undefined.to_string()
                }
            }
        )
    }
}

pub fn common_type(types: &[Ty]) -> Ty {
    types.iter().fold(Ty::Any, |r, t| r.common_type(t))
}

/// Check if the given type is compatible to the expected types.
///
/// # Panics
///
/// Will panic if `expected` is empty or location is `None`.
pub fn check_type(actual: &Ty, expected: &[Ty], location: &Location, description: &str) -> Error {
    assert!(!expected.is_empty());

    if *actual == Ty::Undefined {
        return undefined_type(location, description);
    }
    let mut error = Error::default();

    if !expected.contains(&Ty::Undefined) && !expected.iter().any(|e| actual.is_compatible(e)) {
        let desc = expected
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join(" or ");
        error.push(Entry::new(
            format!("expected {desc}"),
            Severity::Error,
            location.clone(),
            vec![Annotation::new(
                Some(format!("found {actual}")),
                Severity::Error,
                location.clone(),
            )],
            false,
        ));
    }

    error
}

/// Check if the given type is an instance of the expected types.
///
/// # Panics
///
/// Will panic if `expected` is empty or if an unexpected type was found and location is `None`.
pub fn check_type_instance(
    actual: &Ty,
    expected: &[TyDiscriminants],
    location: &Location,
    description: &str,
    additional_annotations: &[Annotation],
) -> Error {
    assert!(!expected.is_empty());

    if *actual == Ty::Undefined {
        return undefined_type(location, description);
    }
    let mut error = Error::default();

    if !expected
        .iter()
        .any(|e| TyDiscriminants::from(actual).is_instance(e))
    {
        let desc = expected
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join(" or ");
        let mut annotations = vec![];
        annotations.push(Annotation::new(
            Some(format!("found {actual}")),
            Severity::Error,
            location.clone(),
        ));
        annotations.extend(additional_annotations.iter().cloned());
        error.push(Entry::new(
            format!("expected {desc}"),
            Severity::Error,
            location.clone(),
            annotations,
            false,
        ));
    }

    error
}

fn undefined_type(location: &Location, description: &str) -> Error {
    let description = if description.is_empty() {
        String::new()
    } else {
        format!(" {description}")
    };
    Error::from(vec![Entry::new(
        format!("undefined{description}"),
        Severity::Error,
        location.clone(),
        vec![],
        true,
    )])
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Enumeration {
    pub id: ID,
    pub literals: Vec<ID>,
    pub always_valid: bool,
    pub location: Location,
}

impl Enumeration {
    pub const DESCRIPTIVE_NAME: &'static str = "enumeration type";
}

impl Display for Enumeration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} \"{}\"", Self::DESCRIPTIVE_NAME, self.id)
    }
}

impl PartialEq for Enumeration {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct UniversalInteger {
    pub bounds: Bounds,
}

impl UniversalInteger {
    pub const DESCRIPTIVE_NAME: &'static str = "type universal integer";
}

impl Display for UniversalInteger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", Self::DESCRIPTIVE_NAME, self.bounds)
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Integer {
    pub id: ID,
    pub bounds: Bounds,
    pub location: Location,
}

impl Integer {
    pub const DESCRIPTIVE_NAME: &'static str = "integer type";
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} \"{}\" ({})",
            Self::DESCRIPTIVE_NAME,
            self.id,
            self.bounds
        )
    }
}

impl PartialEq for Integer {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct Aggregate {
    pub element: Box<Ty>,
}

impl Aggregate {
    pub const DESCRIPTIVE_NAME: &'static str = "aggregate";
}

impl Display for Aggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} with element {}",
            Self::DESCRIPTIVE_NAME,
            self.element
        )
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Sequence {
    pub id: ID,
    pub element: Box<Ty>,
}

impl Sequence {
    pub const DESCRIPTIVE_NAME: &'static str = "sequence type";
}

impl Display for Sequence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} \"{}\" with element {}",
            Self::DESCRIPTIVE_NAME,
            self.id,
            self.element
        )
    }
}

impl PartialEq for Sequence {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Structure {
    pub id: ID,
    pub field_combinations: HashSet<Vec<String>>,
    pub parameter_types: IndexMap<ID, Box<Ty>>,
    pub field_types: IndexMap<ID, Box<Ty>>,
}

impl Structure {
    pub const DESCRIPTIVE_NAME: &'static str = "structure type";
}

impl Display for Structure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} \"{}\"", Self::DESCRIPTIVE_NAME, self.id)
    }
}

impl PartialEq for Structure {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Message {
    pub id: ID,
    pub field_combinations: HashSet<Vec<String>>,
    pub parameter_types: IndexMap<ID, Box<Ty>>,
    pub field_types: IndexMap<ID, Box<Ty>>,
    pub refinements: Vec<Refinement>,
    pub is_definite: bool,
}

impl Message {
    pub const DESCRIPTIVE_NAME: &'static str = "message type";
}

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} \"{}\"", Self::DESCRIPTIVE_NAME, self.id)
    }
}

impl PartialEq for Message {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct Refinement {
    pub field: ID,
    pub sdu: Message,
    pub package: ID,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct Channel {
    pub readable: bool,
    pub writable: bool,
}

impl Channel {
    pub const DESCRIPTIVE_NAME: &'static str = "channel";
}

impl Display for Channel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mode = match (self.readable, self.writable) {
            (true, true) => "readable and writable ",
            (true, false) => "readable ",
            (false, true) => "writable ",
            (false, false) => "",
        };
        write!(f, "{}{}", mode, Self::DESCRIPTIVE_NAME)
    }
}

#[must_use]
#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct Bounds {
    lower: i128,
    upper: i128,
}

impl Bounds {
    /// A range containing all values with `lower <= x <= upper`.
    ///
    /// # Panics
    ///
    /// Will panic if `lower > upper`.
    pub const fn new(lower: i128, upper: i128) -> Self {
        assert!(lower <= upper);
        Bounds { lower, upper }
    }

    pub const fn lower(&self) -> i128 {
        self.lower
    }

    pub const fn upper(&self) -> i128 {
        self.upper
    }

    pub fn contains_int(&self, value: i128) -> bool {
        self.lower <= value && value <= self.upper
    }

    pub fn contains_bounds(&self, bounds: &Bounds) -> bool {
        self.lower <= bounds.lower && bounds.upper <= self.upper
    }

    pub fn merge(&self, bounds: &Bounds) -> Bounds {
        Bounds {
            lower: self.lower.min(bounds.lower),
            upper: self.upper.max(bounds.upper),
        }
    }

    fn write_with_exponent(f: &mut impl std::fmt::Write, number: i128) -> std::fmt::Result {
        if number > u16::MAX.into() {
            let bits: u32 = i128::BITS - number.leading_zeros();
            let remaining = 2i128.checked_pow(bits).unwrap_or(i128::MAX) - number;
            debug_assert_eq!(
                2i128.checked_pow(bits).unwrap_or(i128::MAX) - remaining,
                number
            );

            if remaining > 0 {
                write!(f, "2**{bits} - {remaining}")
            } else {
                write!(f, "2**{bits}")
            }
        } else {
            write!(f, "{number}")
        }
    }
}

impl Display for Bounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::write_with_exponent(f, self.lower)?;
        write!(f, " .. ")?;
        Self::write_with_exponent(f, self.upper)
    }
}

/// Assumes types in the generated code semantically equivalent to:
///
/// ```ada
/// type Length is new Natural;
/// type Index is new Length range 1 .. Length'Last;
/// type Bit_Length is range 0 .. Length'Last * 8
/// ```
///
/// TODO(eng/recordflux/RecordFlux#317): Enable detection of failing range checks in the model
///
/// Potentially failing range checks will not be detected in the model, if custom types
/// with different bounds are used.
const LENGTH_BOUNDS: Bounds = Bounds::new(0, i128::pow(2, 31) - 1);
const BIT_LENGTH_BOUNDS: Bounds = Bounds::new(0, LENGTH_BOUNDS.upper() * 8);

pub const UNIVERSAL_INTEGER: Ty = Ty::UniversalInteger(UniversalInteger {
    bounds: Bounds::new(0, i128::pow(2, consts::MAX_SCALAR_SIZE) - 1),
});

lazy_static! {
    static ref BUILTINS_LOCATION: Location = Location::File {
        start: FilePosition::new(1, 1),
        end: FilePosition::new(1, 1),
        source: PathBuf::from(consts::BUILTINS_PACKAGE),
    };
    pub static ref BOOLEAN: Ty = Ty::Enumeration(Enumeration {
        id: create_id!(
            [consts::BUILTINS_PACKAGE, "Boolean"],
            BUILTINS_LOCATION.clone()
        ),
        literals: vec![
            create_id!(["False"], Location::None),
            create_id!(["True"], Location::None)
        ],
        always_valid: false,
        location: BUILTINS_LOCATION.clone(),
    });
    pub static ref INDEX: Ty = Ty::Integer(Integer {
        id: create_id!(
            [consts::BUILTINS_PACKAGE, "Index"],
            BUILTINS_LOCATION.clone(),
        ),
        bounds: Bounds::new(1, LENGTH_BOUNDS.upper()),
        location: BUILTINS_LOCATION.clone(),
    });
    pub static ref BIT_LENGTH: Ty = Ty::Integer(Integer {
        id: create_id!(
            [consts::BUILTINS_PACKAGE, "Bit_Length"],
            BUILTINS_LOCATION.clone(),
        ),
        bounds: BIT_LENGTH_BOUNDS.clone(),
        location: BUILTINS_LOCATION.clone(),
    });
    pub static ref BIT_INDEX: Ty = Ty::Integer(Integer {
        id: create_id!(
            [consts::BUILTINS_PACKAGE, "Bit_Index"],
            BUILTINS_LOCATION.clone(),
        ),
        bounds: Bounds::new(1, BIT_LENGTH_BOUNDS.upper()),
        location: BUILTINS_LOCATION.clone(),
    });
    pub static ref BASE_INTEGER: Ty = Ty::Integer(Integer {
        id: create_id!(
            [consts::BUILTINS_PACKAGE, "Base_Integer"],
            BUILTINS_LOCATION.clone(),
        ),
        bounds: Bounds::new(0, i128::pow(2, consts::MAX_SCALAR_SIZE) - 1),
        location: BUILTINS_LOCATION.clone(),
    });
    static ref BYTE: Ty = Ty::Integer(Integer {
        id: create_id!(
            [consts::INTERNAL_PACKAGE, "Byte"],
            BUILTINS_LOCATION.clone(),
        ),
        bounds: Bounds::new(0, 255),
        location: BUILTINS_LOCATION.clone(),
    });
    pub static ref OPAQUE: Ty = Ty::Sequence(Sequence {
        id: create_id!(
            [consts::INTERNAL_PACKAGE, "Opaque"],
            BUILTINS_LOCATION.clone(),
        ),
        element: Box::new(BYTE.clone()),
    });
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use indexmap::IndexMap;
    use indoc::indoc;
    use lazy_static::lazy_static;
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::{
        check_type, check_type_instance, common_type, Aggregate, Bounds, Channel, Enumeration,
        FilePosition, Integer, Location, Message, Refinement, Sequence, Structure, Ty,
        TyDiscriminants, UniversalInteger, BASE_INTEGER, BIT_LENGTH_BOUNDS, ID, LENGTH_BOUNDS,
        OPAQUE, UNIVERSAL_INTEGER,
    };
    use crate::create_id;

    lazy_static! {
        static ref A: ID = create_id!(["A"], Location::None);
        static ref B: ID = create_id!(["B"], Location::None);
        static ref ENUM_A: Ty = Ty::Enumeration(Enumeration {
            id: A.clone(),
            literals: vec![],
            always_valid: true,
            location: Location::None,
        });
        static ref ENUM_B: Ty = Ty::Enumeration(Enumeration {
            id: B.clone(),
            literals: vec![],
            always_valid: true,
            location: Location::None,
        });
        static ref INT_A: Ty = Ty::Integer(Integer {
            id: A.clone(),
            bounds: Bounds::new(1, 5),
            location: Location::None,
        });
        static ref INT_B: Ty = Ty::Integer(Integer {
            id: B.clone(),
            bounds: Bounds::new(5, 9),
            location: Location::None,
        });
        static ref UNIV_INT_1_3: Ty = Ty::UniversalInteger(UniversalInteger {
            bounds: Bounds::new(1, 3),
        });
        static ref UNIV_INT_7_9: Ty = Ty::UniversalInteger(UniversalInteger {
            bounds: Bounds::new(7, 9),
        });
        static ref UNIV_INT_1_9: Ty = Ty::UniversalInteger(UniversalInteger {
            bounds: Bounds::new(1, 9),
        });
        static ref AGG_INT_A: Ty = Ty::Aggregate(Aggregate {
            element: Box::new(INT_A.clone())
        });
        static ref AGG_INT_B: Ty = Ty::Aggregate(Aggregate {
            element: Box::new(INT_B.clone())
        });
        static ref AGG_1_3: Ty = Ty::Aggregate(Aggregate {
            element: Box::new(UNIV_INT_1_3.clone())
        });
        static ref AGG_7_9: Ty = Ty::Aggregate(Aggregate {
            element: Box::new(UNIV_INT_7_9.clone())
        });
        static ref AGG_1_9: Ty = Ty::Aggregate(Aggregate {
            element: Box::new(UNIV_INT_1_9.clone())
        });
        static ref AGG_BASE_INT: Ty = Ty::Aggregate(Aggregate {
            element: Box::new(BASE_INTEGER.clone())
        });
        static ref AGG_MSG_A: Ty = Ty::Aggregate(Aggregate {
            element: Box::new(MSG_A.clone())
        });
        static ref AGG_MSG_B: Ty = Ty::Aggregate(Aggregate {
            element: Box::new(MSG_B.clone())
        });
        static ref SEQ_A: Ty = Ty::Sequence(Sequence {
            id: A.clone(),
            element: Box::new(INT_A.clone())
        });
        static ref SEQ_B: Ty = Ty::Sequence(Sequence {
            id: B.clone(),
            element: Box::new(INT_B.clone())
        });
        static ref STRUCT_A: Ty = Ty::Structure(Structure {
            id: A.clone(),
            field_combinations: HashSet::new(),
            parameter_types: IndexMap::new(),
            field_types: IndexMap::new(),
        });
        static ref STRUCT_B: Ty = Ty::Structure(Structure {
            id: B.clone(),
            field_combinations: HashSet::new(),
            parameter_types: IndexMap::new(),
            field_types: IndexMap::new(),
        });
        static ref MSG_A: Ty = Ty::Message(Message {
            id: A.clone(),
            field_combinations: HashSet::from([vec!["A".to_string()]]),
            parameter_types: IndexMap::new(),
            field_types: IndexMap::from([(A.clone(), Box::new(OPAQUE.clone()))]),
            refinements: vec![REFINEMENT.clone()],
            is_definite: true,
        });
        static ref MSG_B: Ty = Ty::Message(Message {
            id: B.clone(),
            field_combinations: HashSet::new(),
            parameter_types: IndexMap::new(),
            field_types: IndexMap::new(),
            refinements: vec![],
            is_definite: true,
        });
        static ref REFINEMENT: Refinement = Refinement {
            package: A.clone(),
            field: B.clone(),
            sdu: Message {
                id: A.clone(),
                field_combinations: HashSet::new(),
                parameter_types: IndexMap::new(),
                field_types: IndexMap::new(),
                refinements: vec![],
                is_definite: true,
            },
        };
    }

    const CHAN: Ty = Ty::Channel(Channel {
        readable: false,
        writable: false,
    });
    const CHAN_R: Ty = Ty::Channel(Channel {
        readable: true,
        writable: false,
    });
    const CHAN_W: Ty = Ty::Channel(Channel {
        readable: false,
        writable: true,
    });
    const CHAN_RW: Ty = Ty::Channel(Channel {
        readable: true,
        writable: true,
    });

    #[rstest]
    #[case::undefined(&Ty::Undefined, "undefined type")]
    #[case::any(&Ty::Any, "any type")]
    #[case::enumeration(&ENUM_A, "enumeration type \"A\"")]
    #[case::any_integer(&Ty::AnyInteger, "integer type")]
    #[case::universal_integer(&UNIV_INT_1_3, "type universal integer (1 .. 3)")]
    #[case::integer(&INT_A, "integer type \"A\" (1 .. 5)")]
    #[case::composite(&Ty::Composite, "composite type")]
    #[case::aggregate(&AGG_INT_A, "aggregate with element integer type \"A\" (1 .. 5)")]
    #[case::sequence(&SEQ_A, "sequence type \"A\" with element integer type \"A\" (1 .. 5)")]
    #[case::compound(&Ty::Compound, "compound type")]
    #[case::structure(&STRUCT_A, "structure type \"A\"")]
    #[case::message(&MSG_A, "message type \"A\"")]
    #[case::channel(&CHAN, "channel")]
    #[case::channel_r(&CHAN_R, "readable channel")]
    #[case::channel_w(&CHAN_W, "writable channel")]
    #[case::channel_rw(&CHAN_RW, "readable and writable channel")]
    fn test_ty_display(#[case] ty: &Ty, #[case] expected: &str) {
        assert_eq!(ty.to_string(), expected);
    }

    #[rstest]
    #[case::enumeration_and_any(&ENUM_A, &Ty::Any, &ENUM_A, true)]
    #[case::enumeration_and_undefined(&ENUM_A, &Ty::Undefined, &Ty::Undefined, false)]
    #[case::enumeration_and_same(&ENUM_A, &ENUM_A, &ENUM_A, true)]
    #[case::enumeration_and_different(&ENUM_A, &ENUM_B, &Ty::Undefined, false)]
    #[case::enumeration_and_integer(&ENUM_A, &INT_A, &Ty::Undefined, false)]
    #[case::universal_integer_and_any(&UNIV_INT_1_3, &Ty::Any, &UNIV_INT_1_3, true)]
    #[case::universal_integer_and_same(&UNIV_INT_1_3, &UNIV_INT_1_3, &UNIV_INT_1_3, true)]
    #[case::universal_integer_and_undefined(&UNIV_INT_1_3, &Ty::Undefined, &Ty::Undefined, false)]
    #[case::universal_integer_and_different(&UNIV_INT_1_3, &UNIV_INT_7_9, &UNIV_INT_1_9, true)]
    #[case::universal_integer_and_integer(&UNIV_INT_1_3, &INT_A, &BASE_INTEGER, true)]
    #[case::universal_integer_and_integer_outside_bounds(&UNIV_INT_1_3, &INT_B, &BASE_INTEGER, false)]
    #[case::universal_integer_and_enumeration(&UNIV_INT_1_3, &ENUM_A, &Ty::Undefined, false)]
    #[case::integer_and_any(&INT_A, &Ty::Any, &INT_A, true)]
    #[case::integer_and_undefined(&INT_A, &Ty::Undefined, &Ty::Undefined, false)]
    #[case::integer_and_same(&INT_A, &INT_A, &BASE_INTEGER, true)]
    #[case::integer_and_different(&INT_A, &INT_B, &BASE_INTEGER, false)]
    #[case::integer_and_enumeration(&INT_A, &ENUM_A, &Ty::Undefined, false)]
    #[case::aggregate_and_any(&AGG_1_3, &Ty::Any, &AGG_1_3, true)]
    #[case::aggregate_and_undefined(&AGG_1_3, &Ty::Undefined, &Ty::Undefined, false)]
    #[case::aggregate_and_same_universal_integer(&AGG_1_3, &AGG_1_3, &AGG_1_3, true)]
    #[case::aggregate_and_same_integer(&AGG_INT_A, &AGG_INT_A, &AGG_INT_A, true)]
    #[case::aggregate_and_same_message(&AGG_MSG_A, &AGG_MSG_A, &AGG_MSG_A, true)]
    #[case::aggregate_and_aggregate_different_universal_integer(&AGG_1_3, &AGG_7_9, &AGG_1_9, true)]
    #[case::aggregate_and_aggregate_different_integer(&AGG_INT_A, &AGG_INT_B, &AGG_BASE_INT, true)]
    #[case::aggregate_and_aggregate_different_message(&AGG_MSG_A, &AGG_MSG_B, &Ty::Undefined, false)]
    #[case::aggregate_and_aggregate_different_type(&AGG_1_3, &ENUM_A, &Ty::Undefined, false)]
    #[case::sequence_and_any(&SEQ_A, &Ty::Any, &SEQ_A, true)]
    #[case::sequence_and_same(&SEQ_A, &SEQ_A, &SEQ_A, true)]
    #[case::sequence_and_undefined(&SEQ_A, &Ty::Undefined, &Ty::Undefined, false)]
    #[case::sequence_and_sequence_different_element(&SEQ_A, &SEQ_B, &Ty::Undefined, false)]
    #[case::sequence_and_aggregate_same_element(&SEQ_A, &AGG_INT_A, &SEQ_A, true)]
    #[case::sequence_and_aggregate_universal_integer(&SEQ_A, &AGG_1_3, &SEQ_A, true)]
    #[case::sequence_and_aggregate_different_integer(&SEQ_A, &AGG_INT_B, &Ty::Undefined, false)]
    #[case::sequence_and_aggregate_different_universal_integer(&SEQ_A, &AGG_7_9, &Ty::Undefined, false)]
    #[case::sequence_and_enumeration(&SEQ_A, &ENUM_A, &Ty::Undefined, false)]
    #[case::structure_and_any(&STRUCT_A, &Ty::Any, &STRUCT_A, true)]
    #[case::structure_and_undefined(&STRUCT_A, &Ty::Undefined, &Ty::Undefined, false)]
    #[case::structure_and_same(&STRUCT_A, &STRUCT_A, &STRUCT_A, true)]
    #[case::structure_and_different(&STRUCT_A, &STRUCT_B, &Ty::Undefined, false)]
    #[case::structure_and_enumeration(&STRUCT_A, &ENUM_A, &Ty::Undefined, false)]
    #[case::message_and_any(&MSG_A, &Ty::Any, &MSG_A, true)]
    #[case::message_and_undefined(&MSG_A, &Ty::Undefined, &Ty::Undefined, false)]
    #[case::message_and_same(&MSG_A, &MSG_A, &MSG_A, true)]
    #[case::message_and_different(&MSG_A, &MSG_B, &Ty::Undefined, false)]
    #[case::message_and_enumeration(&MSG_A, &ENUM_A, &Ty::Undefined, false)]
    #[case::channel_and_any(&CHAN_R, &Ty::Any, &CHAN_R, true)]
    #[case::channel_and_undefined(&CHAN_R, &Ty::Undefined, &Ty::Undefined, false)]
    #[case::channel_and_same(&CHAN_R, &CHAN_R, &CHAN_R, true)]
    #[case::channel_and_different(&CHAN_R, &CHAN_W, &Ty::Undefined, false)]
    #[case::channel_and_enumeration(&CHAN_R, &ENUM_A, &Ty::Undefined, false)]
    fn test_ty_compatibility(
        #[case] ty: &Ty,
        #[case] other: &Ty,
        #[case] common_type: &Ty,
        #[case] is_compatible_strong: bool,
    ) {
        assert_eq!(ty.common_type(other), *common_type);
        assert_eq!(other.common_type(ty), *common_type);
        assert_eq!(ty.is_compatible(other), *common_type != Ty::Undefined);
        assert_eq!(other.is_compatible(ty), *common_type != Ty::Undefined);
        assert_eq!(ty.is_compatible_strong(other), is_compatible_strong);
        assert_eq!(other.is_compatible_strong(ty), is_compatible_strong);
    }

    #[rstest]
    #[case(&Ty::AnyInteger, &Ty::Any)]
    #[case(&Ty::Composite, &Ty::Any)]
    #[case(&Ty::Compound, &Ty::Any)]
    #[case(&Ty::Any, &Ty::AnyInteger)]
    #[case(&Ty::Any, &Ty::Composite)]
    #[case(&Ty::Any, &Ty::Compound)]
    #[should_panic(expected = "unexpected type instance")]
    fn test_ty_is_compatible_panic(#[case] ty: &Ty, #[case] other: &Ty) {
        ty.is_compatible(other);
    }

    #[rstest]
    #[case(&Ty::AnyInteger, &Ty::Any)]
    #[case(&Ty::Composite, &Ty::Any)]
    #[case(&Ty::Compound, &Ty::Any)]
    #[case(&Ty::Any, &Ty::AnyInteger)]
    #[case(&Ty::Any, &Ty::Composite)]
    #[case(&Ty::Any, &Ty::Compound)]
    #[should_panic(expected = "unexpected type instance")]
    fn test_ty_common_type_panic(#[case] ty: &Ty, #[case] other: &Ty) {
        let _ = ty.common_type(other);
    }

    #[rstest]
    #[case::undefined(&Ty::Undefined)]
    #[case::any(&Ty::Any)]
    #[case::enumeration(&ENUM_A)]
    #[case::universal_integer(&UNIV_INT_1_3)]
    #[case::integer(&INT_A)]
    #[case::aggregate(&AGG_INT_A)]
    #[case::sequence(&SEQ_A)]
    #[case::structure(&STRUCT_A)]
    #[case::message(&MSG_A)]
    #[case::channel(&CHAN_R)]
    fn test_ty_serde(#[case] ty: &Ty) {
        let bytes = bincode::serialize(ty).expect("failed to serialize");
        let deserialized_ty: Ty = bincode::deserialize(&bytes).expect("failed to deserialize");
        assert_eq!(*ty, deserialized_ty);
    }

    #[rstest]
    #[case(&TyDiscriminants::Undefined, &TyDiscriminants::Any, false)]
    #[case(&TyDiscriminants::Any, &TyDiscriminants::Undefined, false)]
    #[case(&TyDiscriminants::Integer, &TyDiscriminants::Enumeration, false)]
    #[case(&TyDiscriminants::Enumeration, &TyDiscriminants::Enumeration, true)]
    #[case(&TyDiscriminants::UniversalInteger, &TyDiscriminants::AnyInteger, true)]
    #[case(&TyDiscriminants::Integer, &TyDiscriminants::AnyInteger, true)]
    #[case(&TyDiscriminants::Aggregate, &TyDiscriminants::Composite, true)]
    #[case(&TyDiscriminants::Sequence, &TyDiscriminants::Composite, true)]
    #[case(&TyDiscriminants::Structure, &TyDiscriminants::Compound, true)]
    #[case(&TyDiscriminants::Message, &TyDiscriminants::Compound, true)]
    fn test_ty_discriminants_is_instance(
        #[case] ty: &TyDiscriminants,
        #[case] other: &TyDiscriminants,
        #[case] expected: bool,
    ) {
        assert_eq!(ty.is_instance(other), expected);
    }

    #[rstest]
    #[case::empty(&[], &Ty::Any)]
    #[case::undefined(&[&CHAN_R, &*MSG_A], &Ty::Undefined)]
    #[case::integer(&[&*INT_A, &*UNIV_INT_1_3], &BASE_INTEGER)]
    #[case::aggregate(&[&*AGG_INT_A, &*AGG_INT_B], &AGG_BASE_INT)]
    fn test_common_type(#[case] types: &[&Ty], #[case] expected: &Ty) {
        assert_eq!(
            common_type(&types.iter().copied().cloned().collect::<Vec<Ty>>()),
            *expected
        );
    }

    #[rstest]
    #[case::valid(&Ty::Any, &[&Ty::Any], "")]
    #[case::expected_undefined(&*INT_A, &[&Ty::Undefined], "")]
    #[case::actual_undefined(
        &Ty::Undefined,
        &[&*INT_A],
        indoc! {r"
            <stdin>:1:1: error: undefined foo
        "},
    )]
    #[case::invalid(
        &ENUM_A,
        &[&*INT_A],
        indoc! {r#"
            <stdin>:1:1: error: expected integer type "A" (1 .. 5)
            <stdin>:1:1: error: found enumeration type "A"
        "#},
    )]
    fn test_check_type(#[case] actual: &Ty, #[case] types: &[&Ty], #[case] expected: &str) {
        assert_eq!(
            check_type(
                actual,
                &types.iter().copied().cloned().collect::<Vec<Ty>>(),
                &Location::Stdin {
                    start: FilePosition::new(1, 1),
                    end: FilePosition::new(1, 1),
                },
                "foo"
            )
            .to_string(),
            *expected.trim()
        );
    }

    #[rstest]
    #[case::valid(&Ty::Any, &[TyDiscriminants::Any], "")]
    #[case::actual_undefined(
        &Ty::Undefined,
        &[TyDiscriminants::AnyInteger],
        indoc! {r"
            <stdin>:1:1: error: undefined
        "},
    )]
    #[case::invalid(
        &ENUM_A,
        &[TyDiscriminants::AnyInteger],
        indoc! {r#"
            <stdin>:1:1: error: expected integer type
            <stdin>:1:1: error: found enumeration type "A"
        "#},
    )]
    fn test_check_type_instance(
        #[case] actual: &Ty,
        #[case] types: &[TyDiscriminants],
        #[case] expected: &str,
    ) {
        assert_eq!(
            check_type_instance(
                actual,
                types,
                &Location::Stdin {
                    start: FilePosition::new(1, 1),
                    end: FilePosition::new(1, 1),
                },
                "",
                &[]
            )
            .to_string(),
            *expected.trim()
        );
    }

    #[rstest]
    #[case(i128::MIN, i128::MIN)]
    #[case(i128::MIN, i128::MAX)]
    #[case(i128::MAX, i128::MAX)]
    fn test_bounds_new(#[case] lower: i128, #[case] upper: i128) {
        let bounds = Bounds::new(lower, upper);
        assert_eq!(bounds.lower(), lower);
        assert_eq!(bounds.upper(), upper);
    }

    #[rstest]
    #[case(i128::MIN + 1, i128::MIN)]
    #[case(i128::MAX, i128::MAX - 1)]
    #[should_panic(expected = "assertion failed: lower <= upper")]
    fn test_bounds_new_invalid(#[case] lower: i128, #[case] upper: i128) {
        let _ = Bounds::new(lower, upper);
    }

    #[rstest]
    #[case::below_bounds(Bounds::new(1, 10), 0, false)]
    #[case::lower_bound(Bounds::new(1, 10), 1, true)]
    #[case::in_bounds(Bounds::new(1, 10), 5, true)]
    #[case::upper_bound(Bounds::new(1, 10), 10, true)]
    #[case::above_bounds(Bounds::new(1, 10), 11, false)]
    fn test_bounds_contains_int(
        #[case] bounds: Bounds,
        #[case] value: i128,
        #[case] expected: bool,
    ) {
        assert_eq!(bounds.contains_int(value), expected);
    }

    #[rstest]
    #[case::below_bounds(Bounds::new(1, 10), Bounds::new(0, 1), false)]
    #[case::lower_bound(Bounds::new(1, 10), Bounds::new(1, 1), true)]
    #[case::in_bounds(Bounds::new(1, 10), Bounds::new(1, 10), true)]
    #[case::upper_bound(Bounds::new(1, 10), Bounds::new(10, 10), true)]
    #[case::above_bounds(Bounds::new(1, 10), Bounds::new(10, 11), false)]
    fn test_bounds_contains_bounds(
        #[case] bounds: Bounds,
        #[case] contained_bounds: Bounds,
        #[case] expected: bool,
    ) {
        assert_eq!(bounds.contains_bounds(&contained_bounds), expected);
    }

    #[rstest]
    #[case::overlapping_lower(Bounds::new(1, 9), Bounds::new(0, 2), Bounds::new(0, 9))]
    #[case::contained(Bounds::new(1, 9), Bounds::new(1, 9), Bounds::new(1, 9))]
    #[case::overlapping_upper(Bounds::new(1, 8), Bounds::new(7, 9), Bounds::new(1, 9))]
    #[case::non_overlapping(Bounds::new(1, 2), Bounds::new(3, 4), Bounds::new(1, 4))]
    fn test_bounds_merge(#[case] bounds: Bounds, #[case] other: Bounds, #[case] expected: Bounds) {
        let result = bounds.merge(&other);
        assert_eq!(result.lower, expected.lower);
        assert_eq!(result.upper, expected.upper);
    }

    #[test]
    fn test_bounds_serde() {
        let bounds = Bounds::new(1, 2);
        let bytes = bincode::serialize(&bounds).expect("failed to serialize");
        let deserialized_bounds = bincode::deserialize(&bytes).expect("failed to deserialize");
        assert_eq!(bounds, deserialized_bounds);
    }

    #[rstest]
    #[case::small_bound(Bounds::new(1, 2), "1 .. 2")]
    #[case::limit_bounds(
            Bounds::new((u64::MAX - 16).into(), (u64::MAX - 1).into()),
            "2**64 - 17 .. 2**64 - 2"
        )
    ]
    #[case::limit_upper_bound(
            Bounds::new(0, (u64::MAX - 1).into()),
            "0 .. 2**64 - 2"
        )
    ]
    #[case::bound_close_to_max_value(Bounds::new(0, i128::MAX - 1), "0 .. 2**127 - 1")]
    #[case::upper_bound_i128_max_value(Bounds::new(0, i128::MAX), "0 .. 2**127")]
    fn test_bounds_display(#[case] bound: Bounds, #[case] expected: &str) {
        assert_eq!(bound.to_string(), expected);
    }

    #[test]
    fn test_consts() {
        assert_eq!(LENGTH_BOUNDS.upper(), 2_147_483_647);
        assert_eq!(BIT_LENGTH_BOUNDS.upper(), 17_179_869_176);
        assert!(
            matches!(UNIVERSAL_INTEGER, Ty::UniversalInteger(i) if i.bounds.upper() == 9_223_372_036_854_775_807)
        );
    }
}
