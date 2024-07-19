use std::fmt::Display;

use serde::{Deserialize, Serialize};

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
    pub fn new(lower: i128, upper: i128) -> Self {
        assert!(lower <= upper);
        Bounds { lower, upper }
    }

    pub fn lower(&self) -> i128 {
        self.lower
    }

    pub fn upper(&self) -> i128 {
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

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::Bounds;

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
}
