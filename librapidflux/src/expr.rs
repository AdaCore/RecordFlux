use std::fmt::Display;

use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
#[cfg(test)]
use strum::EnumIter;
use strum::{Display, FromRepr};

use crate::{
    create_id,
    diagnostics::{error::Error, location::Location},
    identifier::ID,
    ty,
};

#[derive(Display, Clone, Debug, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(test, derive(EnumIter))]
pub enum Precedence {
    Undefined = 0,
    BooleanOperator = 1,
    RelationalOperator = 2,
    BinaryAddingOperator = 3,
    UnaryAddingOperator = 4,
    MultiplyingOperator = 5,
    HighestPrecedenceOperator = 6,
    Literal = 7,
}

#[must_use]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    Var(Sym),
    Lit(Sym),
    Num(Num),
    Neg(Neg),
    BinExpr(BinExpr),
}

impl Expr {
    pub fn var(id: ID, ty: ty::Ty) -> Self {
        Self::Var(Sym { id, ty })
    }

    pub fn lit(id: ID, ty: ty::Ty) -> Self {
        Self::Lit(Sym { id, ty })
    }

    pub fn num(value: i64, location: Location) -> Self {
        Self::Num(Num {
            value,
            base: NumBase::Default,
            location,
        })
    }

    pub fn num_with_base(value: i64, base: NumBase, location: Location) -> Self {
        Self::Num(Num {
            value,
            base,
            location,
        })
    }

    pub fn neg(expr: Self, location: Location) -> Self {
        Self::Neg(Neg {
            expr: Box::new(expr),
            location,
        })
    }

    pub fn bin_expr(op: BinOp, left: Expr, right: Expr, location: Location) -> Self {
        Self::BinExpr(BinExpr {
            op,
            left: Box::new(left),
            right: Box::new(right),
            location,
        })
    }

    pub fn location(&self) -> &Location {
        match self {
            Self::Var(sym) | Self::Lit(sym) => sym.id.location(),
            Self::Num(num) => &num.location,
            Self::Neg(neg) => &neg.location,
            Self::BinExpr(bin_expr) => &bin_expr.location,
        }
    }

    pub fn check_type(&self, expected: &[ty::Ty]) -> Error {
        let mut error = self.check_sub_expr_type();
        if error.entries().is_empty() {
            error.extend(
                ty::check_type(&self.ty(), expected, self.location(), &self.expr_name())
                    .into_entries(),
            );
        }
        error
    }

    pub fn check_type_instance(&self, expected: &[ty::TyDiscriminants]) -> Error {
        let mut error = self.check_sub_expr_type();
        if error.entries().is_empty() {
            error.extend(
                ty::check_type_instance(
                    &self.ty(),
                    expected,
                    self.location(),
                    &self.expr_name(),
                    &[],
                )
                .into_entries(),
            );
        }
        error
    }

    fn check_sub_expr_type(&self) -> Error {
        match self {
            Self::Num(..) | Self::Var(..) | Self::Lit(..) => Error::default(),
            Self::Neg(neg) => neg.check_sub_expr_type(),
            Self::BinExpr(bin_expr) => bin_expr.check_sub_expr_type(),
        }
    }

    pub fn variables(&self) -> Vec<Expr> {
        self.find_all(&|e: &Expr| matches!(e, Self::Var(_)))
    }

    pub fn find_all(&self, f: &impl Fn(&Expr) -> bool) -> Vec<Expr> {
        match self {
            Self::Var(..) | Self::Lit(..) | Self::Num(..) => {
                if f(self) {
                    vec![self.clone()]
                } else {
                    vec![]
                }
            }
            Self::Neg(neg) => neg.find_all(f),
            Self::BinExpr(bin_expr) => bin_expr.find_all(f),
        }
    }

    /// # Panics
    ///
    /// Will panic if called on a non-negatable expression.
    pub fn into_negated(self) -> Expr {
        match self {
            Self::Var(..) => Self::neg(self.clone(), self.location().clone()),
            Self::Lit(..) => panic!("literal cannot be negated"),
            Self::Num(num) => num.into_negated(),
            Self::Neg(neg) => neg.into_negated(),
            Self::BinExpr(bin_expr) => bin_expr.into_negated(),
        }
    }

    pub fn into_simplified(self) -> Expr {
        match self {
            Self::Var(..) | Self::Lit(..) | Self::Num(..) => self,
            Self::Neg(neg) => neg.into_simplified(),
            Self::BinExpr(bin_expr) => bin_expr.into_simplified(),
        }
    }

    pub fn into_substituted(self, f: &impl Fn(&Expr) -> Expr) -> Expr {
        match self {
            Self::Var(..) | Self::Lit(..) | Self::Num(..) => f(&self),
            Self::Neg(neg) => f(&neg.into_substituted(f)),
            Self::BinExpr(bin_expr) => f(&bin_expr.into_substituted(f)),
        }
    }

    fn ty(&self) -> ty::Ty {
        match self {
            Self::Var(sym) | Self::Lit(sym) => sym.ty.clone(),
            Self::Num(num) => num.ty(),
            Self::Neg(neg) => neg.ty(),
            Self::BinExpr(bin_expr) => bin_expr.ty(),
        }
    }

    fn expr_name(&self) -> String {
        let expr_type = match self {
            Self::Num(..) | Self::Neg(..) | Self::BinExpr(..) => "expression",
            Self::Var(..) => "variable",
            Self::Lit(..) => "literal",
        };
        let expr_name = match self {
            Self::Num(..) | Self::Neg(..) | Self::BinExpr(..) => self.to_string(),
            Self::Var(sym) | Self::Lit(sym) => sym.id.to_string(),
        };
        format!("{expr_type} \"{expr_name}\"")
    }

    fn parenthesized(&self, expr: &Expr) -> String {
        if expr.precedence() <= self.precedence() {
            format!("({expr})")
        } else {
            expr.to_string()
        }
    }

    fn precedence(&self) -> Precedence {
        match self {
            Self::Var(sym) | Self::Lit(sym) => sym.precedence(),
            Self::Num(num) => num.precedence(),
            Self::Neg(neg) => neg.precedence(),
            Self::BinExpr(bin_expr) => bin_expr.precedence(),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Num(num) => num.to_string(),
                Self::Var(sym) | Self::Lit(sym) => sym.id.to_string(),
                Self::Neg(neg) => neg.to_string(),
                Self::BinExpr(bin_expr) => bin_expr.to_string(),
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Sym {
    pub id: ID,
    pub ty: ty::Ty,
}

impl Sym {
    #[allow(clippy::unused_self)]
    pub fn precedence(&self) -> Precedence {
        Precedence::Literal
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Num {
    pub value: i64,
    pub base: NumBase,
    pub location: Location,
}

impl Num {
    pub fn into_negated(self) -> Expr {
        Expr::num_with_base(-self.value, self.base, self.location)
    }

    #[allow(clippy::unused_self)]
    pub fn precedence(&self) -> Precedence {
        Precedence::Literal
    }

    pub fn ty(&self) -> ty::Ty {
        ty::Ty::UniversalInteger(ty::UniversalInteger {
            bounds: ty::Bounds::new(self.value.into(), self.value.into()),
        })
    }
}

impl PartialEq for Num {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Display for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = self.value.abs();
        let r = match self.base {
            NumBase::Default => format!("{value}"),
            NumBase::Bin => format!("2#{value:b}#"),
            NumBase::Oct => format!("8#{value:o}#"),
            NumBase::Dec => format!("10#{value}#"),
            NumBase::Hex => format!("16#{value:X}#"),
        };
        write!(
            f,
            "{}",
            if self.value < 0 {
                format!("(-{r})")
            } else {
                r.to_string()
            }
        )
    }
}

#[derive(FromRepr, Copy, Clone, Debug, Serialize, Deserialize)]
#[cfg_attr(test, derive(EnumIter, PartialEq))]
#[repr(u8)]
pub enum NumBase {
    Default = 0,
    Bin = 2,
    Oct = 8,
    Dec = 10,
    Hex = 16,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Neg {
    pub expr: Box<Expr>,
    pub location: Location,
}

impl Neg {
    pub fn find_all(&self, f: &impl Fn(&Expr) -> bool) -> Vec<Expr> {
        let mut result = vec![];
        let e = Expr::Neg(self.clone());
        if f(&e) {
            result.push(e);
        }
        result.extend(self.expr.find_all(f));
        result
    }

    pub fn into_negated(self) -> Expr {
        *self.expr
    }

    pub fn into_simplified(self) -> Expr {
        let self_expr = Expr::Neg(self.clone());
        let simplified_expr = self.expr.into_simplified().into_negated();
        if simplified_expr == self_expr {
            self_expr
        } else {
            simplified_expr
        }
    }

    pub fn into_substituted(self, f: &impl Fn(&Expr) -> Expr) -> Expr {
        f(&Expr::neg(self.expr.into_substituted(f), self.location))
    }

    #[allow(clippy::unused_self)]
    pub fn precedence(&self) -> Precedence {
        Precedence::UnaryAddingOperator
    }

    pub fn ty(&self) -> ty::Ty {
        ty::common_type(&[self.expr.ty().clone()])
    }

    fn check_sub_expr_type(&self) -> Error {
        let mut error = Error::default();

        error.extend(
            self.expr
                .check_type_instance(&[ty::TyDiscriminants::AnyInteger])
                .into_entries(),
        );

        error
    }
}

impl PartialEq for Neg {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl Display for Neg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let self_expr = Expr::Neg(self.clone());
        write!(f, "-{}", self_expr.parenthesized(&self.expr))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum BinOp {
    Sub,
    Div,
    Pow,
    Mod,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Sub => "-",
                Self::Div => "/",
                Self::Pow => "**",
                Self::Mod => "mod",
            }
        )
    }
}

#[allow(clippy::module_name_repetitions)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BinExpr {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub location: Location,
}

impl BinExpr {
    pub fn find_all(&self, f: &impl Fn(&Expr) -> bool) -> Vec<Expr> {
        let mut result = vec![];
        let e = Expr::BinExpr(self.clone());
        if f(&e) {
            result.push(e);
        }
        result.extend(self.left.find_all(f));
        result.extend(self.right.find_all(f));
        result
    }

    pub fn into_negated(self) -> Expr {
        match self.op {
            BinOp::Sub => Expr::BinExpr(BinExpr {
                op: self.op,
                left: self.right,
                right: self.left,
                location: self.location.clone(),
            }),
            BinOp::Div => Expr::BinExpr(BinExpr {
                op: self.op,
                left: Box::new(self.left.into_negated()),
                right: self.right,
                location: self.location.clone(),
            }),
            BinOp::Pow | BinOp::Mod => Expr::neg(
                Expr::BinExpr(BinExpr {
                    op: self.op,
                    left: self.left,
                    right: self.right,
                    location: self.location.clone(),
                }),
                self.location.clone(),
            ),
        }
    }

    /// # Panics
    ///
    /// Will panic if the expression contains an invalid operation.
    pub fn into_simplified(self) -> Expr {
        let left = self.left.clone().into_simplified();
        let right = self.right.clone().into_simplified();
        match (left, right) {
            (
                Expr::Num(Num {
                    value: l,
                    base: _,
                    location: _,
                }),
                Expr::Num(Num {
                    value: r,
                    base: _,
                    location: _,
                }),
            ) => match self.op {
                BinOp::Sub => Expr::Num(Num {
                    value: l - r,
                    base: NumBase::Default,
                    location: self.location,
                }),
                BinOp::Div => {
                    if l % r == 0 {
                        Expr::Num(Num {
                            value: l / r,
                            base: NumBase::Default,
                            location: self.location,
                        })
                    } else {
                        Expr::BinExpr(self)
                    }
                }
                BinOp::Pow => {
                    if r >= 0 {
                        Expr::Num(Num {
                            value: l.pow(u32::try_from(r).expect("too big exponent")),
                            base: NumBase::Default,
                            location: self.location,
                        })
                    } else {
                        panic!("negative exponent")
                    }
                }
                BinOp::Mod => {
                    if r != 0 {
                        Expr::Num(Num {
                            value: l.rem_euclid(r) + if r < 0 { r } else { 0 },
                            base: NumBase::Default,
                            location: self.location,
                        })
                    } else {
                        panic!("modulo by zero")
                    }
                }
            },
            (
                Expr::Num(Num {
                    value: 0,
                    base: _,
                    location,
                }),
                other,
            ) => match self.op {
                BinOp::Sub => other.into_negated().into_simplified(),
                BinOp::Div | BinOp::Pow | BinOp::Mod => Expr::num(0, location),
            },
            (
                other,
                Expr::Num(Num {
                    value: 0,
                    base: _,
                    location,
                }),
            ) => match self.op {
                BinOp::Sub => other.into_simplified(),
                BinOp::Div => panic!("division by zero"),
                BinOp::Pow => Expr::num(1, location),
                BinOp::Mod => panic!("modulo by zero"),
            },
            (Expr::Lit(_), Expr::Lit(_)) => panic!("invalid operation"),
            _ => Expr::BinExpr(BinExpr {
                op: self.op,
                left: Box::new(self.left.into_simplified()),
                right: Box::new(self.right.into_simplified()),
                location: self.location,
            }),
        }
    }

    pub fn into_substituted(self, f: &impl Fn(&Expr) -> Expr) -> Expr {
        f(&Expr::bin_expr(
            self.op,
            self.left.into_substituted(f),
            self.right.into_substituted(f),
            self.location,
        ))
    }

    pub fn precedence(&self) -> Precedence {
        match self.op {
            BinOp::Sub => Precedence::BinaryAddingOperator,
            BinOp::Div | BinOp::Mod => Precedence::MultiplyingOperator,
            BinOp::Pow => Precedence::HighestPrecedenceOperator,
        }
    }

    pub fn ty(&self) -> ty::Ty {
        ty::common_type(&[self.left.ty().clone(), self.right.ty().clone()])
    }

    fn check_sub_expr_type(&self) -> Error {
        let mut error = Error::default();

        for e in &[&self.left, &self.right] {
            error.extend(
                e.check_type_instance(&[ty::TyDiscriminants::AnyInteger])
                    .into_entries(),
            );
        }

        error
    }
}

impl PartialEq for BinExpr {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op && self.left == other.left && self.right == other.right
    }
}

impl Display for BinExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let self_expr = Expr::BinExpr(self.clone());
        write!(
            f,
            "{} {} {}",
            self_expr.parenthesized(&self.left),
            self.op,
            self_expr.parenthesized(&self.right),
        )
    }
}

lazy_static! {
    static ref TRUE_SYM: Sym = Sym {
        id: create_id!(["True"], Location::None),
        ty: ty::BOOLEAN.clone(),
    };
    static ref FALSE_SYM: Sym = Sym {
        id: create_id!(["False"], Location::None),
        ty: ty::BOOLEAN.clone(),
    };
    pub static ref TRUE: Expr = Expr::Lit(TRUE_SYM.clone());
    pub static ref FALSE: Expr = Expr::Lit(FALSE_SYM.clone());
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use strum::IntoEnumIterator;

    use super::*;
    use crate::{
        consts, create_id,
        diagnostics::{
            error::{Annotation, Entry, Severity},
            location::FilePosition,
        },
    };

    #[test]
    fn test_precedence_display() {
        for precedence in Precedence::iter() {
            assert_eq!(
                precedence.to_string(),
                match precedence {
                    Precedence::Undefined => "Undefined",
                    Precedence::BooleanOperator => "BooleanOperator",
                    Precedence::RelationalOperator => "RelationalOperator",
                    Precedence::BinaryAddingOperator => "BinaryAddingOperator",
                    Precedence::UnaryAddingOperator => "UnaryAddingOperator",
                    Precedence::MultiplyingOperator => "MultiplyingOperator",
                    Precedence::HighestPrecedenceOperator => "HighestPrecedenceOperator",
                    Precedence::Literal => "Literal",
                }
            );
        }
    }

    #[test]
    fn test_precedence_serde() {
        for precedence in Precedence::iter() {
            let bytes = bincode::serialize(&precedence).expect("failed to serialize");
            let deserialized_precedence: Precedence =
                bincode::deserialize(&bytes).expect("failed to deserialize");
            assert_eq!(precedence, deserialized_precedence);
        }
    }

    #[rstest]
    #[case::var(var("X", location(1)))]
    #[case::lit(lit("X", location(1)))]
    #[case::num(Expr::num(42, location(1)))]
    #[case::neg(Expr::neg(Expr::num(42, location(2)), location(1)))]
    #[case::bin_expr(Expr::bin_expr(
        BinOp::Sub,
        Expr::num(2, location(3)),
        Expr::num(1, location(2)),
        location(1)
    ))]
    fn test_expr_location(#[case] expression: Expr) {
        assert_eq!(*expression.location(), location(1));
    }

    #[rstest]
    #[case::var(
        var("X", location(1)),
        vec![var("X", location(1))]
    )]
    #[case::lit(
        lit("X", location(1)),
        vec![]
    )]
    #[case::num(
        Expr::num(42, location(1)),
        vec![]
    )]
    #[case::neg(
        Expr::neg(Expr::num(42, location(2)), location(1)),
        vec![]
    )]
    #[case::bin_expr(
        Expr::bin_expr(BinOp::Sub, Expr::num(2, location(3)), Expr::num(1, location(2)), location(1)),
        vec![]
    )]
    #[case::bin_expr(
        Expr::bin_expr(BinOp::Sub, var("X", location(2)), var("Y", location(3)), location(1)),
        vec![var("X", location(2)), var("Y", location(3))]
    )]
    fn test_expr_variables(#[case] expression: Expr, #[case] expected: Vec<Expr>) {
        assert_eq!(*expression.variables(), expected);
    }

    #[rstest]
    #[case::var(
        var("X", location(1)),
        |_: &Expr| false,
        vec![]
    )]
    #[case::var(
        var("X", location(1)),
        |e: &Expr| matches!(e, Expr::Var(_)),
        vec![var("X", location(1))]
    )]
    #[case::lit(
        lit("X", location(1)),
        |_: &Expr| false,
        vec![]
    )]
    #[case::lit(
        lit("X", location(1)),
        |e: &Expr| matches!(e, Expr::Lit(_)),
        vec![lit("X", location(1))]
    )]
    #[case::num(
        Expr::num(42, location(1)),
        |_: &Expr| false,
        vec![]
    )]
    #[case::num(
        Expr::num(42, location(1)),
        |e: &Expr| matches!(e, Expr::Num(_)),
        vec![Expr::num(42, location(1))]
    )]
    #[case::neg(
        Expr::neg(Expr::num(42, location(2)), location(1)),
        |_: &Expr| false,
        vec![]
    )]
    #[case::neg(
        Expr::neg(Expr::num(42, location(2)), location(1)),
        |e: &Expr| matches!(e, Expr::Num(_)),
        vec![Expr::num(42, location(2))]
    )]
    #[case::neg(
        Expr::neg(Expr::num(42, location(2)), location(1)),
        |e: &Expr| matches!(e, Expr::Neg(_)),
        vec![Expr::neg(Expr::num(42, location(2)), location(1))]
    )]
    #[case::bin_expr(
        Expr::bin_expr(BinOp::Sub, Expr::num(2, location(3)), Expr::num(1, location(2)), location(1)),
        |_: &Expr| false,
        vec![]
    )]
    #[case::bin_expr(
        Expr::bin_expr(BinOp::Sub, Expr::num(2, location(3)), Expr::num(1, location(2)), location(1)),
        |e: &Expr| matches!(e, Expr::Num(_)),
        vec![Expr::num(2, location(3)), Expr::num(1, location(2))]
    )]
    #[case::bin_expr(
        Expr::bin_expr(BinOp::Sub, var("X", location(2)), var("Y", location(3)), location(1)),
        |e: &Expr| matches!(e, Expr::BinExpr(_)),
        vec![Expr::bin_expr(BinOp::Sub, var("X", location(2)), var("Y", location(3)), location(1))]
    )]
    fn test_expr_find_all(
        #[case] expression: Expr,
        #[case] f: impl Fn(&Expr) -> bool,
        #[case] expected: Vec<Expr>,
    ) {
        assert_eq!(*expression.find_all(&f), expected);
    }

    #[rstest]
    #[case::var(var("X", location(1)), Expr::neg(var("X", location(1)), location(1)))]
    #[should_panic(expected = "literal cannot be negated")]
    #[case::lit(lit("X", location(1)), lit("X", location(1)))]
    #[case::num(
        Expr::num(42, location(1)),
        Expr::num(-42, location(1))
    )]
    #[case::neg(
        Expr::neg(Expr::num(42, location(2)), location(1)),
        Expr::num(42, location(2))
    )]
    #[case::sub(
        Expr::bin_expr(
            BinOp::Sub,
            var("X", location(2)),
            Expr::num(1, location(3)),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(1, location(3)),
            var("X", location(2)),
            location(1)
        )
    )]
    #[case::div(
        Expr::bin_expr(
            BinOp::Div,
            Expr::num(2, location(3)),
            var("X", location(2)),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Div,
            Expr::num(-2, location(3)),
            var("X", location(2)),
            location(1)
        ),
    )]
    #[case::pow(
        Expr::bin_expr(
            BinOp::Pow,
            Expr::num(2, location(3)),
            var("X", location(2)),
            location(1)
        ),
        Expr::neg(
            Expr::bin_expr(
                BinOp::Pow,
                Expr::num(2, location(3)),
                var("X", location(2)),
                location(1)
            ),
            location(1)
        )
    )]
    fn test_expr_into_negated(#[case] expression: Expr, #[case] expected: Expr) {
        assert_eq!(expression.into_negated(), expected);
    }

    #[rstest]
    #[case::var(var("X", location(1)), var("X", location(1)))]
    #[case::lit(lit("X", location(1)), lit("X", location(1)))]
    #[case::num(Expr::num(42, location(1)), Expr::num(42, location(1)))]
    #[case::neg_var(
        Expr::neg(var("X", location(2)), location(1)),
        Expr::neg(var("X", location(2)), location(1))
    )]
    #[case::neg_var(
        Expr::neg(Expr::neg(var("X", location(1)), Location::None), Location::None),
        var("X", location(1))
    )]
    #[case::neg_var(
        Expr::neg(
            Expr::neg(Expr::neg(var("X", Location::None), Location::None), Location::None),
            Location::None
        ),
        Expr::neg(var("X", Location::None), Location::None)
    )]
    #[case::neg_num(
        Expr::neg(Expr::num(42, Location::None), location(1)),
        Expr::num(-42, Location::None)
    )]
    #[case::neg_num(
        Expr::neg(
            Expr::neg(Expr::num(42, Location::None), Location::None),
            Location::None
        ),
        Expr::num(42, Location::None)
    )]
    #[case::neg_num(
        Expr::neg(
        Expr::neg(
            Expr::neg(Expr::num(42, Location::None), Location::None),
            Location::None
        ),
            Location::None
        ),
        Expr::num(-42, Location::None)
    )]
    #[case::sub_ident(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(1, Location::None),
            var("X", Location::None),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(1, Location::None),
            var("X", Location::None),
            location(1)
        )
    )]
    #[case::sub_num(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(1, Location::None),
            Expr::num(2, Location::None),
            location(1)
        ),
        Expr::num(-1, location(1))
    )]
    #[case::sub_nested(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::bin_expr(
                BinOp::Sub,
                Expr::num(1, Location::None),
                Expr::num(2, Location::None),
                Location::None,
            ),
            Expr::bin_expr(
                BinOp::Sub,
                Expr::num(1, Location::None),
                Expr::bin_expr(
                    BinOp::Sub,
                    Expr::num(2, Location::None),
                    Expr::num(3, Location::None),
                    Location::None,
                ),
                Location::None,
            ),
            location(1)
        ),
        Expr::num(-3, location(1))
    )]
    #[case::sub_zero(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::bin_expr(
                BinOp::Sub,
                var("X", Location::None),
                Expr::num(0, Location::None),
                Location::None,
            ),
            Expr::bin_expr(
                BinOp::Sub,
                Expr::num(0, Location::None),
                var("Y", Location::None),
                Location::None,
            ),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Sub,
            var("X", Location::None),
            Expr::neg(var("Y", Location::None), Location::None),
            location(1)
        )
    )]
    #[case::div_ident(
        Expr::bin_expr(
            BinOp::Div,
            Expr::num(1, Location::None),
            Expr::num(2, Location::None),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Div,
            Expr::num(1, Location::None),
            Expr::num(2, Location::None),
            location(1)
        )
    )]
    #[case::div_num(
        Expr::bin_expr(
            BinOp::Div,
            Expr::num(4, Location::None),
            Expr::num(2, Location::None),
            location(1)
        ),
        Expr::num(2, location(1))
    )]
    #[case::div_nested(
        Expr::bin_expr(
            BinOp::Div,
            Expr::bin_expr(
                BinOp::Div,
                Expr::num(8, Location::None),
                Expr::num(2, Location::None),
                Location::None,
            ),
            Expr::bin_expr(
                BinOp::Div,
                Expr::num(6, Location::None),
                Expr::num(3, Location::None),
                Location::None,
            ),
            location(1)
        ),
        Expr::num(2, location(1))
    )]
    #[case::div_zero_dividend(
        Expr::bin_expr(
            BinOp::Div,
            Expr::num(0, location(1)),
            var("X", Location::None),
            Location::None
        ),
        Expr::num(0, location(1))
    )]
    #[should_panic(expected = "division by zero")]
    #[case::div_zero_divisor(
        Expr::bin_expr(
            BinOp::Div,
            var("X", Location::None),
            Expr::num(0, location(1)),
            Location::None
        ),
        Expr::num(0, location(1))
    )]
    #[case::pow_ident(
        Expr::bin_expr(
            BinOp::Pow,
            var("X", Location::None),
            Expr::num(2, Location::None),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Pow,
            var("X", Location::None),
            Expr::num(2, Location::None),
            location(1)
        )
    )]
    #[case::pow_num(
        Expr::bin_expr(
            BinOp::Pow,
            Expr::num(4, Location::None),
            Expr::num(2, Location::None),
            location(1)
        ),
        Expr::num(16, location(1))
    )]
    #[case::pow_nested(
        Expr::bin_expr(
            BinOp::Pow,
            Expr::bin_expr(
                BinOp::Pow,
                Expr::num(2, Location::None),
                Expr::num(3, Location::None),
                Location::None,
            ),
            Expr::bin_expr(
                BinOp::Pow,
                Expr::num(2, Location::None),
                Expr::num(2, Location::None),
                Location::None,
            ),
            location(1)
        ),
        Expr::num(4096, location(1))
    )]
    #[case::pow_zero_base(
        Expr::bin_expr(
            BinOp::Pow,
            Expr::num(0, Location::None),
            var("X", Location::None),
            location(1)
        ),
        Expr::num(0, location(1))
    )]
    #[case::pow_zero_exponent(
        Expr::bin_expr(
            BinOp::Pow,
            var("X", Location::None),
            Expr::num(0, Location::None),
            location(1)
        ),
        Expr::num(1, location(1))
    )]
    #[should_panic(expected = "negative exponent")]
    #[case::pow_negative_exponent(
        Expr::bin_expr(
            BinOp::Pow,
            Expr::num(2, Location::None),
            Expr::num(-1, Location::None),
            Location::None
        ),
        Expr::bin_expr(
            BinOp::Pow,
            Expr::num(2, Location::None),
            Expr::num(-1, Location::None),
            Location::None
        )
    )]
    #[should_panic(expected = "too big exponent")]
    #[case::pow_too_big_exponent(
        Expr::bin_expr(
            BinOp::Pow,
            Expr::num(2, Location::None),
            Expr::num(i64::MAX, Location::None),
            Location::None
        ),
        Expr::bin_expr(
            BinOp::Pow,
            Expr::num(2, Location::None),
            Expr::num(i64::MAX, Location::None),
            Location::None
        )
    )]
    #[case::mod_ident(
        Expr::bin_expr(
            BinOp::Mod,
            var("X", Location::None),
            Expr::num(2, Location::None),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Mod,
            var("X", Location::None),
            Expr::num(2, Location::None),
            location(1)
        )
    )]
    #[case::mod_num(
        Expr::bin_expr(
            BinOp::Mod,
            Expr::num(4, Location::None),
            Expr::num(3, Location::None),
            location(1)
        ),
        Expr::num(1, location(1))
    )]
    #[case::mod_num_neg(
        Expr::bin_expr(
            BinOp::Mod,
            Expr::num(-7, Location::None),
            Expr::num(-3, Location::None),
            location(1)
        ),
        Expr::num(-1, location(1))
    )]
    #[case::mod_nested(
        Expr::bin_expr(
            BinOp::Mod,
            Expr::bin_expr(
                BinOp::Mod,
                Expr::num(8, Location::None),
                Expr::num(5, Location::None),
                Location::None,
            ),
            Expr::bin_expr(
                BinOp::Mod,
                Expr::num(5, Location::None),
                Expr::num(3, Location::None),
                Location::None,
            ),
            location(1)
        ),
        Expr::num(1, location(1))
    )]
    #[case::mod_zero_dividend(
        Expr::bin_expr(
            BinOp::Mod,
            Expr::num(0, location(1)),
            var("X", Location::None),
            Location::None
        ),
        Expr::num(0, location(1))
    )]
    #[should_panic(expected = "modulo by zero")]
    #[case::mod_zero_divisor_num(
        Expr::bin_expr(
            BinOp::Mod,
            Expr::num(2, location(1)),
            Expr::num(0, location(1)),
            Location::None
        ),
        Expr::num(0, location(1))
    )]
    #[should_panic(expected = "modulo by zero")]
    #[case::mod_zero_divisor_var(
        Expr::bin_expr(
            BinOp::Mod,
            var("X", Location::None),
            Expr::num(0, location(1)),
            Location::None
        ),
        Expr::num(0, location(1))
    )]
    #[should_panic(expected = "invalid operation")]
    #[case::sub_invalid_operation(
        Expr::bin_expr(
            BinOp::Sub,
            TRUE.clone(),
            FALSE.clone(),
        location(1)),
        TRUE.clone()
    )]
    fn test_expr_into_simplified(#[case] expression: Expr, #[case] expected: Expr) {
        assert_eq!(expression.into_simplified(), expected);
    }

    #[rstest]
    #[case::var(
        var("X", location(1)),
        &|e: &Expr| Expr::num(1, e.location().clone()),
        Expr::num(1, location(1)),
    )]
    #[case::lit(
        lit("X", location(1)),
        &|e: &Expr| Expr::num(1, e.location().clone()),
        Expr::num(1, location(1)),
    )]
    #[case::num(
        Expr::num(42, location(1)),
        &|e: &Expr| var("X", e.location().clone()),
        var("X", location(1)),
    )]
    #[case::neg(
        Expr::neg(Expr::num(42, location(2)), location(1)),
        &|e: &Expr|
        if matches!(e, Expr::Neg(_)) {
            var("X", e.location().clone())
        } else {
            e.clone()
        },
        var("X", location(1)),
    )]
    #[case::neg_nested(
        Expr::neg(Expr::num(42, location(2)), location(1)),
        &|e: &Expr|
        if matches!(e, Expr::Num(n) if n.value == 42) {
            var("X", e.location().clone())
        } else {
            e.clone()
        },
        Expr::neg(var("X", location(1)), location(1)),
    )]
    #[case::bin_expr(
        Expr::bin_expr(BinOp::Sub, Expr::num(2, location(3)), Expr::num(1, location(2)), location(1)),
        &|e: &Expr|
        if matches!(e, Expr::BinExpr(_)) {
            var("X", e.location().clone())
        } else {
            e.clone()
        },
        var("X", location(1))
    )]
    #[case::bin_expr_nested(
        Expr::bin_expr(BinOp::Sub, Expr::num(2, location(3)), Expr::num(1, location(2)), location(1)),
        &|e: &Expr|
        if matches!(e, Expr::Num(_)) {
            var("X", e.location().clone())
        } else {
            e.clone()
        },
        Expr::bin_expr(BinOp::Sub, var("X", location(3)), var("X", location(2)), location(1)),
    )]
    fn test_expr_into_substituted(
        #[case] expression: Expr,
        #[case] f: &impl Fn(&Expr) -> Expr,
        #[case] expected: Expr,
    ) {
        assert_eq!(expression.into_substituted(f), expected);
    }

    #[rstest]
    #[case::var(var("X", Location::None), ty::BASE_INTEGER.clone())]
    #[case::lit(lit("X", Location::None), ty::BASE_INTEGER.clone())]
    #[case::num(
        Expr::num(1, Location::None),
        ty::Ty::UniversalInteger(ty::UniversalInteger { bounds: ty::Bounds::new(1, 1) })
    )]
    #[case::neg(
        Expr::neg(Expr::num(1, Location::None), Location::None),
        ty::Ty::UniversalInteger(ty::UniversalInteger { bounds: ty::Bounds::new(1, 1) })
    )]
    #[case::sub(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::bin_expr(
                BinOp::Sub,
                var("X", Location::None),
                Expr::num(1, Location::None),
                Location::None
            ),
            Expr::num(2, Location::None),
            Location::None
        ),
        ty::BASE_INTEGER.clone()
    )]
    #[case::div(
        Expr::bin_expr(
            BinOp::Div,
            Expr::bin_expr(
                BinOp::Div,
                var("X", Location::None),
                Expr::num(1, Location::None),
                Location::None
            ),
            TRUE.clone(),
            Location::None
        ),
        ty::Ty::Undefined
    )]
    fn test_expr_ty(#[case] expression: Expr, #[case] expected: ty::Ty) {
        assert_eq!(expression.ty(), expected);
    }

    #[rstest]
    #[case::var(
        Expr::var(create_id!(["X"], Location::None), int_ty()),
        int_ty()
    )]
    #[case::lit(
        TRUE.clone(),
        ty::BOOLEAN.clone(),
    )]
    #[case::num(
        Expr::num(1, Location::None),
        ty::Ty::UniversalInteger(ty::UniversalInteger {
            bounds: ty::Bounds::new(1, 1),
        })
    )]
    #[case::neg(
        Expr::neg(Expr::num(1, Location::None), Location::None),
        ty::Ty::UniversalInteger(ty::UniversalInteger {
            bounds: ty::Bounds::new(1, 1),
        })
    )]
    #[case::sub(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::bin_expr(
                BinOp::Sub,
                Expr::var(create_id!(["X"], Location::None), int_ty()),
                Expr::num(1, Location::None),
                Location::None
            ),
            Expr::num(2, Location::None),
            Location::None
        ),
        int_ty()
    )]
    #[case::div(
        Expr::bin_expr(
            BinOp::Div,
            Expr::bin_expr(
                BinOp::Div,
                Expr::var(create_id!(["X"], Location::None), int_ty()),
                Expr::num(1, Location::None),
                Location::None
            ),
            Expr::num(2, Location::None),
            Location::None
        ),
        int_ty()
    )]
    #[case::pow(
        Expr::bin_expr(
            BinOp::Pow,
            Expr::bin_expr(
                BinOp::Pow,
                Expr::var(create_id!(["X"], Location::None), int_ty()),
                Expr::num(1, Location::None),
                Location::None
            ),
            Expr::num(2, Location::None),
            Location::None
        ),
        int_ty()
    )]
    fn test_expr_check_type(#[case] expression: Expr, #[case] expected: ty::Ty) {
        assert_eq!(expression.check_type(&[expected.clone()]).entries(), vec![]);
        assert_eq!(expression.ty(), expected);
    }

    #[rstest]
    #[case::neg(
        Expr::neg(Expr::var(create_id!(["X"], location(1)), ty::BOOLEAN.clone()), Location::None),
        &[
            Entry::new(
                "expected integer type".to_string(),
                Severity::Error,
                location(1),
                vec![
                    Annotation::new(
                        Some("found enumeration type \"__BUILTINS__::Boolean\"".to_string()),
                        Severity::Error,
                        location(1),
                    )
                ],
                false
            )
        ]
    )]
    #[case::bin_expr(
        Expr::bin_expr(
            BinOp::Div,
            Expr::bin_expr(
                BinOp::Sub,
                var("X", Location::None),
                Expr::neg(Expr::num(1, Location::None), Location::None),
                Location::None
            ),
            Expr::num(2, Location::None),
            location(1)
        ),
        &[
            Entry::new(
                "expected enumeration type \"__BUILTINS__::Boolean\"".to_string(),
                Severity::Error,
                location(1),
                vec![Annotation::new(Some("found integer type \"__BUILTINS__::Base_Integer\" (0 .. 2**63 - 1)".to_string()), Severity::Error, location(1) )],
                false
            )
        ]
    )]
    #[case::bin_expr_sub_expr(
        Expr::bin_expr(
            BinOp::Div,
            Expr::bin_expr(
                BinOp::Sub,
                Expr::var(create_id!(["X"], location(1)), ty::BOOLEAN.clone()),
                Expr::neg(Expr::num(1, Location::None), Location::None),
                Location::None
            ),
            Expr::num(2, Location::None),
                Location::None
        ),
        &[
            Entry::new(
                "expected integer type".to_string(),
                Severity::Error,
                location(1),
                vec![Annotation::new(Some("found enumeration type \"__BUILTINS__::Boolean\"".to_string()), Severity::Error, location(1) )],
                false
            )
        ]
    )]
    fn test_expr_check_type_error(#[case] expression: Expr, #[case] expected: &[Entry]) {
        assert_eq!(
            expression.check_type(&[ty::BOOLEAN.clone()]),
            expected.to_vec().into()
        );
    }

    #[rstest]
    #[case::var(var("X", location(1)), "variable \"X\"")]
    #[case::lit(lit("X", location(1)), "literal \"X\"")]
    #[case::num(Expr::num(42, location(1)), "expression \"42\"")]
    #[case::neg(
        Expr::neg(Expr::num(42, location(2)), location(1)),
        "expression \"-42\""
    )]
    fn test_expr_expr_name(#[case] expression: Expr, #[case] expected: &str) {
        assert_eq!(expression.expr_name(), expected);
    }

    #[rstest]
    #[case::var(var("X", location(1)), var("X", location(2)))]
    #[case::lit(lit("X", location(1)), lit("X", location(2)))]
    #[case::num(Expr::num(42, location(1)), Expr::num(42, location(2)))]
    #[case::neg(
        Expr::neg(Expr::num(42, location(2)), location(1)),
        Expr::neg(Expr::num(42, location(4)), location(3))
    )]
    #[case::bin_expr(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(2, location(3)),
            Expr::num(1, location(2)),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(2, location(6)),
            Expr::num(1, location(5)),
            location(4)
        )
    )]
    fn test_expr_eq(#[case] left: Expr, #[case] right: Expr) {
        assert_eq!(left, right);
    }

    #[rstest]
    #[case::var(var("X", Location::None), var("Y", Location::None))]
    #[case::lit(lit("X", Location::None), lit("Y", Location::None))]
    #[case::num(Expr::num(42, Location::None), Expr::num(0, Location::None))]
    #[case::neg(
        Expr::neg(Expr::num(42, Location::None), Location::None),
        Expr::neg(var("X", Location::None), Location::None)
    )]
    #[case::bin_expr_op(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(2, location(3)),
            Expr::num(1, location(2)),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Div,
            Expr::num(2, location(6)),
            Expr::num(1, location(5)),
            location(4)
        )
    )]
    #[case::bin_expr_left(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(2, location(3)),
            Expr::num(1, location(2)),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(1, location(6)),
            Expr::num(1, location(5)),
            location(4)
        )
    )]
    #[case::bin_expr_right(
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(2, location(3)),
            Expr::num(1, location(2)),
            location(1)
        ),
        Expr::bin_expr(
            BinOp::Sub,
            Expr::num(2, location(6)),
            Expr::num(2, location(5)),
            location(4)
        )
    )]
    fn test_expr_ne(#[case] left: Expr, #[case] right: Expr) {
        assert_ne!(left, right);
    }

    #[rstest]
    #[case::var(var("X", Location::None), "X")]
    #[case::lit(lit("X", Location::None), "X")]
    #[case::num(Expr::num(42, Location::None), "42")]
    #[case::num_negative(Expr::num(-42, Location::None), "(-42)")]
    #[case::num_bin(Expr::num_with_base(42, NumBase::Bin, Location::None), "2#101010#")]
    #[case::num_oct(Expr::num_with_base(42, NumBase::Oct, Location::None), "8#52#")]
    #[case::num_dec(Expr::num_with_base(42, NumBase::Dec, Location::None), "10#42#")]
    #[case::num_hex(Expr::num_with_base(42, NumBase::Hex, Location::None), "16#2A#")]
    #[case::neg(Expr::neg(var("X", Location::None), Location::None), "-X")]
    #[case::neg_neg(
        Expr::neg(Expr::neg(var("X", Location::None), Location::None), Location::None),
        "-(-X)"
    )]
    #[case::bin_expr(
        Expr::bin_expr(
            BinOp::Mod,
            Expr::bin_expr(
                BinOp::Div,
                Expr::bin_expr(
                    BinOp::Sub,
                    Expr::bin_expr(
                        BinOp::Pow,
                        var("X", Location::None),
                        Expr::num(2, Location::None),
                        Location::None
                    ),
                    Expr::num(1, Location::None),
                    Location::None
                ),
                Expr::neg(Expr::num(2, Location::None), Location::None),
                Location::None
            ),
            Expr::num(3, Location::None),
            Location::None
        ),
        "((X ** 2 - 1) / (-2)) mod 3"
    )]
    fn test_expr_display(#[case] expression: Expr, #[case] expected: &str) {
        assert_eq!(expression.to_string(), expected);
    }

    #[rstest]
    #[case::var(var("X", location(1)))]
    #[case::lit(lit("X", location(1)))]
    #[case::num(Expr::num(42, location(1)))]
    #[case::neg(Expr::neg(Expr::num(42, location(2)), location(1)))]
    #[case::bin_expr(Expr::bin_expr(
        BinOp::Div,
        Expr::bin_expr(
            BinOp::Sub,
            var("X", Location::None),
            Expr::num(1, Location::None),
            Location::None
        ),
        Expr::neg(Expr::num(2, Location::None), Location::None),
        Location::None
    ))]
    fn test_expr_serde(#[case] expr: Expr) {
        let bytes = bincode::serialize(&expr).expect("failed to serialize");
        let deserialized_expr: Expr = bincode::deserialize(&bytes).expect("failed to deserialize");
        assert_eq!(expr, deserialized_expr);
    }

    #[test]
    fn test_expr_precedence() {
        assert_eq!(lit("X", Location::None).precedence(), Precedence::Literal);
    }

    #[test]
    fn test_sym_precedence() {
        assert_eq!(
            Sym {
                id: create_id!(["X"], Location::None),
                ty: ty::BASE_INTEGER.clone()
            }
            .precedence(),
            Precedence::Literal
        );
    }

    #[test]
    fn test_num_precedence() {
        assert_eq!(
            Num {
                value: 42,
                base: NumBase::Default,
                location: Location::None
            }
            .precedence(),
            Precedence::Literal
        );
    }

    #[test]
    fn test_neg_precedence() {
        assert_eq!(
            Neg {
                expr: Box::new(Expr::Num(Num {
                    value: 42,
                    base: NumBase::Default,
                    location: Location::None
                })),
                location: Location::None
            }
            .precedence(),
            Precedence::UnaryAddingOperator
        );
    }

    #[test]
    fn test_num_base_from_repr() {
        for base in NumBase::iter() {
            assert_eq!(NumBase::from_repr(base as u8).unwrap(), base);
        }
    }

    fn location(num: u32) -> Location {
        Location::Stdin {
            start: FilePosition::new(num, num),
            end: FilePosition::new(num, num),
        }
    }

    fn var(id: &str, location: Location) -> Expr {
        Expr::var(create_id!([id], location), ty::BASE_INTEGER.clone())
    }

    fn lit(id: &str, location: Location) -> Expr {
        Expr::lit(create_id!([id], location), ty::BASE_INTEGER.clone())
    }

    fn int_ty() -> ty::Ty {
        ty::Ty::Integer(ty::Integer {
            id: create_id!([consts::BUILTINS_PACKAGE, "Base_Integer"], Location::None,),
            bounds: ty::Bounds::new(0, i128::pow(2, consts::MAX_SCALAR_SIZE) - 1),
            location: Location::None,
        })
    }
}
