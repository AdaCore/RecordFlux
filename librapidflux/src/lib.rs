#![deny(clippy::pedantic)]
#![allow(clippy::must_use_candidate)]
#![forbid(unsafe_code)]

pub mod consts;
pub mod diagnostics;
pub mod expr;
pub mod identifier;
pub mod source_code;
pub mod ty;
