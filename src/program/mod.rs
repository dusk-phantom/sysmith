pub use std::collections::HashMap;
pub use std::fmt::{Debug, Display};

pub use libfuzzer_sys::arbitrary::{self, Unstructured};
pub use libfuzzer_sys::arbitrary::{Arbitrary, Result};

pub mod array;
pub mod comp_unit;
pub mod context;
pub mod exp;
pub mod func_def;
pub mod ident;
pub mod stmt;
pub mod traits;
pub mod value;
pub mod value_type;
pub mod var_decl;

pub use array::*;
pub use comp_unit::*;
pub use context::*;
pub use exp::*;
pub use func_def::*;
pub use ident::*;
pub use stmt::*;
pub use traits::*;
pub use value::*;
pub use value_type::*;
pub use var_decl::*;

pub const MAX_VEC_LEN: i32 = 10;
pub const MAX_DEPTH: i32 = 10;

