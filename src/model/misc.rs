use super::*;

/// A record of map assignment.
/// Example: `x: 1`
#[derive(Clone, PartialEq, Debug, Arbitrary)]
pub struct MapEntry {
    pub id: String,
    pub expr: Expr,
}
