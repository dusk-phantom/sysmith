use super::*;

/// Current context of program generation
#[derive(Debug, Clone)]
pub struct Context {
    /// Mapping of variable names to their types
    pub ctx: HashMap<String, Type>,

    /// Mapping of variable names to their values
    pub env: HashMap<String, Value>,

    /// Expected type for the current expression
    pub expected_type: Type,

    /// Flag if expected type is a constant
    pub expected_const: bool,

    /// Expected return type for the current function
    pub return_type: Type,

    /// Flag if the current context is in a loop
    pub in_loop: bool,
}