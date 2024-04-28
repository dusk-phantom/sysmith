use super::*;

/// Current context of program generation
#[derive(Debug, Clone)]
pub struct Context {
    /// Mapping of variable names to their types
    pub ctx: HashMap<String, Type>,

    /// Mapping of variable names to their values
    pub env: HashMap<String, Value>,

    /// Expected type for the current expression
    pub expected: ExpectedType,

    /// Expected return type for the current function
    pub return_type: Type,

    /// Flag if the current context is in a loop
    pub in_loop: bool,

    /// Current AST depth
    pub depth: i32,
}

impl Context {
    /// Create a new context with increased depth
    pub fn next(&self) -> Self {
        let mut c = self.clone();
        c.depth += 1;
        c
    }

    /// Check if depth exceeds
    pub fn depth_is_valid(&self) -> bool {
        self.depth <= MAX_DEPTH
    }
}
