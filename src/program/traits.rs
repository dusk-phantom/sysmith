use super::*;

pub trait ArbitraryInContext<'a>: Sized {
    /// Generate an arbitrary instance of Self,
    /// with respect to the given context
    fn arbitrary(g: &mut Unstructured<'a>, context: &Context) -> Result<Self>;
}

pub trait Resolve {
    /// Resolve generated declaration to a given context.
    /// Typically means adding self to `ctx` (Name -> Type) 
    /// or `env` (Name -> Value) when self is constant
    fn resolve(&self, context: &mut Context);
}

pub trait Eval {
    /// Evaluate the expression in the given context,
    /// Panics if the expression is not a constant.
    /// Error will be caught by libfuzzer.
    fn eval(&self, context: &Context) -> Value;
}
