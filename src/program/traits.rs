use super::*;

pub trait ArbitraryIn<'a, T>: Sized {
    /// Generate an arbitrary instance of Self,
    /// with respect to the given context
    fn arbitrary(g: &mut Unstructured<'a>, c: &T) -> Result<Self>;

    /// Check if generating an arbitrary instance is possible,
    /// does not consume bytes
    fn can_arbitrary(_: &T) -> bool {
        true
    }
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