use std::marker::PhantomData;

use super::*;

pub trait ArbitraryTo<'a, T>: Sized {
    /// Generate an arbitrary instance of T,
    /// with respect to the given context (self)
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<T>;

    /// Check if generating an arbitrary instance is possible,
    /// does not consume bytes.
    /// Phantom data is required to specify the type it converts to.
    fn can_arbitrary(&self, _: PhantomData<T>) -> bool {
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