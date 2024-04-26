use super::*;

pub trait ArbitraryInContext<'a>: Sized {
    /// Generate an arbitrary instance of Self,
    /// with respect to the given context
    fn arbitrary(g: &mut Unstructured<'a>, context: &Context) -> Result<Self>;
}

pub trait Eval {
    /// Evaluate the expression in the given context,
    /// Panics if the expression is not a constant.
    /// Error will be caught by libfuzzer.
    fn eval(&self, context: &Context) -> Value;
}
