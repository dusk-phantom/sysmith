use std::marker::PhantomData;

use super::*;

pub trait ArbitraryTo<'a, T> {
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

/// Check if any of the ArbitraryTo can generate an instance of T
pub fn can_arbitrary_any<'a, T>(v: &[Box<dyn ArbitraryTo<'a, T> + 'a>]) -> bool
{
    v.iter().any(|x| x.can_arbitrary(PhantomData))
}

/// Generate an arbitrary instance of T from any of the ArbitraryTo
pub fn arbitrary_any<'a, T>(u: &mut Unstructured<'a>, v: &[Box<dyn ArbitraryTo<'a, T> + '_>]) -> Result<T>
{
    // Get a mapping of nth avaliable ArbitraryTo
    let mut map = Vec::new();
    for (i, x) in v.iter().enumerate() {
        if x.can_arbitrary(PhantomData) {
            map.push(i);
        }
    }

    // Randomly select one of the available ArbitraryTo
    let i = u.choose(&map)?;
    v[*i].arbitrary(u)
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