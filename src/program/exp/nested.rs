use super::*;

/// NestedContext generates an arbitrary expression
/// which is in a pair of parenthesis.
/// Example: `(4)`
#[derive(Clone, Debug)]
pub struct NestedContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Exp> for NestedContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        Ok(Exp::Exp(Box::new(self.0.arbitrary(u)?)))
    }
}
