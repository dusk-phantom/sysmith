use super::*;

#[derive(Debug, Clone)]
pub struct BreakContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for BreakContext<'_> {
    fn can_arbitrary(&self, _: std::marker::PhantomData<Stmt>) -> bool {
        // Prevent break outside of loop
        self.0.in_loop
    }

    fn arbitrary(&self, _: &mut Unstructured<'a>) -> Result<Stmt> {
        // Prevent break outside of loop
        if !self.0.in_loop {
            panic!("can't break outside loop; call can_arbitrary before arbitrary");
        }
        Ok(Stmt::Break)
    }
}
