use super::*;

#[derive(Debug, Clone)]
pub struct ContinueContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for ContinueContext<'_> {
    fn can_arbitrary(&self, _: std::marker::PhantomData<Stmt>) -> bool {
        // Prevent continue outside of loop
        self.0.in_loop
    }

    fn arbitrary(&self, _: &mut Unstructured<'a>) -> Result<Stmt> {
        // Prevent continue outside of loop
        if !self.0.in_loop {
            panic!("can't continue outside loop; call can_arbitrary before arbitrary");
        }
        Ok(Stmt::Continue)
    }
}
