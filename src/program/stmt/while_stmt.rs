use super::*;

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Exp,
    pub body: Stmt,
}

#[derive(Debug, Clone)]
pub struct WhileContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for WhileContext<'_> {
    fn can_arbitrary(&self, _: std::marker::PhantomData<Stmt>) -> bool {
        self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        // Context for condition expects int type
        let mut c = self.0.clone();
        c.expected = ExpectedType {
            is_const: false,
            value_type: Type::Int,
            bound: NumBound::None,
        };
        let cond = c.arbitrary(u)?;

        // Context for body is in loop
        c.in_loop = true;
        let body = c.arbitrary(u)?;
        Ok(Stmt::While(Box::new(While { cond, body })))
    }
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {}", self.cond, self.body)
    }
}
