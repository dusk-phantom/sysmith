use super::*;

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Exp,
    pub then: Stmt,
    pub else_then: Option<Stmt>,
}

#[derive(Debug, Clone)]
pub struct IfContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for IfContext<'_> {
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

        // Context for then and else is in loop
        c.in_loop = true;
        let then = c.arbitrary(u)?;
        let else_then = if u.arbitrary()? {
            Some(c.arbitrary(u)?)
        } else {
            None
        };
        Ok(Stmt::If(Box::new(If {
            cond,
            then,
            else_then,
        })))
    }
}

impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.else_then {
            Some(e) => write!(f, "if ({}) {} else {}", self.cond, self.then, e),
            None => write!(f, "if ({}) {}", self.cond, self.then),
        }
    }
}
