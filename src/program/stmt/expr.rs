use super::*;

#[derive(Debug, Clone)]
pub struct ExpContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for ExpContext<'_> {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        let exp = if u.arbitrary()? {
            // Generate a random type for this expression
            let func_type = FuncType::arbitrary(u)?;

            // Initialize a context expecting this type
            let mut c = self.0.clone();
            c.expected = ExpectedType {
                is_const: false,
                value_type: func_type.clone().into(),
                bound: NumBound::None,
            };

            // Generate a random statement of this type (non-constant)
            // If impossible, generate a stray semicolon
            match c.arbitrary(u) {
                Ok(e) => Some(e),
                Err(_) => None,
            }
        } else {
            // Possible to generate a stray semicolon
            None
        };
        Ok(Stmt::ExpStmt(ExpStmt { exp }))
    }
}

#[derive(Debug, Clone)]
pub struct ExpStmt {
    pub exp: Option<Exp>,
}

impl Display for ExpStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.exp {
            Some(e) => write!(f, "{};", e),
            None => write!(f, ";"),
        }
    }
}
