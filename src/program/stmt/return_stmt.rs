use super::*;

#[derive(Debug, Clone)]
pub struct Return {
    pub exp: Option<Exp>,
}

#[derive(Debug, Clone)]
pub struct ReturnContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for ReturnContext<'_> {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        let exp = match self.0.return_type {
            Type::Void => None,
            Type::Float | Type::Int => {
                // Context for returned expression expects return type
                let mut c = self.0.clone();
                c.expected = ExpectedType {
                    is_const: false,
                    value_type: c.return_type.clone(),
                    bound: NumBound::None,
                };
                Some(c.arbitrary(u)?)
            }
            _ => panic!("Invalid return type"),
        };
        Ok(Stmt::Return(Return { exp }))
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.exp {
            Some(a) => write!(f, "return {};", a),
            None => write!(f, "return;"),
        }
    }
}
