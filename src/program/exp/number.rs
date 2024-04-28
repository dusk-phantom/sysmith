use super::*;

/// NumberContext generates an arbitrary expression
/// which is a number literal.
/// Example: `4`
#[derive(Clone, Debug)]
pub struct NumberContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Exp> for NumberContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        self.0.expected.value_type.is_numeric()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        Ok(Exp::Number(self.0.arbitrary(u)?))
    }
}

#[derive(Debug, Clone)]
pub enum Number {
    IntConst(IntConst),
    FloatConst(FloatConst),
}

impl<'a> ArbitraryTo<'a, Number> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Number> {
        match self.expected.value_type {
            Type::Int => Ok(Number::IntConst(IntConst::arbitrary(u)?)),
            Type::Float => Ok(Number::FloatConst(FloatConst::arbitrary(u)?)),
            _ => panic!("Invalid type for a number literal"),
        }
    }
}

impl Eval for Number {
    fn eval(&self, _: &Context) -> Value {
        match self {
            Number::IntConst(a) => Value::Int(*a),
            Number::FloatConst(a) => Value::Float(*a),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::IntConst(a) => write!(f, "{}", a),
            Number::FloatConst(a) => write!(f, "{}", a),
        }
    }
}
