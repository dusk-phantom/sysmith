use super::*;

/// BinaryOpContext generates an arbitrary expression
/// which is a binary operation.
/// Example: `4 + 5`
#[derive(Clone, Debug)]
pub struct BinaryOpContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Exp> for BinaryOpContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        self.0.expected.value_type.is_numeric() && self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        // Generate LHS and operator first
        let a = Box::new(self.0.arbitrary(u)?);
        let b = BinaryOp::arbitrary(u)?;

        // Apply a bound to RHS according to binary operator generated
        let mut ctx = self.0.clone();
        if let BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod = b {
            ctx.expected.bound = NumBound::NonZero;
        }

        // Generate RHS
        let c = Box::new(ctx.arbitrary(u)?);
        Ok(Exp::OpExp((a, b, c)))
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Minus,
    Mul,
    Div,
    Mod,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

impl<'a> Arbitrary<'a> for BinaryOp {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        match u.int_in_range(0..=12)? {
            0 => Ok(BinaryOp::Add),
            1 => Ok(BinaryOp::Minus),
            2 => Ok(BinaryOp::Mul),
            3 => Ok(BinaryOp::Div),
            4 => Ok(BinaryOp::Mod),
            5 => Ok(BinaryOp::Less),
            6 => Ok(BinaryOp::LessOrEqual),
            7 => Ok(BinaryOp::Greater),
            8 => Ok(BinaryOp::GreaterOrEqual),
            9 => Ok(BinaryOp::Equal),
            10 => Ok(BinaryOp::NotEqual),
            11 => Ok(BinaryOp::And),
            12 => Ok(BinaryOp::Or),
            _ => unreachable!(),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessOrEqual => write!(f, "<="),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterOrEqual => write!(f, ">="),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
        }
    }
}
