use super::*;

#[derive(Debug, Clone)]
pub struct Exp {
    pub add_exp: Box<BinaryExp>,
}

impl Exp {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        let add_exp = BinaryExp::arbitrary(u, c)?;
        Ok(Exp {
            add_exp: Box::new(add_exp),
        })
    }

    pub fn eval(&self, ctx: &Context) -> Value {
        self.add_exp.eval(ctx)
    }
}

impl Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.add_exp)
    }
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub id: Ident,
    pub exp_vec: Vec<Exp>,
}

impl LVal {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // TODO make arbitrary identifier type match
        let id = Ident::arbitrary(u)?;
        let exp_vec = Vec::new();
        Ok(LVal { id, exp_vec })
    }

    pub fn arbitrary_infer(u: &mut Unstructured, c: &Context) -> Result<(Self, Type)> {
        // TODO make arbitrary identifier type match
        let id = Ident::arbitrary(u)?;
        let exp_vec = Vec::new();
        Ok((LVal { id, exp_vec }, Type::Int))
    }

    pub fn eval(&self, ctx: &Context) -> Value {
        let mut array_type = ctx.ctx.get(&self.id.to_string()).unwrap().clone();
        let mut array_value = ctx.env.get(&self.id.to_string()).unwrap().clone();
        for exp in &self.exp_vec {
            let index_value: Value = exp.eval(ctx);
            match (array_type, array_value) {
                (Type::Array(content_type, content_len), Value::Array(content_value)) => {
                    if let Value::Int(i) = index_value {
                        if i < 0 || i >= content_len {
                            panic!("Index out of range")
                        }
                        array_type = *content_type;
                        array_value = content_value[i as usize].clone();
                    } else {
                        panic!("Index must be an integer")
                    }
                }
                _ => panic!("Not an array"),
            }
        }
        array_value
    }
}

impl Display for LVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.id,
            self.exp_vec
                .iter()
                .map(|a| format!("[{}]", a))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(Number),
}

impl PrimaryExp {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
            0 => Ok(PrimaryExp::Exp(Box::new(Exp::arbitrary(u, c)?))),
            1 => Ok(PrimaryExp::LVal(LVal::arbitrary(u, c)?)),
            2 => Ok(PrimaryExp::Number(Number::arbitrary(u, c)?)),
            _ => unreachable!(),
        }
    }

    pub fn eval(&self, ctx: &Context) -> Value {
        match self {
            PrimaryExp::Exp(a) => a.eval(ctx),
            PrimaryExp::LVal(a) => a.eval(ctx),
            PrimaryExp::Number(a) => a.eval(ctx),
        }
    }
}

impl Display for PrimaryExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimaryExp::Exp(a) => write!(f, "{}", a),
            PrimaryExp::LVal(a) => write!(f, "{}", a),
            PrimaryExp::Number(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Number {
    IntConst(IntConst),
    FloatConst(FloatConst),
}

impl Number {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match c.expected_type {
            Type::Int => Ok(Number::IntConst(IntConst::arbitrary(u)?)),
            Type::Float => Ok(Number::FloatConst(FloatConst::arbitrary(u)?)),
            _ => panic!("Invalid type for a number literal"),
        }
    }

    pub fn eval(&self, _: &Context) -> Value {
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

#[derive(Debug, Clone)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    FuncCall((Ident, Option<FuncRParams>)),
    OpUnary((UnaryOp, Box<UnaryExp>)),
}

impl UnaryExp {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
            0 => Ok(UnaryExp::PrimaryExp(Box::new(PrimaryExp::arbitrary(u, c)?))),
            1 => {
                // TODO make sure function type match
                let id = Ident::arbitrary(u)?;
                let func_rparams = if u.arbitrary()? {
                    Some(FuncRParams::arbitrary(u, c)?)
                } else {
                    None
                };
                Ok(UnaryExp::FuncCall((id, func_rparams)))
            }
            2 => {
                let unary_op = UnaryOp::arbitrary(u)?;
                let unary_exp = UnaryExp::arbitrary(u, c)?;
                Ok(UnaryExp::OpUnary((unary_op, Box::new(unary_exp))))
            }
            _ => unreachable!(),
        }
    }

    pub fn eval(&self, ctx: &Context) -> Value {
        match self {
            UnaryExp::PrimaryExp(a) => a.eval(ctx),
            UnaryExp::FuncCall((_, _)) => {
                panic!("No function is constant")
            }
            UnaryExp::OpUnary((op, exp)) => {
                let exp = exp.eval(ctx);
                match op {
                    UnaryOp::Add => exp,
                    UnaryOp::Minus => -exp,
                    UnaryOp::Exclamation => if exp == Value::Int(0) {
                        Value::Int(1)
                    } else {
                        Value::Int(0)
                    },
                }
            }
        }
    }
}

impl Display for UnaryExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryExp::PrimaryExp(a) => write!(f, "{}", a),
            UnaryExp::FuncCall((id, None)) => write!(f, "{}()", id),
            UnaryExp::FuncCall((id, Some(param))) => write!(f, "{}({})", id, param),
            UnaryExp::OpUnary((a, b)) => write!(f, "{}({})", a, b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Add,
    Minus,
    Exclamation,
}

impl UnaryOp {
    pub fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
            0 => Ok(UnaryOp::Add),
            1 => Ok(UnaryOp::Minus),
            2 => Ok(UnaryOp::Exclamation),
            _ => unreachable!(),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Add => write!(f, "+"),
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Exclamation => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncRParams {
    pub exp_vec: Vec<Exp>,
}

impl FuncRParams {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        let mut exp_vec = Vec::new();
        loop {
            // TODO make param type match
            let exp = Exp::arbitrary(u, c)?;
            exp_vec.push(exp);
            if u.arbitrary()? {
                return Ok(FuncRParams { exp_vec });
            }
        }
    }
}

impl Display for FuncRParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.exp_vec
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
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

impl BinaryOp {
    pub fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        match u.arbitrary::<u8>()? % 13 {
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

#[derive(Debug, Clone)]
pub enum BinaryExp {
    UnaryExp(Box<UnaryExp>),
    OpExp((Box<BinaryExp>, BinaryOp, UnaryExp)),
}

impl BinaryExp {
    pub fn arbitrary(u: &mut Unstructured, ctx: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(BinaryExp::UnaryExp(Box::new(UnaryExp::arbitrary(u, ctx)?))),
            1 => {
                let a = BinaryExp::arbitrary(u, ctx)?;
                let b = BinaryOp::arbitrary(u)?;
                let c = UnaryExp::arbitrary(u, ctx)?;
                Ok(BinaryExp::OpExp((Box::new(a), b, c)))
            }
            _ => unreachable!(),
        }
    }

    pub fn eval(&self, ctx: &Context) -> Value {
        match self {
            Self::UnaryExp(a) => a.eval(ctx),
            Self::OpExp((a, b, c)) => {
                let a = a.eval(ctx);
                let c = c.eval(ctx);
                match b {
                    BinaryOp::Add => a + c,
                    BinaryOp::Minus => a - c,
                    BinaryOp::Mul => a * c,
                    BinaryOp::Div => a / c,
                    BinaryOp::Mod => a % c,
                    BinaryOp::Less => Value::Int(if a < c { 1 } else { 0 }),
                    BinaryOp::LessOrEqual => Value::Int(if a <= c { 1 } else { 0 }),
                    BinaryOp::Greater => Value::Int(if a > c { 1 } else { 0 }),
                    BinaryOp::GreaterOrEqual => Value::Int(if a >= c { 1 } else { 0 }),
                    BinaryOp::Equal => Value::Int(if a == c { 1 } else { 0 }),
                    BinaryOp::NotEqual => Value::Int(if a != c { 1 } else { 0 }),
                    BinaryOp::And => Value::Int(if a != Value::Int(0) && c != Value::Int(0) { 1 } else { 0 }),
                    BinaryOp::Or => Value::Int(if a != Value::Int(0) || c != Value::Int(0) { 1 } else { 0 }),
                }
            }
        }
    }
}

impl Display for BinaryExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnaryExp(a) => write!(f, "{}", a),
            Self::OpExp((a, b, c)) => write!(f, "({}) {} ({})", a, b, c),
        }
    }
}