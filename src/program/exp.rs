use super::*;

#[derive(Debug, Clone)]
pub struct Exp {
    pub add_exp: Box<AddExp>,
}

impl Exp {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        let add_exp = AddExp::arbitrary(u, c)?;
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
pub struct Cond {
    pub l_or_exp: LOrExp,
}

impl Cond {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        let l_or_exp = LOrExp::arbitrary(u, c)?;
        Ok(Cond { l_or_exp })
    }
}

impl Display for Cond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.l_or_exp)
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
pub enum MulExp {
    UnaryExp(Box<UnaryExp>),
    MulExp((Box<MulExp>, UnaryExp)),
    DivExp((Box<MulExp>, UnaryExp)),
    ModExp((Box<MulExp>, UnaryExp)),
}

impl MulExp {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 4 {
            0 => Ok(MulExp::UnaryExp(Box::new(UnaryExp::arbitrary(u, c)?))),
            1 => {
                let a = MulExp::arbitrary(u, c)?;
                let b = UnaryExp::arbitrary(u, c)?;
                Ok(MulExp::MulExp((Box::new(a), b)))
            }
            2 => {
                let a = MulExp::arbitrary(u, c)?;
                let b = UnaryExp::arbitrary(u, c)?;
                Ok(MulExp::DivExp((Box::new(a), b)))
            }
            3 => {
                let a = MulExp::arbitrary(u, c)?;
                let b = UnaryExp::arbitrary(u, c)?;
                Ok(MulExp::ModExp((Box::new(a), b)))
            }
            _ => unreachable!(),
        }
    }

    pub fn eval(&self, ctx: &Context) -> Value {
        match self {
            Self::MulExp((a, b)) => {
                let a = a.eval(ctx);
                let b = b.eval(ctx);
                a * b
            }
            Self::DivExp((a, b)) => {
                let a = a.eval(ctx);
                let b = b.eval(ctx);
                a / b
            }
            Self::ModExp((a, b)) => {
                let a = a.eval(ctx);
                let b = b.eval(ctx);
                a % b
            }
            Self::UnaryExp(a) => a.eval(ctx),
        }
    }
}

impl Display for MulExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnaryExp(a) => write!(f, "{}", a),
            Self::MulExp((a, b)) => write!(f, "({}) * ({})", a, b),
            Self::DivExp((a, b)) => write!(f, "({}) / ({})", a, b),
            Self::ModExp((a, b)) => write!(f, "({}) % ({})", a, b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AddOp {
    Add,
    Minus,
}

impl AddOp {
    pub fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(AddOp::Add),
            1 => Ok(AddOp::Minus),
            _ => unreachable!(),
        }
    }
}

impl Display for AddOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddOp::Add => write!(f, "+"),
            AddOp::Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AddExp {
    MulExp(Box<MulExp>),
    OpExp((Box<AddExp>, AddOp, MulExp)),
}

impl AddExp {
    pub fn arbitrary(u: &mut Unstructured, ctx: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(AddExp::MulExp(Box::new(MulExp::arbitrary(u, ctx)?))),
            1 => {
                let a = AddExp::arbitrary(u, ctx)?;
                let b = AddOp::arbitrary(u)?;
                let c = MulExp::arbitrary(u, ctx)?;
                Ok(AddExp::OpExp((Box::new(a), b, c)))
            }
            _ => unreachable!(),
        }
    }

    pub fn eval(&self, ctx: &Context) -> Value {
        match self {
            Self::MulExp(a) => a.eval(ctx),
            Self::OpExp((a, b, c)) => {
                let a = a.eval(ctx);
                let c = c.eval(ctx);
                match b {
                    AddOp::Add => a + c,
                    AddOp::Minus => a - c,
                }
            }
        }
    }
}

impl Display for AddExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MulExp(a) => write!(f, "{}", a),
            Self::OpExp((a, b, c)) => write!(f, "({}) {} ({})", a, b, c),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RelOp {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}

impl RelOp {
    pub fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        match u.arbitrary::<u8>()? % 4 {
            0 => Ok(RelOp::Less),
            1 => Ok(RelOp::LessOrEqual),
            2 => Ok(RelOp::Greater),
            3 => Ok(RelOp::GreaterOrEqual),
            _ => unreachable!(),
        }
    }
}

impl Display for RelOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RelOp::Less => write!(f, "<"),
            RelOp::LessOrEqual => write!(f, "<="),
            RelOp::Greater => write!(f, ">"),
            RelOp::GreaterOrEqual => write!(f, ">="),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RelExp {
    AddExp(AddExp),
    OpExp((Box<RelExp>, RelOp, AddExp)),
}

impl RelExp {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(RelExp::AddExp(AddExp::arbitrary(u, c)?)),
            1 => {
                let a = RelExp::arbitrary(u, c)?;
                let b = RelOp::arbitrary(u)?;
                let c = AddExp::arbitrary(u, c)?;
                Ok(RelExp::OpExp((Box::new(a), b, c)))
            }
            _ => unreachable!(),
        }
    }
}

impl Display for RelExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AddExp(a) => write!(f, "{}", a),
            Self::OpExp((a, b, c)) => write!(f, "({}) {} ({})", a, b, c),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EqExp {
    RelExp(RelExp),
    EqualExp((Box<EqExp>, RelExp)),
    NotEqualExp((Box<EqExp>, RelExp)),
}

impl EqExp {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
            0 => Ok(EqExp::RelExp(RelExp::arbitrary(u, c)?)),
            1 => {
                let a = EqExp::arbitrary(u, c)?;
                let b = RelExp::arbitrary(u, c)?;
                Ok(EqExp::EqualExp((Box::new(a), b)))
            }
            2 => {
                let a = EqExp::arbitrary(u, c)?;
                let b = RelExp::arbitrary(u, c)?;
                Ok(EqExp::NotEqualExp((Box::new(a), b)))
            }
            _ => unreachable!(),
        }
    }
}

impl Display for EqExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RelExp(a) => write!(f, "{}", a),
            Self::EqualExp((a, b)) => write!(f, "({}) == ({})", a, b),
            Self::NotEqualExp((a, b)) => write!(f, "({}) != ({})", a, b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LAndExp {
    EqExp(EqExp),
    AndExp((Box<LAndExp>, EqExp)),
}

impl LAndExp {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(LAndExp::EqExp(EqExp::arbitrary(u, c)?)),
            1 => {
                let a = LAndExp::arbitrary(u, c)?;
                let b = EqExp::arbitrary(u, c)?;
                Ok(LAndExp::AndExp((Box::new(a), b)))
            }
            _ => unreachable!(),
        }
    }
}

impl Display for LAndExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EqExp(a) => write!(f, "{}", a),
            Self::AndExp((a, b)) => write!(f, "({}) && ({})", a, b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LOrExp {
    LAndExp(LAndExp),
    OrExp((Box<LOrExp>, LAndExp)),
}

impl LOrExp {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(LOrExp::LAndExp(LAndExp::arbitrary(u, c)?)),
            1 => {
                let a = LOrExp::arbitrary(u, c)?;
                let b = LAndExp::arbitrary(u, c)?;
                Ok(LOrExp::OrExp((Box::new(a), b)))
            }
            _ => unreachable!(),
        }
    }
}

impl Display for LOrExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LAndExp(a) => write!(f, "{}", a),
            Self::OrExp((a, b)) => write!(f, "({}) || ({})", a, b),
        }
    }
}