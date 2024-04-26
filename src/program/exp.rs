use crate::random_retry;

use super::*;

#[derive(Debug, Clone)]
pub enum Exp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(Number),
    FuncCall((Ident, Option<FuncRParams>)),
    OpUnary((UnaryOp, Box<Exp>)),
    OpExp((Box<Exp>, BinaryOp, Box<Exp>)),
}

impl<'a> ArbitraryIn<'a, Context> for Exp {
    /// Safety: if expected type is not numeric, it can possibly fail.
    /// The failure should be handled manually.
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        // Increase depth of context, converge if depth exceeds
        let mut c = c.clone();
        c.depth += 1;
        if c.depth > MAX_DEPTH {
            if !c.expected_type.is_numeric() {
                return Err(arbitrary::Error::EmptyChoose);
            }
            return Ok(Exp::Number(Number::arbitrary(u, &c)?));
        }

        // Select a random expression type.
        // Only function argument will demand non-numeric type,
        // so this function guarantees to succeed
        random_retry! { size = 5, bytes = u;
            0 => {
                // Ensure not require constant for variable reference
                // TODO filter constant (requires eval to return Result)
                if c.expected_const {
                    return Err(arbitrary::Error::EmptyChoose);
                }

                // Get all candidates from environment
                let mut candidates: Vec<Self> = c.ctx.iter().filter_map(|(id, ty)| {
                    let id = Ident::from(id.clone());
        
                    // If type match, directly add as candidate
                    if *ty == c.expected_type {
                        return Some(Ok(Exp::LVal(LVal {
                            id,
                            index: Index(Vec::new()),
                        })));
                    }
        
                    // If type is function, check if return type matches
                    // If matches, add as candidate
                    if let Type::Func(ret_type, param) = ty {
                        if **ret_type == c.expected_type {
                            // Map parameters to arbitrary instances of its type
                            match param.iter().map(|x| {
                                let mut c = c.clone();
                                c.expected_type = x.clone();
                                c.expected_const = false;
                                Exp::arbitrary(u, &c)
                            }).collect::<Result<_, _>>() {
                                Ok(exp_vec) => return Some(Ok(Exp::FuncCall((id, 
                                    Some(FuncRParams { exp_vec })
                                )))),
                                Err(err) => return Some(Err(err)),
                            }
                        }
                    }
        
                    // If type is array, recursively check if content type matches
                    // until content type is no longer an array
                    let mut current_type = ty;
                    let mut current_exp = LVal {
                        id: id.clone(),
                        index: Index(Vec::new()),
                    };
                    while let Type::Array(content_type, len) = current_type {
                        // Generate a random index in bound
                        // TODO refer to constant here
                        let index = match u.arbitrary::<i32>() {
                            Ok(i) => i,
                            Err(err) => return Some(Err(err)),
                        };
    
                        // Update current processing type and expression
                        current_type = content_type;
                        current_exp.index.0.push(Exp::Number(Number::IntConst(index % len)));
                        if *current_type == c.expected_type {
                            return Some(Ok(Exp::LVal(current_exp)));
                        }
                    }

                    // Otherwise, it's impossible for this identifier to transform
                    // to the expected type
                    None
                }).collect::<Result<_, _>>()?;

                // If there's no candidate, report unable to construct
                if candidates.is_empty() {
                    return Err(arbitrary::Error::EmptyChoose);
                }
                
                // Randomly choose a candidate
                let selected_index = u.arbitrary::<usize>()? % candidates.len();
                Ok(candidates.swap_remove(selected_index))
            },
            1 => Ok(Exp::Exp(Box::new(Exp::arbitrary(u, &c)?))),
            2 => {
                // Expected type must be numeric to generate unary expression
                if !c.expected_type.is_numeric() {
                    return Err(arbitrary::Error::EmptyChoose);
                }
                let op = UnaryOp::arbitrary(u)?;
                let exp = Box::new(Exp::arbitrary(u, &c)?);
                Ok(Exp::OpUnary((op, exp)))
            },
            3 => {
                // Expected type must be numeric to generate binary expression
                if !c.expected_type.is_numeric() {
                    return Err(arbitrary::Error::EmptyChoose);
                }
                let a = Box::new(Exp::arbitrary(u, &c)?);
                let b = BinaryOp::arbitrary(u)?;
                let c = Box::new(Exp::arbitrary(u, &c)?);
                Ok(Exp::OpExp((a, b, c)))
            },
            4 => {
                // Expected type must be numeric to generate number
                // Safety: this can generate a constant number
                if !c.expected_type.is_numeric() {
                    return Err(arbitrary::Error::EmptyChoose);
                }
                Ok(Exp::Number(Number::arbitrary(u, &c)?))
            },
        }
    }
}

impl Eval for Exp {
    fn eval(&self, ctx: &Context) -> Value {
        match self {
            Self::Exp(a) => a.eval(ctx),
            Self::LVal(a) => a.eval(ctx),
            Self::Number(a) => a.eval(ctx),
            Self::FuncCall((_, _)) => panic!("No function is constant"),
            Self::OpUnary((op, exp)) => {
                let exp = exp.eval(ctx);
                match op {
                    UnaryOp::Add => exp,
                    UnaryOp::Minus => -exp,
                    UnaryOp::Exclamation => {
                        if exp == Value::Int(0) {
                            Value::Int(1)
                        } else {
                            Value::Int(0)
                        }
                    }
                }
            }
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
                    BinaryOp::And => Value::Int(if a != Value::Int(0) && c != Value::Int(0) {
                        1
                    } else {
                        0
                    }),
                    BinaryOp::Or => Value::Int(if a != Value::Int(0) || c != Value::Int(0) {
                        1
                    } else {
                        0
                    }),
                }
            }
        }
    }
}

impl Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exp(a) => write!(f, "({})", a),
            Self::LVal(a) => write!(f, "{}", a),
            Self::Number(a) => write!(f, "{}", a),
            Self::FuncCall((id, None)) => write!(f, "{}()", id),
            Self::FuncCall((id, Some(param))) => write!(f, "{}({})", id, param),
            Self::OpUnary((a, b)) => write!(f, "{}({})", a, b),
            Self::OpExp((a, b, c)) => write!(f, "({}) {} ({})", a, b, c),
        }
    }
}


#[derive(Debug, Clone)]
pub struct LVal {
    pub id: Ident,
    pub index: Index,
}

impl Eval for LVal {
    fn eval(&self, ctx: &Context) -> Value {
        let mut array_type = ctx.ctx.get(&self.id.to_string()).unwrap().clone();
        let mut array_value = ctx.env.get(&self.id.to_string()).unwrap().clone();
        for exp in &self.index.0 {
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
            self.index,
        )
    }
}

#[derive(Debug, Clone)]
pub enum Number {
    IntConst(IntConst),
    FloatConst(FloatConst),
}

impl<'a> ArbitraryIn<'a, Context> for Number {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        match c.expected_type {
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

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Add,
    Minus,
    Exclamation,
}

impl<'a> Arbitrary<'a> for UnaryOp {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        match u.int_in_range(0..=2)? {
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