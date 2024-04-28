use std::marker::PhantomData;

use super::*;

/// SingleVarContext generates an arbitrary expression
/// from a given root variable in the context.
/// Example: `f(1)` when id is `f`.
#[derive(Clone, Debug)]
pub struct SingleVarContext<'a> {
    ctx: &'a Context,
    id: Ident,
    ty: Type,
}

impl<'a> ArbitraryTo<'a, Exp> for SingleVarContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        // If expected constant, make sure variable is constant
        if self.ctx.expected.is_const && !self.ctx.env.contains_key(&self.id.to_string()) {
            return false;
        }

        // If type match, using this variable is OK
        if self.ty == self.ctx.expected.value_type {
            return true;
        }

        // If type is function, check if return type matches
        // If matches, check if argument can be filled
        if let Type::Func(ret_type, param) = &self.ty {
            if **ret_type == self.ctx.expected.value_type {
                let mut can_arbitrary = true;
                for x in param.iter() {
                    let mut c = self.ctx.clone();
                    c.expected = ExpectedType {
                        is_const: false,
                        value_type: x.clone(),
                        bound: IntBound::None,
                    };
                    if !c.can_arbitrary(PhantomData::<Exp>) {
                        can_arbitrary = false;
                        break;
                    }
                }
                if can_arbitrary {
                    return true;
                }
            }
        }

        // If type is array, recursively check if content type matches
        // until content type is no longer an array
        let mut current_type = &self.ty;
        while let Type::Array(content_type, _) = current_type {
            current_type = content_type;
            if *current_type == self.ctx.expected.value_type {
                return true;
            }
        }

        // No variable found in context
        false
    }

    fn arbitrary(&self, u: &mut Unstructured) -> Result<Exp> {
        let c: &Context = self.ctx;
        let id = &self.id;
        let ty = &self.ty;
        let id = id.clone();

        // If type match, directly add as candidate
        if *ty == c.expected.value_type {
            return Ok(Exp::LVal(LVal {
                id,
                index: Index(Vec::new()),
            }));
        }

        // If type is function, check if return type matches
        // If matches, add as candidate
        if let Type::Func(ret_type, param) = ty {
            if **ret_type == c.expected.value_type {
                // Map parameters to arbitrary instances of its type
                return param
                    .iter()
                    .map(|x| {
                        let mut c = c.clone();
                        c.expected = ExpectedType {
                            is_const: false,
                            value_type: x.clone(),
                            bound: IntBound::None,
                        };
                        c.arbitrary(u)
                    })
                    .collect::<Result<_, _>>()
                    .map(|exp_vec| {
                        Exp::FuncCall((id, FuncRParams { exp_vec }))
                    })
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
            let mut c = self.ctx.clone();
            c.expected = ExpectedType {
                is_const: true,
                value_type: Type::Int,
                bound: IntBound::new(0, len - 1),
            };
            let exp = c.arbitrary(u)?;

            // Update current processing type and expression
            current_type = content_type;
            current_exp
                .index
                .0
                .push(exp);
            if *current_type == c.expected.value_type {
                return Ok(Exp::LVal(current_exp));
            }
        }

        // Otherwise, it's impossible for this identifier to transform
        // to the expected type
        panic!("impossible to construct variable; you should check can_arbitrary first");
    }
}

/// VarContext generates an arbitrary expression
/// from an arbitrary root variable in the context.
/// Example: `f(1)` when `f` is in context
#[derive(Clone, Debug)]
pub struct VarContext<'a>(&'a Context);

impl<'a> ArbitraryTo<'a, Exp> for VarContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        let contexts: Vec<_> = self.0.ctx.iter().map(|(id, ty)| {
            Box::new(SingleVarContext {
                ctx: self.0,
                id: id.clone().into(),
                ty: ty.clone(),
            }) as Box<dyn ArbitraryTo<Exp>>
        }).collect();
        can_arbitrary_any(contexts.as_slice())
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        let contexts: Vec<_> = self.0.ctx.iter().map(|(id, ty)| {
            Box::new(SingleVarContext {
                ctx: self.0,
                id: id.clone().into(),
                ty: ty.clone(),
            }) as Box<dyn ArbitraryTo<Exp>>
        }).collect();
        arbitrary_any(u, contexts.as_slice())
    }
}

/// NestedContext generates an arbitrary expression
/// which is in a pair of parenthesis.
/// Example: `(4)`
#[derive(Clone, Debug)]
pub struct NestedContext<'a>(&'a Context);

impl<'a> ArbitraryTo<'a, Exp> for NestedContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        Ok(Exp::Exp(Box::new(self.0.arbitrary(u)?)))
    }
}

/// BinaryOpContext generates an arbitrary expression
/// which is a binary operation.
/// Example: `4 + 5`
#[derive(Clone, Debug)]
pub struct BinaryOpContext<'a>(&'a Context);

impl<'a> ArbitraryTo<'a, Exp> for BinaryOpContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        self.0.expected.value_type.is_numeric() && self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        let a = Box::new(self.0.arbitrary(u)?);
        let b = BinaryOp::arbitrary(u)?;
        let c = Box::new(self.0.arbitrary(u)?);
        Ok(Exp::OpExp((a, b, c)))
    }
}

/// UnaryOpContext generates an arbitrary expression
/// which is a unary operation.
/// Example: `-4`
#[derive(Clone, Debug)]
pub struct UnaryOpContext<'a>(&'a Context);

impl<'a> ArbitraryTo<'a, Exp> for UnaryOpContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        self.0.expected.value_type.is_numeric() && self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        let op = UnaryOp::arbitrary(u)?;
        let exp = Box::new(self.0.arbitrary(u)?);
        Ok(Exp::OpUnary((op, exp)))
    }
}

/// NumberContext generates an arbitrary expression
/// which is a number literal.
/// Example: `4`
#[derive(Clone, Debug)]
pub struct NumberContext<'a>(&'a Context);

impl<'a> ArbitraryTo<'a, Exp> for NumberContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        self.0.expected.value_type.is_numeric()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        Ok(Exp::Number(self.0.arbitrary(u)?))
    }
}

#[derive(Debug, Clone)]
pub enum Exp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(Number),
    FuncCall((Ident, FuncRParams)),
    OpUnary((UnaryOp, Box<Exp>)),
    OpExp((Box<Exp>, BinaryOp, Box<Exp>)),
}

impl<'a> ArbitraryTo<'a, Exp> for Context {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        // Increase context depth
        let c = self.next();

        // All possible choices
        let contexts = [
            Box::new(NumberContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
            Box::new(UnaryOpContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
            Box::new(BinaryOpContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
            Box::new(VarContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
            Box::new(NestedContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
        ];
        can_arbitrary_any(&contexts)
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        // Increase context depth
        let c = self.next();

        // All possible choices
        let contexts = [
            Box::new(NumberContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
            Box::new(UnaryOpContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
            Box::new(BinaryOpContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
            Box::new(VarContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
            Box::new(NestedContext(&c)) as Box<dyn ArbitraryTo<Exp>>,
        ];
        let result = arbitrary_any(u, &contexts)?;

        // Wrap generated expression to range
        let result = self.expected.bound.wrap(result, &c);

        // Otherwise directly return result
        Ok(result)
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
            Self::FuncCall((id, param)) => write!(f, "{}({})", id, param),
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
        write!(f, "{}{}", self.id, self.index,)
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
