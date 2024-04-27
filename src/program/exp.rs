use std::marker::PhantomData;

use super::*;

/// Convert a variable in context to an expression
fn reify_variable(u: &mut Unstructured, c: &Context, id: &Ident, ty: &Type) -> Option<Result<Exp>> {
    let id = id.clone();

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
            match param
                .iter()
                .map(|x| {
                    let mut c = c.clone();
                    c.expected_type = x.clone();
                    c.expected_const = false;
                    Exp::arbitrary(u, &c)
                })
                .collect::<Result<_, _>>()
            {
                Ok(exp_vec) => return Some(Ok(Exp::FuncCall((id, Some(FuncRParams { exp_vec }))))),
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
        current_exp
            .index
            .0
            .push(Exp::Number(Number::IntConst(index % len)));
        if *current_type == c.expected_type {
            return Some(Ok(Exp::LVal(current_exp)));
        }
    }

    // Otherwise, it's impossible for this identifier to transform
    // to the expected type
    None
}

#[derive(Debug, Clone)]
pub struct Var(Exp);

impl From<Var> for Exp {
    fn from(value: Var) -> Self {
        let Var(e) = value;
        e
    }
}

impl<'a> ArbitraryTo<'a, Var> for Context {
    fn can_arbitrary(&self) -> bool {
        for ty in self.ctx.values() {
            // If type match, using this variable is OK
            if *ty == self.expected_type {
                return true;
            }

            // If type is function, check if return type matches
            // If matches, check if argument can be filled
            if let Type::Func(ret_type, param) = ty {
                if **ret_type == self.expected_type {
                    let mut can_arbitrary = true;
                    for x in param.iter() {
                        let mut c = self.clone();
                        c.expected_type = x.clone();
                        c.expected_const = false;
                        if !self.can_arbitrary() {
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
            let mut current_type = ty;
            while let Type::Array(content_type, _) = current_type {
                current_type = content_type;
                if *current_type == self.expected_type {
                    return true;
                }
            }
        }

        // No variable found in context
        false
    }

    fn arbitrary(&self, u: &mut Unstructured) -> Result<Var> {
        // Ensure not require constant for variable reference
        // TODO filter constant (requires eval to return Result)
        if self.expected_const {
            return Err(arbitrary::Error::EmptyChoose);
        }

        // Filter possible candidates from context
        let mut candidates: Vec<_> = self
            .ctx
            .iter()
            .filter_map(|(id, ty)| {
                let id = Ident::from(id.clone());

                // If type match, using this variable is OK
                if *ty == self.expected_type {
                    return Some((id, ty));
                }

                // If type is function, check if return type matches
                // If matches, check if argument can be filled
                if let Type::Func(ret_type, param) = ty {
                    if **ret_type == self.expected_type {
                        let mut can_arbitrary = true;
                        for x in param.iter() {
                            let mut c = self.clone();
                            c.expected_type = x.clone();
                            c.expected_const = false;
                            if !c.can_arbitrary(PhantomData::<Exp>) {
                                can_arbitrary = false;
                                break;
                            }
                        }
                        if can_arbitrary {
                            return Some((id, ty));
                        }
                    }
                }

                // If type is array, recursively check if content type matches
                // until content type is no longer an array
                let mut current_type = ty;
                while let Type::Array(content_type, _) = current_type {
                    current_type = content_type;
                    if *current_type == self.expected_type {
                        return Some((id, ty));
                    }
                }

                // Impossible to convert this variable to correct type
                None
            })
            .collect();

        // If there's no candidate, report unable to construct
        if candidates.is_empty() {
            panic!("impossible to construct variable");
        }

        // Randomly choose a candidate
        let selected_index = u.arbitrary::<usize>()? % candidates.len();
        let (id, ty) = candidates.swap_remove(selected_index);
        Ok(Var(reify_variable(u, self, &id, &ty).unwrap()?))
    }
}

#[derive(Debug, Clone)]
pub enum Exp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(Number),
    FuncCall((Ident, Option<FuncRParams>)),
    OpUnary((UnaryOp, Box<Exp>)),
    OpExp((Box<Exp>, BinaryOp, Box<Exp>)),
}

impl<'a> ArbitraryTo<'a, Exp> for Context {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        // Number can be generated trivially
        if matches!(self.expected_type, Type::Float | Type::Int) {
            return true;
        }
        return false;
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        /// Generate a random variable from context
        fn gen_var(u: &mut Unstructured, c: &Context) -> Result<Exp> {
            // Ensure not require constant for variable reference
            // TODO filter constant (requires eval to return Result)
            if c.expected_const {
                return Err(arbitrary::Error::EmptyChoose);
            }

            // Get all candidates from environment
            let mut candidates: Vec<Exp> = c
                .ctx
                .iter()
                .filter_map(|(id, ty)| {
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
                            match param
                                .iter()
                                .map(|x| {
                                    let mut c = c.clone();
                                    c.expected_type = x.clone();
                                    c.expected_const = false;
                                    Exp::arbitrary(u, &c)
                                })
                                .collect::<Result<_, _>>()
                            {
                                Ok(exp_vec) => {
                                    return Some(Ok(Exp::FuncCall((
                                        id,
                                        Some(FuncRParams { exp_vec }),
                                    ))))
                                }
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
                        current_exp
                            .index
                            .0
                            .push(Exp::Number(Number::IntConst(index % len)));
                        if *current_type == c.expected_type {
                            return Some(Ok(Exp::LVal(current_exp)));
                        }
                    }

                    // Otherwise, it's impossible for this identifier to transform
                    // to the expected type
                    None
                })
                .collect::<Result<_, _>>()?;

            // If there's no candidate, report unable to construct
            if candidates.is_empty() {
                return Err(arbitrary::Error::EmptyChoose);
            }

            // Randomly choose a candidate
            let selected_index = u.arbitrary::<usize>()? % candidates.len();
            Ok(candidates.swap_remove(selected_index))
        }

        fn gen_nested(u: &mut Unstructured, c: &Context) -> Result<Exp> {
            Ok(Exp::Exp(Box::new(Exp::arbitrary(u, &c)?)))
        }

        fn gen_unary(u: &mut Unstructured, c: &Context) -> Result<Exp> {
            // Expected type must be numeric to generate unary expression
            if !c.expected_type.is_numeric() {
                return Err(arbitrary::Error::EmptyChoose);
            }
            let op = UnaryOp::arbitrary(u)?;
            let exp = Box::new(Exp::arbitrary(u, &c)?);
            Ok(Exp::OpUnary((op, exp)))
        }

        fn gen_binary(u: &mut Unstructured, c: &Context) -> Result<Exp> {
            // Expected type must be numeric to generate binary expression
            if !c.expected_type.is_numeric() {
                return Err(arbitrary::Error::EmptyChoose);
            }
            let a = Box::new(Exp::arbitrary(u, &c)?);
            let b = BinaryOp::arbitrary(u)?;
            let c = Box::new(Exp::arbitrary(u, &c)?);
            Ok(Exp::OpExp((a, b, c)))
        }

        fn gen_number(u: &mut Unstructured, c: &Context) -> Result<Exp> {
            // Expected type must be numeric to generate number
            if !c.expected_type.is_numeric() {
                return Err(arbitrary::Error::EmptyChoose);
            }
            Ok(Exp::Number(Number::arbitrary(u, &c)?))
        }

        // Increase depth of context, converge if depth exceeds
        let mut c = c.clone();
        c.depth += 1;

        // Depth exceeds
        if c.depth > MAX_DEPTH {
            if !c.expected_type.is_numeric() {
                return Err(arbitrary::Error::EmptyChoose);
            }
            return Ok(Exp::Number(Number::arbitrary(u, &c)?));
        }

        // Select a random expression type.
        // Only function argument will demand non-numeric type,
        // so this function guarantees to succeed
        match u.int_in_range(0..=4)? {
            0 => gen_var(u, &c),
            1 => gen_nested(u, &c),
            2 => gen_unary(u, &c),
            3 => gen_binary(u, &c),
            4 => gen_number(u, &c),
            _ => unreachable!(),
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
