use std::marker::PhantomData;

use super::*;

pub mod binary;
pub mod nested;
pub mod number;
pub mod unary;
pub mod var;

pub use binary::*;
pub use nested::*;
pub use number::*;
pub use unary::*;
pub use var::*;

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
