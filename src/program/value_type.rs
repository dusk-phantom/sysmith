use super::*;

/// All possible types
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    Void,
    Func(Box<Type>, Vec<Type>),
    Array(Box<Type>, i32),
    Pointer(Box<Type>),
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Int can be implicitly converted to and from float
            (Type::Int | Type::Float, Type::Int | Type::Float) => true,
            (Type::Void, Type::Void) => true,
            (Type::Func(a, b), Type::Func(c, d)) => a == c && b == d,
            (Type::Array(a, b), Type::Array(c, d)) => a == c && b == d,
            (Type::Pointer(a), Type::Pointer(b)) => a == b,
            // Array can be implicitly converted to and from pointer
            (Type::Array(a, _), Type::Pointer(b)) => a == b,
            (Type::Pointer(a), Type::Array(b, _)) => a == b,
            _ => false,
        }
    }
}

impl From<BType> for Type {
    fn from(btype: BType) -> Self {
        match btype {
            BType::Int => Type::Int,
            BType::Float => Type::Float,
        }
    }
}

impl From<FuncType> for Type {
    fn from(func_type: FuncType) -> Self {
        match func_type {
            FuncType::Int => Type::Int,
            FuncType::Float => Type::Float,
            FuncType::Void => Type::Void,
        }
    }
}

/// Basic types, core of each variable declaration
#[derive(Debug, Clone)]
pub enum BType {
    Int,
    Float,
}

impl<'a> Arbitrary<'a> for BType {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        // Generate a random basic type
        match u.int_in_range(0..=1)? {
            0 => Ok(BType::Int),
            1 => Ok(BType::Float),
            _ => unreachable!(),
        }
    }
}

impl Display for BType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BType::Int => write!(f, "int"),
            BType::Float => write!(f, "float"),
        }
    }
}

/// Function types, used in function declarations
#[derive(Debug, Clone)]
pub enum FuncType {
    Void,
    Int,
    Float,
}

impl<'a> Arbitrary<'a> for FuncType {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        match u.int_in_range(0..=2)? {
            0 => Ok(FuncType::Void),
            1 => Ok(FuncType::Int),
            2 => Ok(FuncType::Float),
            _ => unreachable!(),
        }
    }
}

impl Display for FuncType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncType::Void => write!(f, "void"),
            FuncType::Int => write!(f, "int"),
            FuncType::Float => write!(f, "float"),
        }
    }
}
