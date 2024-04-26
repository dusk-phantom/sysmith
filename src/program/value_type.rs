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

impl BType {
    pub fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        // Generate a random basic type
        match u.arbitrary::<u8>()? % 2 {
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

impl FuncType {
    pub fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
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