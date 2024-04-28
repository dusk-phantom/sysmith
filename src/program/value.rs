use super::*;

/// All possible values
#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Array(Vec<Value>),
}

impl Value {
    pub fn as_int(&self) -> i32 {
        match self {
            Value::Int(a) => *a,
            Value::Float(a) => *a as i32,
            _ => panic!("Value is not a number: {:?}", self),
        }
    }
}

impl std::ops::Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Int(a) => Value::Int(-a),
            Value::Float(a) => Value::Float(-a),
            Value::Array(_) => panic!("Cannot negate an array"),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f32),
            _ => panic!("Cannot add an array"),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_sub(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 - b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a - b as f32),
            _ => panic!("Cannot subtract an array"),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_mul(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 * b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f32),
            _ => panic!("Cannot multiply an array"),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 / b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a / b as f32),
            _ => panic!("Cannot divide an array"),
        }
    }
}

impl std::ops::Rem for Value {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 % b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a % b as f32),
            _ => panic!("Cannot modulo an array"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Int(a), Value::Float(b)) => *a as f32 == *b,
            (Value::Float(a), Value::Int(b)) => *a == *b as f32,
            _ => panic!("Cannot compare an array"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
            (Value::Int(a), Value::Float(b)) => (*a as f32).partial_cmp(b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(*b as f32)),
            _ => panic!("Cannot compare an array"),
        }
    }
}
