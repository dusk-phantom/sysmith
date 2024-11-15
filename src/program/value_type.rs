// Copyright 2024 Duskphantom Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// SPDX-License-Identifier: Apache-2.0

use super::*;

/// Expected type
#[derive(Debug, Clone)]
pub struct ExpectedType {
    pub is_const: bool,
    pub value_type: Type,
    pub bound: NumBound,
}

/// Bound on integer type
#[derive(Debug, Clone)]
pub enum NumBound {
    None,
    NonZero,
    Range(i32, i32),
}

impl NumBound {
    /// [min, max]
    pub fn new(min: i32, max: i32) -> Self {
        NumBound::Range(min, max)
    }

    /// Wrap a given expression into range
    pub fn wrap(&self, exp: Exp, c: &Context) -> Exp {
        // If not expected constant, return the expression as is
        if !c.expected.is_const {
            return exp;
        }

        // Apply wrapping for constant expressions
        match self {
            NumBound::None => exp,
            NumBound::NonZero => match exp.eval(c) {
                Value::Int(a) => {
                    let delta = if a == 0 { 1 } else { 0 };
                    Exp::OpExp((
                        Box::new(exp),
                        BinaryOp::Add,
                        Box::new(Exp::Number(Number::IntConst(delta))),
                    ))
                }
                Value::Float(a) => {
                    let delta = if a == 0.0 { 1 } else { 0 };
                    Exp::OpExp((
                        Box::new(exp),
                        BinaryOp::Add,
                        Box::new(Exp::Number(Number::IntConst(delta))),
                    ))
                }
                _ => panic!(
                    "Expected a constant numeric expression, got: {:?}",
                    exp.eval(c)
                ),
            },
            NumBound::Range(min, max) => {
                match exp.eval(c) {
                    Value::Int(a) => {
                        let delta = a
                            .rem_euclid(max - min + 1)
                            .wrapping_add(*min)
                            .wrapping_sub(a);
                        Exp::OpExp((
                            Box::new(exp),
                            BinaryOp::Add,
                            Box::new(Exp::Number(Number::IntConst(delta))),
                        ))
                    }
                    Value::Float(a) => {
                        // Float const as integer will cause numeric problems,
                        // so we just replace it with an integer constant
                        Exp::Number(Number::IntConst(
                            (a as i32).rem_euclid(max - min + 1).wrapping_add(*min),
                        ))
                    }
                    _ => panic!(
                        "Expected a constant numeric expression, got: {:?}",
                        exp.eval(c)
                    ),
                }
            }
        }
    }
}

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

    /// View as an array (for pointer and array)
    pub fn as_array(&self) -> Option<(&Type, &i32)> {
        match self {
            Type::Pointer(t) => Some((t, &MAX_ARR_LEN)),
            Type::Array(t, n) => Some((t, n)),
            _ => None,
        }
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
