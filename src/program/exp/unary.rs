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

/// UnaryOpContext generates an arbitrary expression
/// which is a unary operation.
/// Example: `-4`
#[derive(Clone, Debug)]
pub struct UnaryOpContext<'a>(pub &'a Context);

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
