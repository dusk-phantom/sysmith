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

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Exp,
    pub body: Stmt,
}

#[derive(Debug, Clone)]
pub struct WhileContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for WhileContext<'_> {
    fn can_arbitrary(&self, _: std::marker::PhantomData<Stmt>) -> bool {
        self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        // Context for condition expects int type
        let mut c = self.0.clone();
        c.expected = ExpectedType {
            is_const: false,
            value_type: Type::Int,
            bound: NumBound::None,
        };
        let cond = c.arbitrary(u)?;

        // Context for body is in loop
        c.in_loop = true;
        let body = c.arbitrary(u)?;
        Ok(Stmt::While(Box::new(While { cond, body })))
    }
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {}", self.cond, self.body)
    }
}
