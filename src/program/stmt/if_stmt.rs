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
pub struct If {
    pub cond: Exp,
    pub then: Stmt,
    pub else_then: Option<Stmt>,
}

#[derive(Debug, Clone)]
pub struct IfContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for IfContext<'_> {
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

        // Context for then and else is in loop
        c.in_loop = true;
        let then = c.arbitrary(u)?;
        let else_then = if u.arbitrary()? {
            Some(c.arbitrary(u)?)
        } else {
            None
        };
        Ok(Stmt::If(Box::new(If {
            cond,
            then,
            else_then,
        })))
    }
}

impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.else_then {
            Some(e) => write!(f, "if ({}) {} else {}", self.cond, self.then, e),
            None => write!(f, "if ({}) {}", self.cond, self.then),
        }
    }
}
