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
pub struct Return {
    pub exp: Option<Exp>,
}

#[derive(Debug, Clone)]
pub struct ReturnContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for ReturnContext<'_> {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        let exp = match self.0.return_type {
            Type::Void => None,
            Type::Float | Type::Int => {
                // Context for returned expression expects return type
                let mut c = self.0.clone();
                c.expected = ExpectedType {
                    is_const: false,
                    value_type: c.return_type.clone(),
                    bound: NumBound::None,
                };
                Some(c.arbitrary(u)?)
            }
            _ => panic!("Invalid return type"),
        };
        Ok(Stmt::Return(Return { exp }))
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.exp {
            Some(a) => write!(f, "return {};", a),
            None => write!(f, "return;"),
        }
    }
}
