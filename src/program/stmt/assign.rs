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
struct SingleAssignContext<'a> {
    ctx: &'a Context,
    id: Ident,
    ty: Type,
}

impl<'a> ArbitraryTo<'a, Stmt> for SingleAssignContext<'_> {
    fn can_arbitrary(&self, _: std::marker::PhantomData<Stmt>) -> bool {
        let mut current_type = self.ty.clone();

        // Collapse array type,
        // did not allow assigning an array to another
        while let Type::Array(t, len) = current_type {
            // If array length is 0, it contains nothing
            if len == 0 {
                return false;
            }

            // Otherwise continue collapsing type,
            // because midway assignment `int x[3][3]; x[1] = y;` is not allowed
            current_type = *t;
        }

        // Only accept int or float type as left value,
        // void or function will be rejected
        matches!(current_type, Type::Int | Type::Float)
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        let mut ty = self.ty.clone();
        let mut lval = LVal {
            id: self.id.clone(),
            index: Index(Vec::new()),
        };

        // Collapse array type,
        // did not allow assigning an array to another
        while let Type::Array(t, len) = ty {
            // Generate a random index in bound,
            // although the bound is not checked because non-constant
            let mut c = self.ctx.clone();
            c.expected = ExpectedType {
                is_const: false,
                value_type: Type::Int,
                bound: NumBound::new(0, len - 1),
            };
            let exp = c.arbitrary(u)?;

            // Advance type and lval
            ty = *t;
            lval.index.0.push(exp);
        }

        // Initialize a new context with expected type `ty`
        let mut c = self.ctx.clone();
        c.expected = ExpectedType {
            is_const: false,
            value_type: ty.clone(),
            bound: NumBound::None,
        };

        // Generate a expression of matching type
        // As assigned variable is always int or float, this will not fail
        let exp = c.arbitrary(u)?;
        Ok(Stmt::Assign(Assign { lval, exp }))
    }
}

#[derive(Debug, Clone)]
pub struct AssignContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for AssignContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Stmt>) -> bool {
        let contexts: Vec<_> = self
            .0
            .ctx
            .iter()
            .map(|(id, ty)| {
                Box::new(SingleAssignContext {
                    ctx: self.0,
                    id: id.clone().into(),
                    ty: ty.clone(),
                }) as Box<dyn ArbitraryTo<Stmt>>
            })
            .collect();
        can_arbitrary_any(contexts.as_slice())
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        let contexts: Vec<_> = self
            .0
            .ctx
            .iter()
            .map(|(id, ty)| {
                Box::new(SingleAssignContext {
                    ctx: self.0,
                    id: id.clone().into(),
                    ty: ty.clone(),
                }) as Box<dyn ArbitraryTo<Stmt>>
            })
            .collect();
        arbitrary_any(u, contexts.as_slice())
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lval: LVal,
    pub exp: Exp,
}

impl Display for Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {};", self.lval, self.exp)
    }
}
