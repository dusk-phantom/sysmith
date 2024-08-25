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
pub struct BreakContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for BreakContext<'_> {
    fn can_arbitrary(&self, _: std::marker::PhantomData<Stmt>) -> bool {
        // Prevent break outside of loop
        self.0.in_loop
    }

    fn arbitrary(&self, _: &mut Unstructured<'a>) -> Result<Stmt> {
        // Prevent break outside of loop
        if !self.0.in_loop {
            panic!("can't break outside loop; call can_arbitrary before arbitrary");
        }
        Ok(Stmt::Break)
    }
}
