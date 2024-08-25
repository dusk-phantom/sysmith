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

/// NestedContext generates an arbitrary expression
/// which is in a pair of parenthesis.
/// Example: `(4)`
#[derive(Clone, Debug)]
pub struct NestedContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Exp> for NestedContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        Ok(Exp::Exp(Box::new(self.0.arbitrary(u)?)))
    }
}
