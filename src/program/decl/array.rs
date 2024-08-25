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
pub struct Index(pub Vec<Exp>);

impl<'a> ArbitraryTo<'a, Index> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Index> {
        let mut index = Vec::new();
        // Create zero or more array indicies
        for _ in 0..MAX_VEC_LEN {
            if u.arbitrary()? {
                break;
            }

            // Generate a random integer as dimension size
            let mut c = self.clone();
            c.expected = ExpectedType {
                is_const: true,
                value_type: Type::Int,
                bound: NumBound::new(0, MAX_ARR_LEN),
            };
            let exp = c.arbitrary(u)?;
            index.push(exp);
        }
        Ok(Index(index))
    }
}

impl Index {
    pub fn apply(&self, mut base_type: Type, c: &Context) -> Type {
        // Apply the array index to the base type
        // Reverse order: int x[2][8] -> [[int x 8] x 2]
        for exp in self.0.iter().rev() {
            let i = exp.eval(c).as_int();
            base_type = Type::Array(base_type.into(), i);
        }
        base_type
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|x| format!("[{}]", x))
                .collect::<Vec<String>>()
                .join("")
        )
    }
}
