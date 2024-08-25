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

/// Current context of program generation
#[derive(Debug, Clone)]
pub struct Context {
    /// Mapping of variable names to their types
    pub ctx: HashMap<String, Type>,

    /// Mapping of variable names to their values
    pub env: HashMap<String, Value>,

    /// Expected type for the current expression
    pub expected: ExpectedType,

    /// Expected return type for the current function
    pub return_type: Type,

    /// Flag if the current context is in a loop
    pub in_loop: bool,

    /// Current AST depth
    pub depth: i32,
}

impl Context {
    /// Create a new context with increased depth
    pub fn next(&self) -> Self {
        let mut c = self.clone();
        c.depth += 1;
        c
    }

    /// Check if depth exceeds
    pub fn depth_is_valid(&self) -> bool {
        self.depth <= MAX_DEPTH
    }
}
