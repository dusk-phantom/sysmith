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

/// Identifier, character is limited
#[derive(Debug, Clone)]
pub struct Ident(String);

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(s)
    }
}

impl<'a> ArbitraryTo<'a, Ident> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Ident> {
        /// Generate a random head character for an identifier
        fn arbitrary_head(u: &mut Unstructured) -> Result<u8> {
            let norm = u.int_in_range(0..=52)? as u8;
            if norm < 26 {
                // 'A'..='Z'
                Ok(norm + 65)
            } else if norm < 52 {
                // 'a'..='z'
                Ok(norm - 26 + 97)
            } else {
                // '_'
                Ok(95)
            }
        }

        /// Generate a random tail character for an identifier
        fn arbitrary_tail(u: &mut Unstructured) -> Result<u8> {
            let norm = u.int_in_range(0..=62)? as u8;
            if norm < 26 {
                // 'A'..='Z'
                Ok(norm + 65)
            } else if norm < 52 {
                // 'a'..='z'
                Ok(norm - 26 + 97)
            } else if norm < 62 {
                // '0'..='9'
                Ok(norm - 52 + 48)
            } else {
                // '_'
                Ok(95)
            }
        }

        // Generate head character
        let mut ident = Ident(String::new());
        ident.0.push(arbitrary_head(u)? as char);

        // Generate zero or more tail characters
        for _ in 0..MAX_VEC_LEN {
            if u.arbitrary()? {
                break;
            }
            ident.0.push(arbitrary_tail(u)? as char);
        }

        // While identifier exists in context, add another random character
        while self.ctx.contains_key(&ident.to_string()) {
            ident.0.push(arbitrary_tail(u)? as char);
        }
        Ok(ident)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub type IntConst = i32;
pub type FloatConst = f32;
