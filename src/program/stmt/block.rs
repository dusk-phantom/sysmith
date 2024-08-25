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
pub struct BlockContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for BlockContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Stmt>) -> bool {
        // Prevent block from being too deep
        self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        let block = self.0.arbitrary(u)?;
        Ok(Stmt::Block(block))
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub block_vec: Vec<BlockItem>,
}

impl<'a> ArbitraryTo<'a, Block> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Block> {
        let mut block_vec = Vec::new();
        let mut local_context = self.clone();

        // Generate zero or more predecessing block items
        for _ in 0..MAX_VEC_LEN {
            if u.arbitrary()? {
                break;
            }
            let block_item: BlockItem = local_context.arbitrary(u)?;
            block_item.resolve(&mut local_context);
            block_vec.push(block_item);
        }

        // Generate a return statement
        let return_stmt = ReturnContext(&local_context).arbitrary(u)?;
        block_vec.push(BlockItem::Stmt(return_stmt));
        Ok(Block { block_vec })
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{ {} }}",
            self.block_vec
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Decl(VarDecl),
    Stmt(Stmt),
}

impl Resolve for BlockItem {
    fn resolve(&self, c: &mut Context) {
        match self {
            BlockItem::Decl(a) => a.resolve(c),
            BlockItem::Stmt(a) => a.resolve(c),
        }
    }
}

impl<'a> ArbitraryTo<'a, BlockItem> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<BlockItem> {
        // Generate declaration or statement at random
        match u.int_in_range(0..=1)? {
            0 => Ok(BlockItem::Decl(self.arbitrary(u)?)),
            1 => Ok(BlockItem::Stmt(self.arbitrary(u)?)),
            _ => unreachable!(),
        }
    }
}

impl Display for BlockItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockItem::Decl(a) => write!(f, "{}", a),
            BlockItem::Stmt(a) => write!(f, "{}", a),
        }
    }
}
