use std::marker::PhantomData;

use super::*;

pub mod assign;
pub mod block;
pub mod break_stmt;
pub mod continue_stmt;
pub mod expr;
pub mod if_stmt;
pub mod return_stmt;
pub mod while_stmt;

pub use assign::*;
pub use block::*;
pub use break_stmt::*;
pub use continue_stmt::*;
pub use expr::*;
pub use if_stmt::*;
pub use return_stmt::*;
pub use while_stmt::*;

#[derive(Debug, Clone)]
pub enum Stmt {
    Assign(Assign),
    ExpStmt(ExpStmt),
    Block(Block),
    If(Box<If>),
    While(Box<While>),
    Break,
    Continue,
    Return(Return),
}

impl Resolve for Stmt {
    fn resolve(&self, _: &mut Context) {
        // Statement does not declare something,
        // so it doesn't modify context
    }
}

impl<'a> ArbitraryTo<'a, Stmt> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        // Increase context depth
        let c = self.next();

        // All possible choices
        let contexts = [
            Box::new(AssignContext(&c)) as Box<dyn ArbitraryTo<Stmt>>,
            Box::new(BlockContext(&c)) as Box<dyn ArbitraryTo<Stmt>>,
            Box::new(ExpContext(&c)) as Box<dyn ArbitraryTo<Stmt>>,
            Box::new(IfContext(&c)) as Box<dyn ArbitraryTo<Stmt>>,
            Box::new(WhileContext(&c)) as Box<dyn ArbitraryTo<Stmt>>,
            Box::new(BreakContext(&c)) as Box<dyn ArbitraryTo<Stmt>>,
            Box::new(ContinueContext(&c)) as Box<dyn ArbitraryTo<Stmt>>,
            Box::new(ReturnContext(&c)) as Box<dyn ArbitraryTo<Stmt>>,
        ];
        arbitrary_any(u, contexts.as_slice())
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Assign(a) => write!(f, "{}", a),
            Stmt::ExpStmt(a) => write!(f, "{}", a),
            Stmt::Block(a) => write!(f, "{}", a),
            Stmt::If(a) => write!(f, "{}", a),
            Stmt::While(a) => write!(f, "{}", a),
            Stmt::Break => write!(f, "break;"),
            Stmt::Continue => write!(f, "continue;"),
            Stmt::Return(a) => write!(f, "{}", a),
        }
    }
}
