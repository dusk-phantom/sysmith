use std::marker::PhantomData;

use super::*;

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
            if !u.arbitrary()? {
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

#[derive(Debug, Clone)]
struct BlockContext<'a>(&'a Context);

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
pub struct Assign {
    pub lval: LVal,
    pub exp: Exp,
}

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
        while let Type::Array(t, _) = current_type {
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
            // Generate a random index in bound
            let mut c = self.ctx.clone();
            c.expected = ExpectedType {
                is_const: false,
                value_type: Type::Int,
                bound: Some(IntBound::new(0, len - 1)),
            };
            let exp = c.arbitrary(u)?;

            // Advance type and lval
            ty = *t;
            lval
                .index
                .0
                .push(exp);
        }

        // Initialize a new context with expected type `ty`
        let mut c = self.ctx.clone();
        c.expected = ExpectedType {
            is_const: false,
            value_type: ty.clone(),
            bound: None,
        };

        // Generate a expression of matching type
        // As assigned variable is always int or float, this will not fail
        let exp = c.arbitrary(u)?;
        Ok(Stmt::Assign(Assign { lval, exp }))
    }
}

#[derive(Debug, Clone)]
struct AssignContext<'a>(&'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for AssignContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Stmt>) -> bool {
        let contexts: Vec<_> = self.0.ctx.iter().map(|(id, ty)| {
            Box::new(SingleAssignContext {
                ctx: self.0,
                id: id.clone().into(),
                ty: ty.clone(),
            }) as Box<dyn ArbitraryTo<Stmt>>
        }).collect();
        can_arbitrary_any(contexts.as_slice())
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        let contexts: Vec<_> = self.0.ctx.iter().map(|(id, ty)| {
            Box::new(SingleAssignContext {
                ctx: self.0,
                id: id.clone().into(),
                ty: ty.clone(),
            }) as Box<dyn ArbitraryTo<Stmt>>
        }).collect();
        arbitrary_any(u, contexts.as_slice())
    }
}

impl Display for Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {};", self.lval, self.exp)
    }
}

#[derive(Debug, Clone)]
pub struct ExpStmt {
    pub exp: Option<Exp>,
}

#[derive(Debug, Clone)]
struct ExpContext<'a>(&'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for ExpContext<'_> {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        let exp = if u.arbitrary()? {
            // Generate a random type for this expression
            let func_type = FuncType::arbitrary(u)?;

            // Initialize a context expecting this type
            let mut c = self.0.clone();
            c.expected = ExpectedType {
                is_const: false,
                value_type: func_type.clone().into(),
                bound: None,
            };

            // Generate a random statement of this type (non-constant)
            // If impossible, generate a stray semicolon
            match c.arbitrary(u) {
                Ok(e) => Some(e),
                Err(_) => None,
            }
        } else {
            // Possible to generate a stray semicolon
            None
        };
        Ok(Stmt::ExpStmt(ExpStmt { exp }))
    }
}

impl Display for ExpStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.exp {
            Some(e) => write!(f, "{};", e),
            None => write!(f, ";"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Exp,
    pub then: Stmt,
    pub else_then: Option<Stmt>,
}

#[derive(Debug, Clone)]
struct IfContext<'a>(&'a Context);

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
            bound: None,
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

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Exp,
    pub body: Stmt,
}

#[derive(Debug, Clone)]
struct WhileContext<'a>(&'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for WhileContext<'_> {
    fn can_arbitrary(&self, _: std::marker::PhantomData<Stmt>) -> bool {
        self.0.depth_is_valid()
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Stmt> {
        // Context for condition expects int type
        let mut c = self.0.clone();
        c.expected = ExpectedType {
            is_const: false,
            value_type: Type::Int,
            bound: None,
        };
        let cond = c.arbitrary(u)?;

        // Context for body is in loop
        c.in_loop = true;
        let body = c.arbitrary(u)?;
        Ok(Stmt::While(Box::new(While { cond, body })))
    }
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {}", self.cond, self.body)
    }
}

#[derive(Debug, Clone)]
struct BreakContext<'a>(&'a Context);

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

#[derive(Debug, Clone)]
struct ContinueContext<'a>(&'a Context);

impl<'a> ArbitraryTo<'a, Stmt> for ContinueContext<'_> {
    fn can_arbitrary(&self, _: std::marker::PhantomData<Stmt>) -> bool {
        // Prevent continue outside of loop
        self.0.in_loop
    }

    fn arbitrary(&self, _: &mut Unstructured<'a>) -> Result<Stmt> {
        // Prevent continue outside of loop
        if !self.0.in_loop {
            panic!("can't continue outside loop; call can_arbitrary before arbitrary");
        }
        Ok(Stmt::Continue)
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub exp: Option<Exp>,
}

#[derive(Debug, Clone)]
struct ReturnContext<'a>(&'a Context);

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
                    bound: None,
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
