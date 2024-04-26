use super::*;

#[derive(Debug, Clone)]
pub struct Block {
    pub block_vec: Vec<BlockItem>,
}

impl<'a> ArbitraryInContext<'a> for Block {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        let mut block_vec = Vec::new();

        // Generate zero or more predecessing block items
        loop {
            if u.arbitrary()? {
                break;
            }
            let block_item = BlockItem::arbitrary(u, c)?;
            block_vec.push(block_item);
        }

        // Generate a return statement
        let return_stmt = Stmt::Return(Return::arbitrary(u, c)?);
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
    Decl(Decl),
    Stmt(Stmt),
}

impl<'a> ArbitraryInContext<'a> for BlockItem {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        // Generate declaration or statement at random
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(BlockItem::Decl(Decl::arbitrary(u, c)?)),
            1 => Ok(BlockItem::Stmt(Stmt::arbitrary(u, c)?)),
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
    Break(Break),
    Continue(Continue),
    Return(Return),
}

impl<'a> ArbitraryInContext<'a> for Stmt {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        if c.in_loop {
            // Generate a random statement including break and continue
            match u.arbitrary::<u8>()? % 8 {
                0 => Ok(Stmt::Assign(Assign::arbitrary(u, c)?)),
                1 => Ok(Stmt::ExpStmt(ExpStmt::arbitrary(u, c)?)),
                2 => Ok(Stmt::Block(Block::arbitrary(u, c)?)),
                3 => Ok(Stmt::If(Box::new(If::arbitrary(u, c)?))),
                4 => Ok(Stmt::While(Box::new(While::arbitrary(u, c)?))),
                5 => Ok(Stmt::Break(Break)),
                6 => Ok(Stmt::Continue(Continue)),
                7 => Ok(Stmt::Return(Return::arbitrary(u, c)?)),
                _ => unreachable!(),
            }
        } else {
            // Generate a random statement excluding break and continue
            match u.arbitrary::<u8>()? % 6 {
                0 => Ok(Stmt::Assign(Assign::arbitrary(u, c)?)),
                1 => Ok(Stmt::ExpStmt(ExpStmt::arbitrary(u, c)?)),
                2 => Ok(Stmt::Block(Block::arbitrary(u, c)?)),
                3 => Ok(Stmt::If(Box::new(If::arbitrary(u, c)?))),
                4 => Ok(Stmt::While(Box::new(While::arbitrary(u, c)?))),
                5 => Ok(Stmt::Return(Return::arbitrary(u, c)?)),
                _ => unreachable!(),
            }
        }
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
            Stmt::Break(a) => write!(f, "{}", a),
            Stmt::Continue(a) => write!(f, "{}", a),
            Stmt::Return(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lval: LVal,
    pub exp: Exp,
}

impl<'a> ArbitraryInContext<'a> for Assign {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        // Randomly find a variable in context
        // TODO ensure such variable exists
        let lval = LVal::arbitrary(u, c)?;

        // Initialize a new context with expected type `ty`
        let mut c = c.clone();
        c.expected_type = Type::Int;
        c.expected_const = false;

        // Generate a expression of matching type
        // Can be generated because `c = c` is possible
        let exp = Exp::arbitrary(u, &c)?;
        Ok(Assign { lval, exp })
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

impl<'a> ArbitraryInContext<'a> for ExpStmt {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        let exp = if u.arbitrary()? {
            // Generate a random type for this expression
            let func_type = FuncType::arbitrary(u)?;

            // Initialize a context expecting this type
            let mut c = c.clone();
            c.expected_type = func_type.clone().into();
            c.expected_const = false;

            // Generate a random statement of this type (non-constant)
            Some(Exp::arbitrary(u, &c)?)
        } else {
            // Possible to generate a stray semicolon
            None
        };
        Ok(ExpStmt { exp })
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

impl<'a> ArbitraryInContext<'a> for If {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        // Context for condition expects int type
        let mut c = c.clone();
        c.expected_type = Type::Int;
        c.expected_const = false;
        let cond = Exp::arbitrary(u, &c)?;

        // Context for then and else is in loop
        c.in_loop = true;
        let then = Stmt::arbitrary(u, &c)?;
        let else_then = if u.arbitrary()? {
            Some(Stmt::arbitrary(u, &c)?)
        } else {
            None
        };
        Ok(If {
            cond,
            then,
            else_then,
        })
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

impl<'a> ArbitraryInContext<'a> for While {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        // Context for condition expects int type
        let mut c = c.clone();
        c.expected_type = Type::Int;
        c.expected_const = false;
        let cond = Exp::arbitrary(u, &c)?;

        // Context for body is in loop
        c.in_loop = true;
        let body = Stmt::arbitrary(u, &c)?;
        Ok(While { cond, body })
    }
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {}", self.cond, self.body)
    }
}

#[derive(Debug, Clone)]
pub struct Break;

impl Display for Break {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "break;")
    }
}

#[derive(Debug, Clone)]
pub struct Continue;

impl Display for Continue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "continue;")
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub exp: Option<Exp>,
}

impl<'a> ArbitraryInContext<'a> for Return {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        let exp = if u.arbitrary()? {
            // Context for returned expression expects return type
            let mut c = c.clone();
            c.expected_type = c.return_type.clone();
            Some(Exp::arbitrary(u, &c)?)
        } else {
            None
        };
        Ok(Return { exp })
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