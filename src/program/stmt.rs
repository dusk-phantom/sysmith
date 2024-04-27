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
            if u.arbitrary()? {
                break;
            }
            let block_item: BlockItem = local_context.arbitrary(u)?;
            block_item.resolve(&mut local_context);
            block_vec.push(block_item);
        }

        // Generate a return statement
        let return_stmt = Stmt::Return(local_context.arbitrary(u)?);
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
    Break(Break),
    Continue(Continue),
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
        // Increase depth of context, converge if depth exceeds
        let mut c = self.clone();
        c.depth += 1;
        if c.depth > MAX_DEPTH {
            return Ok(Stmt::Block(Block {
                block_vec: Vec::new(),
            }));
        }

        // Generate a random statement including break and continue
        match u.int_in_range(0..=7)? {
            0 => Ok(Stmt::Assign(c.arbitrary(u)?)),
            1 => Ok(Stmt::ExpStmt(c.arbitrary(u)?)),
            2 => Ok(Stmt::Block(c.arbitrary(u)?)),
            3 => Ok(Stmt::If(Box::new(c.arbitrary(u)?))),
            4 => Ok(Stmt::While(Box::new(c.arbitrary(u)?))),
            5 => {
                // Prevent break outside of loop
                if !c.in_loop {
                    return Err(arbitrary::Error::EmptyChoose);
                }
                Ok(Stmt::Break(Break))
            }
            6 => {
                // Prevent continue outside of loop
                if !c.in_loop {
                    return Err(arbitrary::Error::EmptyChoose);
                }
                Ok(Stmt::Continue(Continue))
            }
            7 => Ok(Stmt::Return(c.arbitrary(u)?)),
            _ => unreachable!(),
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

impl<'a> ArbitraryTo<'a, Assign> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Assign> {
        // Filter number or array left values from context
        let mut candidates: Vec<(LVal, Type)> = self
            .ctx
            .iter()
            .filter_map(|(id, ty)| {
                let mut current_type = ty.clone();
                let mut current_lval = LVal {
                    id: id.clone().into(),
                    index: Index(Vec::new()),
                };

                // Collapse array type,
                // did not allow assigning an array to another
                while let Type::Array(t, _) = current_type {
                    // Generate a random index in bound
                    // TODO refer to constant here
                    let index = match u.arbitrary::<i32>() {
                        Ok(i) => i,
                        Err(err) => return Some(Err(err)),
                    };

                    current_type = *t;
                    current_lval
                        .index
                        .0
                        .push(Exp::Number(Number::IntConst(index)));
                }

                // Only accept int or float type as left value,
                // void or function will be rejected
                match current_type {
                    Type::Int | Type::Float => Some(Ok((current_lval, current_type))),
                    _ => None,
                }
            })
            .collect::<Result<_>>()?;

        // If there's no candidate, return an error
        if candidates.is_empty() {
            return Err(arbitrary::Error::EmptyChoose);
        }

        // Randomly choose a candidate
        let selected_index = u.arbitrary::<usize>()? % candidates.len();
        let (lval, ty) = candidates.swap_remove(selected_index);

        // Initialize a new context with expected type `ty`
        let mut c = self.clone();
        c.expected_type = ty.clone();
        c.expected_const = false;

        // Generate a expression of matching type
        let exp = c.arbitrary(u)?;
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

impl<'a> ArbitraryTo<'a, ExpStmt> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<ExpStmt> {
        let exp = if u.arbitrary()? {
            // Generate a random type for this expression
            let func_type = FuncType::arbitrary(u)?;

            // Initialize a context expecting this type
            let mut c = self.clone();
            c.expected_type = func_type.clone().into();
            c.expected_const = false;

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

impl<'a> ArbitraryTo<'a, If> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<If> {
        // Context for condition expects int type
        let mut c = self.clone();
        c.expected_type = Type::Int;
        c.expected_const = false;
        let cond = c.arbitrary(u)?;

        // Context for then and else is in loop
        c.in_loop = true;
        let then = c.arbitrary(u)?;
        let else_then = if u.arbitrary()? {
            Some(c.arbitrary(u)?)
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

impl<'a> ArbitraryTo<'a, While> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<While> {
        // Context for condition expects int type
        let mut c = self.clone();
        c.expected_type = Type::Int;
        c.expected_const = false;
        let cond = c.arbitrary(u)?;

        // Context for body is in loop
        c.in_loop = true;
        let body = c.arbitrary(u)?;
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

impl<'a> ArbitraryTo<'a, Return> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Return> {
        let exp = match self.return_type {
            Type::Void => None,
            Type::Float | Type::Int => {
                // Context for returned expression expects return type
                let mut c = self.clone();
                c.expected_type = c.return_type.clone();
                Some(c.arbitrary(u)?)
            }
            _ => panic!("Invalid return type"),
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
