use core::panic;
use std::collections::HashMap;
use std::fmt::{Debug, Display};

use libfuzzer_sys::arbitrary::{self, Unstructured};
use libfuzzer_sys::arbitrary::{Arbitrary, Result};

/// All possible types
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    Void,
    Func(Box<Type>, Vec<Type>),
    Array(Box<Type>, i32),
    Pointer(Box<Type>),
}

impl From<BType> for Type {
    fn from(btype: BType) -> Self {
        match btype {
            BType::Int => Type::Int,
            BType::Float => Type::Float,
        }
    }
}

impl From<FuncType> for Type {
    fn from(func_type: FuncType) -> Self {
        match func_type {
            FuncType::Int => Type::Int,
            FuncType::Float => Type::Float,
            FuncType::Void => Type::Void,
        }
    }
}

/// All possible values
#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Array(Vec<Value>),
}

impl std::ops::Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Int(a) => Value::Int(-a),
            Value::Float(a) => Value::Float(-a),
            Value::Array(_) => panic!("Cannot negate an array"),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f32),
            _ => panic!("Cannot add an array"),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 - b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a - b as f32),
            _ => panic!("Cannot subtract an array"),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 * b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f32),
            _ => panic!("Cannot multiply an array"),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 / b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a / b as f32),
            _ => panic!("Cannot divide an array"),
        }
    }
}

impl std::ops::Rem for Value {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 % b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a % b as f32),
            _ => panic!("Cannot modulo an array"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Int(a), Value::Float(b)) => *a as f32 == *b,
            (Value::Float(a), Value::Int(b)) => *a == *b as f32,
            _ => panic!("Cannot compare an array"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
            (Value::Int(a), Value::Float(b)) => (*a as f32).partial_cmp(b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(*b as f32)),
            _ => panic!("Cannot compare an array"),
        }
    } 
}

/// Current type context
#[derive(Debug, Clone)]
pub struct Context {
    /// Mapping of variable names to their types
    pub ctx: HashMap<String, Type>,

    /// Mapping of variable names to their values
    pub env: HashMap<String, Value>,

    /// Expected type for the current expression
    pub expected_type: Type,

    /// Flag if expected type is a constant
    pub expected_const: bool,

    /// Expected return type for the current function
    pub return_type: Type,

    /// Flag if the current context is in a loop
    pub in_loop: bool,
}

#[derive(Debug, Clone)]
pub struct CompUnit {
    pub global_items: Vec<GlobalItems>,
}

impl<'a> Arbitrary<'a> for CompUnit {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let mut global_items = Vec::new();

        // Initialize with empty context
        let c = Context {
            ctx: HashMap::new(),
            env: HashMap::new(),
            expected_type: Type::Void,
            expected_const: false,
            return_type: Type::Void,
            in_loop: false,
        };

        // Generate at least one global item
        loop {
            let item = GlobalItems::arbitrary(u, &c)?;
            global_items.push(item);
            if u.arbitrary()? {
                return Ok(CompUnit { global_items });
            }
        }
    }
}

impl Display for CompUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.global_items
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

#[derive(Debug, Clone)]
pub enum GlobalItems {
    Decl(Decl),
    FuncDef(FuncDef),
}

impl GlobalItems {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // Generate variable or function at random
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(GlobalItems::Decl(Decl::arbitrary(u, c)?)),
            1 => Ok(GlobalItems::FuncDef(FuncDef::arbitrary(u, c)?)),
            _ => unreachable!(),
        }
    }
}

impl Display for GlobalItems {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlobalItems::Decl(a) => write!(f, "{}", a),
            GlobalItems::FuncDef(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    VarDecl(VarDecl),
}

impl Decl {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        Ok(Decl::VarDecl(VarDecl::arbitrary(u, c)?))
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::VarDecl(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub btype: BType,
    pub const_def_vec: PVec<VarDef>,
}

impl VarDecl {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // Generate a random basic type
        let btype = BType::arbitrary(u)?;

        // Generate at lease one definition
        let mut const_def_vec = Vec::new();
        loop {
            let is_const = u.arbitrary()?;
            let const_def = VarDef::arbitrary(u, c, btype.clone().into(), is_const)?;
            const_def_vec.push(const_def);
            if u.arbitrary()? {
                return Ok(VarDecl {
                    btype,
                    const_def_vec: PVec(const_def_vec),
                });
            }
        }
    }
}

impl Display for VarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "const {} {};",
            self.btype,
            self.const_def_vec
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone)]
pub enum BType {
    Int,
    Float,
}

impl BType {
    fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        // Generate a random basic type
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(BType::Int),
            1 => Ok(BType::Float),
            _ => unreachable!(),
        }
    }
}

impl Display for BType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BType::Int => write!(f, "int"),
            BType::Float => write!(f, "float"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Index(Vec<Exp>);

impl Index {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        let mut index = Vec::new();
        loop {
            // Create zero or more array indicies
            if u.arbitrary()? {
                return Ok(Index(index));
            }

            // Initialize a context expecting `const int`
            let mut c = c.clone();
            c.expected_type = Type::Int;
            c.expected_const = true;

            // Create a const integer as the next array index
            let exp = Exp::arbitrary(u, &c)?;
            index.push(exp);
        }
    }

    fn apply(&self, mut base_type: Type, c: &Context) -> Type {
        // Apply the array index to the base type
        // Reverse order: int x[2][8] -> [[int x 8] x 2]
        for exp in self.0.iter().rev() {
            let Value::Int(i) = exp.eval(c) else {
                panic!("Const expression as index of array must be an integer")
            };
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

#[derive(Debug, Clone)]
pub struct VarDef {
    pub ident: Ident,
    pub index: Index,
    pub init_val: VarInitVal,
}

impl VarDef {
    fn arbitrary(u: &mut Unstructured, c: &Context, base_type: Type, is_const: bool) -> Result<Self> {
        // Generate a random identifier for this definition
        let ident = Ident::arbitrary(u)?;

        // Generate random array type
        let index = Index::arbitrary(u, c)?;
        let var_type = index.apply(base_type, c);

        // Generate assigned value
        let mut c: Context = c.clone();
        c.expected_type = var_type.clone();
        c.expected_const = is_const;
        let init_val = VarInitVal::arbitrary(u, &c)?;

        // Return the variable definition
        Ok(VarDef {
            ident,
            index,
            init_val,
        })
    }
}

impl Display for VarDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{} = {}",
            self.ident,
            self.index,
            self.init_val,
        )
    }
}

#[derive(Debug, Clone)]
pub enum VarInitVal {
    Exp(Exp),
    InitValVec(Vec<VarInitVal>),
}

impl VarInitVal {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match c.expected_type.clone() {
            Type::Int | Type::Float => {
                // Random integer or float
                Ok(VarInitVal::Exp(Exp::arbitrary(u, c)?))
            }
            Type::Array(content_type, content_len) => {
                // Initialize a context expecting `content_type`
                // Constant flag inherits from parent context
                let mut c = c.clone();
                c.expected_type = *content_type.clone();

                // Fill array with random contents
                let mut init_val_vec = Vec::new();
                for _ in 0..content_len {
                    let init_val = VarInitVal::arbitrary(u, &c)?;
                    init_val_vec.push(init_val);
                }
                Ok(VarInitVal::InitValVec(init_val_vec))
            }
            _ => panic!("Invalid type"),
        }
    }

    fn eval(&self, ctx: &Context) -> Value {
        match self {
            VarInitVal::Exp(a) => a.eval(ctx),
            VarInitVal::InitValVec(a) => Value::Array(a.iter().map(|x| x.eval(ctx)).collect()),
        }
    }
}

impl Display for VarInitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarInitVal::Exp(a) => write!(f, "{}", a),
            VarInitVal::InitValVec(a) => write!(
                f,
                "{{ {} }}",
                a.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FuncDef {
    ParameterFuncDef((FuncType, Ident, FuncFParams, Block)),
}

impl FuncDef {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // Generate a random function signature
        let func_type = FuncType::arbitrary(u)?;
        let ident = Ident::arbitrary(u)?;
        let func_fparams = FuncFParams::arbitrary(u, c)?;

        // Initialize a context expecting return type `func_type`
        let mut c = c.clone();
        c.return_type = func_type.clone().into();

        // Generate function statements with return type specified
        let block = Block::arbitrary(u, &c)?;
        Ok(FuncDef::ParameterFuncDef((func_type, ident, func_fparams, block)))
    }
}

impl Display for FuncDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncDef::ParameterFuncDef((a, b, c, d)) => write!(f, "{} {}({}) {}", a, b, c, d),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FuncType {
    Void,
    Int,
    Float,
}

impl FuncType {
    fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
            0 => Ok(FuncType::Void),
            1 => Ok(FuncType::Int),
            2 => Ok(FuncType::Float),
            _ => unreachable!(),
        }
    }
}

impl Display for FuncType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncType::Void => write!(f, "void"),
            FuncType::Int => write!(f, "int"),
            FuncType::Float => write!(f, "float"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncFParams {
    pub func_fparams_vec: Vec<FuncFParam>,
}

impl FuncFParams {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        let mut func_fparams_vec = Vec::new();
        loop {
            // Generate zero or more function params
            if u.arbitrary()? {
                return Ok(FuncFParams { func_fparams_vec });
            }
            let func_fparam = FuncFParam::arbitrary(u, c)?;
            func_fparams_vec.push(func_fparam);
        }
    }
}

impl Display for FuncFParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.func_fparams_vec
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone)]
pub enum FuncFParam {
    NonArray((BType, Ident)),
    Array((BType, Ident, Index)),
}

impl FuncFParam {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // Generate array (x: int[][4]) or non-array (x: int) function parameter
        match u.arbitrary::<u8>()? % 2 {
            0 => {
                // Generate signature
                let btype = BType::arbitrary(u)?;
                let ident = Ident::arbitrary(u)?;
                Ok(FuncFParam::NonArray((btype, ident)))
            }
            1 => {
                // Generate signature
                let btype = BType::arbitrary(u)?;
                let ident = Ident::arbitrary(u)?;

                // Generate random array type
                let var_index = Index::arbitrary(u, c)?;
                Ok(FuncFParam::Array((btype, ident, var_index)))
            }
            _ => unreachable!(),
        }
    }
}

impl Display for FuncFParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncFParam::NonArray((a, b)) => write!(f, "{} {}", a, b),
            FuncFParam::Array((a, b, c)) => write!(f, "{} {}[]{}", a, b, c),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub block_vec: Vec<BlockItem>,
}

impl Block {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
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

impl BlockItem {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
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

impl Stmt {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
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

impl Assign {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // Randomly find a variable in context
        // TODO ensure such variable exists
        let (lval, ty) = LVal::arbitrary_infer(u, c)?;

        // Initialize a new context with expected type `ty`
        let mut c = c.clone();
        c.expected_type = ty;
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

impl ExpStmt {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
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
    pub cond: Cond,
    pub then: Stmt,
    pub else_then: Option<Stmt>,
}

impl If {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // Context for condition expects int type
        let mut c = c.clone();
        c.expected_type = Type::Int;
        c.expected_const = false;
        let cond = Cond::arbitrary(u, &c)?;

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
    pub cond: Cond,
    pub body: Stmt,
}

impl While {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // Context for condition expects int type
        let mut c = c.clone();
        c.expected_type = Type::Int;
        c.expected_const = false;
        let cond = Cond::arbitrary(u, &c)?;

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

impl Return {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
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

#[derive(Debug, Clone)]
pub struct Exp {
    pub add_exp: Box<AddExp>,
}

impl Exp {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        let add_exp = AddExp::arbitrary(u, c)?;
        Ok(Exp {
            add_exp: Box::new(add_exp),
        })
    }

    fn eval(&self, ctx: &Context) -> Value {
        self.add_exp.eval(ctx)
    }
}

impl Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.add_exp)
    }
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub l_or_exp: LOrExp,
}

impl Cond {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        let l_or_exp = LOrExp::arbitrary(u, c)?;
        Ok(Cond { l_or_exp })
    }
}

impl Display for Cond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.l_or_exp)
    }
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub id: Ident,
    pub exp_vec: Vec<Exp>,
}

impl LVal {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // TODO make arbitrary identifier type match
        let id = Ident::arbitrary(u)?;
        let exp_vec = Vec::new();
        Ok(LVal { id, exp_vec })
    }

    fn arbitrary_infer(u: &mut Unstructured, c: &Context) -> Result<(Self, Type)> {
        // TODO make arbitrary identifier type match
        let id = Ident::arbitrary(u)?;
        let exp_vec = Vec::new();
        Ok((LVal { id, exp_vec }, Type::Int))
    }

    fn eval(&self, ctx: &Context) -> Value {
        let mut array_type = ctx.ctx.get(&self.id.to_string()).unwrap().clone();
        let mut array_value = ctx.env.get(&self.id.to_string()).unwrap().clone();
        for exp in &self.exp_vec {
            let index_value: Value = exp.eval(ctx);
            match (array_type, array_value) {
                (Type::Array(content_type, content_len), Value::Array(content_value)) => {
                    if let Value::Int(i) = index_value {
                        if i < 0 || i >= content_len {
                            panic!("Index out of range")
                        }
                        array_type = *content_type;
                        array_value = content_value[i as usize].clone();
                    } else {
                        panic!("Index must be an integer")
                    }
                }
                _ => panic!("Not an array"),
            }
        }
        array_value
    }
}

impl Display for LVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.id,
            self.exp_vec
                .iter()
                .map(|a| format!("[{}]", a))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(Number),
}

impl PrimaryExp {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
            0 => Ok(PrimaryExp::Exp(Box::new(Exp::arbitrary(u, c)?))),
            1 => Ok(PrimaryExp::LVal(LVal::arbitrary(u, c)?)),
            2 => Ok(PrimaryExp::Number(Number::arbitrary(u, c)?)),
            _ => unreachable!(),
        }
    }

    fn eval(&self, ctx: &Context) -> Value {
        match self {
            PrimaryExp::Exp(a) => a.eval(ctx),
            PrimaryExp::LVal(a) => a.eval(ctx),
            PrimaryExp::Number(a) => a.eval(ctx),
        }
    }
}

impl Display for PrimaryExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimaryExp::Exp(a) => write!(f, "{}", a),
            PrimaryExp::LVal(a) => write!(f, "{}", a),
            PrimaryExp::Number(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Number {
    IntConst(IntConst),
    FloatConst(FloatConst),
}

impl Number {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match c.expected_type {
            Type::Int => Ok(Number::IntConst(IntConst::arbitrary(u)?)),
            Type::Float => Ok(Number::FloatConst(FloatConst::arbitrary(u)?)),
            _ => panic!("Invalid type for a number literal"),
        }
    }

    fn eval(&self, _: &Context) -> Value {
        match self {
            Number::IntConst(a) => Value::Int(*a),
            Number::FloatConst(a) => Value::Float(*a),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::IntConst(a) => write!(f, "{}", a),
            Number::FloatConst(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    FuncCall((Ident, Option<FuncRParams>)),
    OpUnary((UnaryOp, Box<UnaryExp>)),
}

impl UnaryExp {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
            0 => Ok(UnaryExp::PrimaryExp(Box::new(PrimaryExp::arbitrary(u, c)?))),
            1 => {
                // TODO make sure function type match
                let id = Ident::arbitrary(u)?;
                let func_rparams = if u.arbitrary()? {
                    Some(FuncRParams::arbitrary(u, c)?)
                } else {
                    None
                };
                Ok(UnaryExp::FuncCall((id, func_rparams)))
            }
            2 => {
                let unary_op = UnaryOp::arbitrary(u)?;
                let unary_exp = UnaryExp::arbitrary(u, c)?;
                Ok(UnaryExp::OpUnary((unary_op, Box::new(unary_exp))))
            }
            _ => unreachable!(),
        }
    }

    fn eval(&self, ctx: &Context) -> Value {
        match self {
            UnaryExp::PrimaryExp(a) => a.eval(ctx),
            UnaryExp::FuncCall((_, _)) => {
                panic!("No function is constant")
            }
            UnaryExp::OpUnary((op, exp)) => {
                let exp = exp.eval(ctx);
                match op {
                    UnaryOp::Add => exp,
                    UnaryOp::Minus => -exp,
                    UnaryOp::Exclamation => if exp == Value::Int(0) {
                        Value::Int(1)
                    } else {
                        Value::Int(0)
                    },
                }
            }
        }
    }
}

impl Display for UnaryExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryExp::PrimaryExp(a) => write!(f, "{}", a),
            UnaryExp::FuncCall((id, None)) => write!(f, "{}()", id),
            UnaryExp::FuncCall((id, Some(param))) => write!(f, "{}({})", id, param),
            UnaryExp::OpUnary((a, b)) => write!(f, "{}({})", a, b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Add,
    Minus,
    Exclamation,
}

impl UnaryOp {
    fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
            0 => Ok(UnaryOp::Add),
            1 => Ok(UnaryOp::Minus),
            2 => Ok(UnaryOp::Exclamation),
            _ => unreachable!(),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Add => write!(f, "+"),
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Exclamation => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncRParams {
    pub exp_vec: Vec<Exp>,
}

impl FuncRParams {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        let mut exp_vec = Vec::new();
        loop {
            // TODO make param type match
            let exp = Exp::arbitrary(u, c)?;
            exp_vec.push(exp);
            if u.arbitrary()? {
                return Ok(FuncRParams { exp_vec });
            }
        }
    }
}

impl Display for FuncRParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.exp_vec
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone)]
pub enum MulExp {
    UnaryExp(Box<UnaryExp>),
    MulExp((Box<MulExp>, UnaryExp)),
    DivExp((Box<MulExp>, UnaryExp)),
    ModExp((Box<MulExp>, UnaryExp)),
}

impl MulExp {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 4 {
            0 => Ok(MulExp::UnaryExp(Box::new(UnaryExp::arbitrary(u, c)?))),
            1 => {
                let a = MulExp::arbitrary(u, c)?;
                let b = UnaryExp::arbitrary(u, c)?;
                Ok(MulExp::MulExp((Box::new(a), b)))
            }
            2 => {
                let a = MulExp::arbitrary(u, c)?;
                let b = UnaryExp::arbitrary(u, c)?;
                Ok(MulExp::DivExp((Box::new(a), b)))
            }
            3 => {
                let a = MulExp::arbitrary(u, c)?;
                let b = UnaryExp::arbitrary(u, c)?;
                Ok(MulExp::ModExp((Box::new(a), b)))
            }
            _ => unreachable!(),
        }
    }

    fn eval(&self, ctx: &Context) -> Value {
        match self {
            Self::MulExp((a, b)) => {
                let a = a.eval(ctx);
                let b = b.eval(ctx);
                a * b
            }
            Self::DivExp((a, b)) => {
                let a = a.eval(ctx);
                let b = b.eval(ctx);
                a / b
            }
            Self::ModExp((a, b)) => {
                let a = a.eval(ctx);
                let b = b.eval(ctx);
                a % b
            }
            Self::UnaryExp(a) => a.eval(ctx),
        }
    }
}

impl Display for MulExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnaryExp(a) => write!(f, "{}", a),
            Self::MulExp((a, b)) => write!(f, "({}) * ({})", a, b),
            Self::DivExp((a, b)) => write!(f, "({}) / ({})", a, b),
            Self::ModExp((a, b)) => write!(f, "({}) % ({})", a, b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AddOp {
    Add,
    Minus,
}

impl AddOp {
    fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(AddOp::Add),
            1 => Ok(AddOp::Minus),
            _ => unreachable!(),
        }
    }
}

impl Display for AddOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddOp::Add => write!(f, "+"),
            AddOp::Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AddExp {
    MulExp(Box<MulExp>),
    OpExp((Box<AddExp>, AddOp, MulExp)),
}

impl AddExp {
    fn arbitrary(u: &mut Unstructured, ctx: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(AddExp::MulExp(Box::new(MulExp::arbitrary(u, ctx)?))),
            1 => {
                let a = AddExp::arbitrary(u, ctx)?;
                let b = AddOp::arbitrary(u)?;
                let c = MulExp::arbitrary(u, ctx)?;
                Ok(AddExp::OpExp((Box::new(a), b, c)))
            }
            _ => unreachable!(),
        }
    }

    fn eval(&self, ctx: &Context) -> Value {
        match self {
            Self::MulExp(a) => a.eval(ctx),
            Self::OpExp((a, b, c)) => {
                let a = a.eval(ctx);
                let c = c.eval(ctx);
                match b {
                    AddOp::Add => a + c,
                    AddOp::Minus => a - c,
                }
            }
        }
    }
}

impl Display for AddExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MulExp(a) => write!(f, "{}", a),
            Self::OpExp((a, b, c)) => write!(f, "({}) {} ({})", a, b, c),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RelOp {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}

impl RelOp {
    fn arbitrary(u: &mut Unstructured) -> Result<Self> {
        match u.arbitrary::<u8>()? % 4 {
            0 => Ok(RelOp::Less),
            1 => Ok(RelOp::LessOrEqual),
            2 => Ok(RelOp::Greater),
            3 => Ok(RelOp::GreaterOrEqual),
            _ => unreachable!(),
        }
    }
}

impl Display for RelOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RelOp::Less => write!(f, "<"),
            RelOp::LessOrEqual => write!(f, "<="),
            RelOp::Greater => write!(f, ">"),
            RelOp::GreaterOrEqual => write!(f, ">="),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RelExp {
    AddExp(AddExp),
    OpExp((Box<RelExp>, RelOp, AddExp)),
}

impl RelExp {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(RelExp::AddExp(AddExp::arbitrary(u, c)?)),
            1 => {
                let a = RelExp::arbitrary(u, c)?;
                let b = RelOp::arbitrary(u)?;
                let c = AddExp::arbitrary(u, c)?;
                Ok(RelExp::OpExp((Box::new(a), b, c)))
            }
            _ => unreachable!(),
        }
    }
}

impl Display for RelExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AddExp(a) => write!(f, "{}", a),
            Self::OpExp((a, b, c)) => write!(f, "({}) {} ({})", a, b, c),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EqExp {
    RelExp(RelExp),
    EqualExp((Box<EqExp>, RelExp)),
    NotEqualExp((Box<EqExp>, RelExp)),
}

impl EqExp {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 3 {
            0 => Ok(EqExp::RelExp(RelExp::arbitrary(u, c)?)),
            1 => {
                let a = EqExp::arbitrary(u, c)?;
                let b = RelExp::arbitrary(u, c)?;
                Ok(EqExp::EqualExp((Box::new(a), b)))
            }
            2 => {
                let a = EqExp::arbitrary(u, c)?;
                let b = RelExp::arbitrary(u, c)?;
                Ok(EqExp::NotEqualExp((Box::new(a), b)))
            }
            _ => unreachable!(),
        }
    }
}

impl Display for EqExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RelExp(a) => write!(f, "{}", a),
            Self::EqualExp((a, b)) => write!(f, "({}) == ({})", a, b),
            Self::NotEqualExp((a, b)) => write!(f, "({}) != ({})", a, b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LAndExp {
    EqExp(EqExp),
    AndExp((Box<LAndExp>, EqExp)),
}

impl LAndExp {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(LAndExp::EqExp(EqExp::arbitrary(u, c)?)),
            1 => {
                let a = LAndExp::arbitrary(u, c)?;
                let b = EqExp::arbitrary(u, c)?;
                Ok(LAndExp::AndExp((Box::new(a), b)))
            }
            _ => unreachable!(),
        }
    }
}

impl Display for LAndExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EqExp(a) => write!(f, "{}", a),
            Self::AndExp((a, b)) => write!(f, "({}) && ({})", a, b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LOrExp {
    LAndExp(LAndExp),
    OrExp((Box<LOrExp>, LAndExp)),
}

impl LOrExp {
    fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        match u.arbitrary::<u8>()? % 2 {
            0 => Ok(LOrExp::LAndExp(LAndExp::arbitrary(u, c)?)),
            1 => {
                let a = LOrExp::arbitrary(u, c)?;
                let b = LAndExp::arbitrary(u, c)?;
                Ok(LOrExp::OrExp((Box::new(a), b)))
            }
            _ => unreachable!(),
        }
    }
}

impl Display for LOrExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LAndExp(a) => write!(f, "{}", a),
            Self::OrExp((a, b)) => write!(f, "({}) || ({})", a, b),
        }
    }
}

/// Positive vector, element count >= 1
#[derive(Debug, Clone)]
pub struct PVec<T>(Vec<T>);

impl<T> PVec<T> {
    pub fn iter(&self) -> core::slice::Iter<T> {
        self.0.iter()
    }
}

impl<'a, T> Arbitrary<'a> for PVec<T>
where
    T: Arbitrary<'a>,
{
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let mut v = Vec::<T>::arbitrary(u)?;
        let t = T::arbitrary(u)?;
        v.push(t);
        Ok(PVec(v))
    }
}

/// Identifier, character is limited
#[derive(Debug, Clone, Arbitrary)]
pub struct Ident(PVec<IdentChar>);

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id: Vec<u8> = self.0.iter().map(|x| x.0).collect();
        write!(f, "{}", String::from_utf8_lossy(&id))
    }
}

/// Character that can appear in an identifier
/// TODO: add support for number
#[derive(Debug, Clone)]
pub struct IdentChar(u8);

impl<'a> Arbitrary<'a> for IdentChar {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let mut buffer = [0; 1];
        u.fill_buffer(&mut buffer)?;
        let norm = buffer[0] % 53;
        if norm < 26 {
            // 'A'..='Z'
            Ok(IdentChar(norm + 65))
        } else if norm < 52 {
            // 'a'..='z'
            Ok(IdentChar(norm - 26 + 97))
        } else {
            // '_'
            Ok(IdentChar(95))
        }
    }
}

pub type IntConst = i32;
pub type FloatConst = f32;
