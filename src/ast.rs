use std::fmt::Display;

use libfuzzer_sys::arbitrary::{self, Unstructured};
use libfuzzer_sys::arbitrary::{Arbitrary, Result};

#[derive(Debug, Arbitrary)]
pub struct CompUnit {
    pub global_items: Vec<GlobalItems>,
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

#[derive(Debug, Arbitrary)]
pub enum GlobalItems {
    Decl(Decl),
    FuncDef(FuncDef),
}

impl Display for GlobalItems {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlobalItems::Decl(a) => write!(f, "{}", a),
            GlobalItems::FuncDef(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::ConstDecl(a) => write!(f, "{}", a),
            Decl::VarDecl(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub struct ConstDecl {
    pub btype: BType,
    pub const_def_vec: PVec<ConstDef>,
}

impl Display for ConstDecl {
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

#[derive(Debug, Arbitrary)]
pub enum BType {
    Int,
    Float,
}

impl Display for BType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BType::Int => write!(f, "int"),
            BType::Float => write!(f, "float"),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub struct ConstDef {
    pub ident: Ident,
    pub const_exp_vec: Vec<ConstExp>,
    pub const_init_val: ConstInitVal,
}

impl Display for ConstDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{} = {}",
            self.ident,
            self.const_exp_vec
                .iter()
                .map(|x| format!("[{}]", x))
                .collect::<Vec<String>>()
                .join(""),
            self.const_init_val,
        )
    }
}

#[derive(Debug, Arbitrary)]
pub enum ConstInitVal {
    ConstExp(ConstExp),
    ConstInitValVec(Vec<ConstInitVal>),
}

impl Display for ConstInitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstInitVal::ConstExp(a) => write!(f, "{}", a),
            ConstInitVal::ConstInitValVec(a) => write!(
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

#[derive(Debug, Arbitrary)]
pub struct VarDecl {
    pub btype: BType,
    pub var_def_vec: PVec<VarDef>,
}

impl Display for VarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {};",
            self.btype,
            self.var_def_vec
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Arbitrary)]
pub enum VarDef {
    Array((Ident, Vec<ConstExp>)),
    ArrayInit((Ident, Vec<ConstExp>, InitVal)),
}

impl Display for VarDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarDef::Array((a, b)) => write!(
                f,
                "{}{}",
                a,
                b.iter()
                    .map(|x| format!("[{}]", x))
                    .collect::<Vec<String>>()
                    .join("")
            ),
            VarDef::ArrayInit((a, b, c)) => write!(
                f,
                "{}{} = {}",
                a,
                b.iter()
                    .map(|x| format!("[{}]", x))
                    .collect::<Vec<String>>()
                    .join(""),
                c,
            ),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub enum InitVal {
    Exp(Exp),
    InitValVec(Vec<InitVal>),
}

impl Display for InitVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InitVal::Exp(e) => write!(f, "{}", e),
            InitVal::InitValVec(e) => write!(
                f,
                "{{ {} }}",
                e.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub enum FuncDef {
    NonParameterFuncDef((FuncType, Ident, Block)),
    ParameterFuncDef((FuncType, Ident, FuncFParams, Block)),
}

impl Display for FuncDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncDef::NonParameterFuncDef((a, b, c)) => write!(f, "{} {}() {}", a, b, c),
            FuncDef::ParameterFuncDef((a, b, c, d)) => write!(f, "{} {}({}) {}", a, b, c, d),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub enum FuncType {
    Void,
    Int,
    Float,
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

#[derive(Debug, Arbitrary)]
pub struct FuncFParams {
    pub func_fparams_vec: Vec<FuncFParam>,
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

#[derive(Debug, Arbitrary)]
pub enum FuncFParam {
    NonArray((BType, Ident)),
    Array((BType, Ident, Vec<Exp>)),
}

impl Display for FuncFParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncFParam::NonArray((a, b)) => write!(f, "{} {}", a, b),
            FuncFParam::Array((a, b, c)) => {
                write!(
                    f,
                    "{} {}[]{}",
                    a,
                    b,
                    c.iter()
                        .map(|x| format!("[{}]", x))
                        .collect::<Vec<_>>()
                        .join("")
                )
            }
        }
    }
}

#[derive(Debug, Arbitrary)]
pub struct Block {
    pub block_vec: Vec<BlockItem>,
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

#[derive(Debug, Arbitrary)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

impl Display for BlockItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockItem::Decl(a) => write!(f, "{}", a),
            BlockItem::Stmt(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Arbitrary)]
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

#[derive(Debug, Arbitrary)]
pub struct Assign {
    pub lval: LVal,
    pub exp: Exp,
}

impl Display for Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {};", self.lval, self.exp)
    }
}

#[derive(Debug, Arbitrary)]
pub struct ExpStmt {
    pub exp: Option<Exp>,
}

impl Display for ExpStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.exp {
            Some(e) => write!(f, "{};", e),
            None => write!(f, ";"),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub struct If {
    pub cond: Cond,
    pub then: Stmt,
    pub else_then: Option<Stmt>,
}

impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.else_then {
            Some(e) => write!(f, "if ({}) {} else {}", self.cond, self.then, e),
            None => write!(f, "if ({}) {}", self.cond, self.then),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub struct While {
    pub cond: Cond,
    pub body: Stmt,
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {}", self.cond, self.body)
    }
}

#[derive(Debug, Arbitrary)]
pub struct Break;

impl Display for Break {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "break;")
    }
}

#[derive(Debug, Arbitrary)]
pub struct Continue;

impl Display for Continue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "continue;")
    }
}

#[derive(Debug, Arbitrary)]
pub struct Return {
    pub exp: Option<Exp>,
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.exp {
            Some(a) => write!(f, "return {};", a),
            None => write!(f, "return;"),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub struct Exp {
    pub add_exp: Box<AddExp>,
}

impl Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.add_exp)
    }
}

#[derive(Debug, Arbitrary)]
pub struct Cond {
    pub l_or_exp: LOrExp,
}

impl Display for Cond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.l_or_exp)
    }
}

#[derive(Debug, Arbitrary)]
pub struct LVal {
    pub id: Ident,
    pub exp_vec: Vec<Exp>,
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

#[derive(Debug, Arbitrary)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(Number),
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

#[derive(Debug, Arbitrary)]
pub enum Number {
    IntConst(IntConst),
    FloatConst(FloatConst),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::IntConst(a) => write!(f, "{}", a),
            Number::FloatConst(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    FuncCall((Ident, Option<FuncRParams>)),
    OpUnary((UnaryOp, Box<UnaryExp>)),
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

#[derive(Debug, Arbitrary)]
pub enum UnaryOp {
    Add,
    Minus,
    Exclamation,
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

#[derive(Debug, Arbitrary)]
pub struct FuncRParams {
    pub exp_vec: Vec<Exp>,
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

#[derive(Debug, Arbitrary)]
pub enum MulExp {
    UnaryExp(Box<UnaryExp>),
    MulExp((Box<MulExp>, UnaryExp)),
    DivExp((Box<MulExp>, UnaryExp)),
    ModExp((Box<MulExp>, UnaryExp)),
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

#[derive(Debug, Arbitrary)]
pub enum AddOp {
    Add,
    Minus,
}

impl Display for AddOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddOp::Add => write!(f, "+"),
            AddOp::Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub enum AddExp {
    MulExp(Box<MulExp>),
    OpExp((Box<AddExp>, AddOp, MulExp)),
}

impl Display for AddExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MulExp(a) => write!(f, "{}", a),
            Self::OpExp((a, b, c)) => write!(f, "({}) {} ({})", a, b, c),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub enum RelOp {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
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

#[derive(Debug, Arbitrary)]
pub enum RelExp {
    AddExp(AddExp),
    OpExp((Box<RelExp>, RelOp, AddExp)),
}

impl Display for RelExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AddExp(a) => write!(f, "{}", a),
            Self::OpExp((a, b, c)) => write!(f, "({}) {} ({})", a, b, c),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub enum EqExp {
    RelExp(RelExp),
    EqualExp((Box<EqExp>, RelExp)),
    NotEqualExp((Box<EqExp>, RelExp)),
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

#[derive(Debug, Arbitrary)]
pub enum LAndExp {
    EqExp(EqExp),
    AndExp((Box<LAndExp>, EqExp)),
}

impl Display for LAndExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EqExp(a) => write!(f, "{}", a),
            Self::AndExp((a, b)) => write!(f, "({}) && ({})", a, b),
        }
    }
}

#[derive(Debug, Arbitrary)]
pub struct ConstExp {
    pub add_exp: AddExp,
}

impl Display for ConstExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.add_exp)
    }
}

#[derive(Debug, Arbitrary)]
pub enum LOrExp {
    LAndExp(LAndExp),
    OrExp((Box<LOrExp>, LAndExp)),
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
#[derive(Debug)]
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
#[derive(Debug, Arbitrary)]
pub struct Ident(PVec<IdentChar>);

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id: Vec<u8> = self.0.iter().map(|x| x.0).collect();
        write!(f, "{}", String::from_utf8_lossy(&id))
    }
}

/// Character that can appear in an identifier
/// TODO: add support for number
#[derive(Debug)]
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
