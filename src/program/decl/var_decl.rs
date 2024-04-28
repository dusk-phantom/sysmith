use super::*;

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub is_const: bool,
    pub btype: BType,
    pub def_vec: Vec<VarDef>,
}

impl Resolve for VarDecl {
    fn resolve(&self, ctx: &mut Context) {
        for def in self.def_vec.iter() {
            // Add to context
            let var_type = def.index.apply(self.btype.clone().into(), ctx);
            ctx.ctx.insert(def.ident.to_string(), var_type);

            // Add to env if constant
            if self.is_const {
                let val = def.init_val.eval(ctx);
                ctx.env.insert(def.ident.to_string(), val);
            }
        }
    }
}

impl<'a> ArbitraryTo<'a, VarDecl> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<VarDecl> {
        // Generate a random basic type and constant flag
        let btype = BType::arbitrary(u)?;
        let is_const = u.arbitrary()?;

        // Initialize context
        let mut c = self.clone();
        c.expected = ExpectedType {
            is_const,
            value_type: btype.clone().into(),
            bound: NumBound::None,
        };

        // Generate at lease one definition
        let mut def_vec = Vec::new();
        for _ in 0..MAX_VEC_LEN {
            let def = c.arbitrary(u)?;
            def_vec.push(def);
            if u.arbitrary()? {
                break;
            }
        }
        Ok(VarDecl {
            is_const,
            btype,
            def_vec,
        })
    }
}

impl Display for VarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{} {};",
            if self.is_const { "const " } else { "" },
            self.btype,
            self.def_vec
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub ident: Ident,
    pub index: Index,
    pub init_val: VarInitVal,
}

impl<'a> ArbitraryTo<'a, VarDef> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<VarDef> {
        // Generate a random identifier for this definition
        let ident = self.arbitrary(u)?;

        // Generate random array type
        let index: Index = self.arbitrary(u)?;
        let var_type = index.apply(self.expected.value_type.clone(), self);

        // Generate assigned value in context with expected type revised
        let mut c: Context = self.clone();
        c.expected = ExpectedType {
            is_const: c.expected.is_const,
            value_type: var_type.clone(),
            bound: NumBound::None,
        };
        let init_val = c.arbitrary(u)?;

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
        write!(f, "{}{} = {}", self.ident, self.index, self.init_val,)
    }
}

#[derive(Debug, Clone)]
pub enum VarInitVal {
    Exp(Exp),
    InitValVec(Vec<VarInitVal>),
}

impl<'a> ArbitraryTo<'a, VarInitVal> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<VarInitVal> {
        match self.expected.value_type.clone() {
            Type::Int | Type::Float => {
                // Random integer or float
                Ok(VarInitVal::Exp(self.arbitrary(u)?))
            }
            Type::Array(content_type, content_len) => {
                // Initialize a context expecting `content_type`
                // Constant flag inherits from parent context
                let mut c = self.clone();
                c.expected = ExpectedType {
                    is_const: c.expected.is_const,
                    value_type: *content_type.clone(),
                    bound: NumBound::None,
                };

                // Fill array with random contents
                let mut init_val_vec = Vec::new();
                for _ in 0..content_len {
                    let init_val = c.arbitrary(u)?;
                    init_val_vec.push(init_val);
                }
                Ok(VarInitVal::InitValVec(init_val_vec))
            }
            _ => panic!("Invalid type"),
        }
    }
}

impl Eval for VarInitVal {
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
