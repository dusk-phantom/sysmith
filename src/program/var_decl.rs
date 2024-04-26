use super::*;

#[derive(Debug, Clone)]
pub enum Decl {
    VarDecl(VarDecl),
}

impl Decl {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
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
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // Generate a random basic type and constant flag
        let btype = BType::arbitrary(u)?;
        let is_const = u.arbitrary()?;

        // Initialize context
        let mut c = c.clone();
        c.expected_type = btype.clone().into();
        c.expected_const = is_const;

        // Generate at lease one definition
        let mut const_def_vec = Vec::new();
        loop {
            let const_def = VarDef::arbitrary(u, &c)?;
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
pub struct VarDef {
    pub ident: Ident,
    pub index: Index,
    pub init_val: VarInitVal,
}

impl VarDef {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
        // Generate a random identifier for this definition
        let ident = Ident::arbitrary(u)?;

        // Generate random array type
        let index = Index::arbitrary(u, c)?;
        let var_type = index.apply(c.expected_type.clone(), c);

        // Generate assigned value in context with expected type revised
        let mut c: Context = c.clone();
        c.expected_type = var_type.clone();
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
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
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

    pub fn eval(&self, ctx: &Context) -> Value {
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