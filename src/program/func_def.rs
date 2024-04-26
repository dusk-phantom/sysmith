use super::*;

#[derive(Debug, Clone)]
pub enum FuncDef {
    ParameterFuncDef((FuncType, Ident, FuncFParams, Block)),
}

impl Resolve for FuncDef {
    fn resolve(&self, ctx: &mut Context) {
        match self {
            FuncDef::ParameterFuncDef((a, b, c, _)) => {
                ctx.ctx.insert(b.to_string(), Type::Func(
                    Box::new(a.clone().into()), 
                    c.func_fparams_vec.iter().map(|x| x.to_type(ctx)).collect(),
                ));
            }
        }
    }
}

impl<'a> ArbitraryIn<'a, Context> for FuncDef {
    fn arbitrary(u: &mut Unstructured<'a>, ctx: &Context) -> Result<Self> {
        // Generate a random function signature
        let func_type = FuncType::arbitrary(u)?;
        let ident = Ident::arbitrary(u)?;
        let func_fparams = FuncFParams::arbitrary(u, ctx)?;

        // Initialize a context expecting return type `func_type`
        let mut ctx = ctx.clone();
        ctx.return_type = func_type.clone().into();

        // Generate function statements with return type specified
        let block = Block::arbitrary(u, &ctx)?;
        Ok(FuncDef::ParameterFuncDef((
            func_type,
            ident,
            func_fparams,
            block,
        )))
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
pub struct FuncFParams {
    pub func_fparams_vec: Vec<FuncFParam>,
}

impl<'a> ArbitraryIn<'a, Context> for FuncFParams {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        let mut func_fparams_vec = Vec::new();
        for _ in 0..MAX_VEC_LEN {
            // Generate zero or more function params
            if u.arbitrary()? {
                break;
            }
            let func_fparam = FuncFParam::arbitrary(u, c)?;
            func_fparams_vec.push(func_fparam);
        }
        Ok(FuncFParams { func_fparams_vec })
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
    /// Convert a function parameter to a type
    fn to_type(&self, c: &Context) -> Type {
        match self {
            FuncFParam::NonArray((a, _)) => a.clone().into(),
            // FIXME: this is a pointer
            FuncFParam::Array((a, _, b)) => b.apply(a.clone().into(), c),
        }
    }
}

impl<'a> ArbitraryIn<'a, Context> for FuncFParam {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
        // Generate array (x: int[][4]) or non-array (x: int) function parameter
        match u.int_in_range(0..=1)? {
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
