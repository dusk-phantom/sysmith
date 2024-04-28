use super::*;

#[derive(Debug, Clone)]
pub struct CompUnit {
    pub global_items: Vec<GlobalItems>,
}

impl<'a> Arbitrary<'a> for CompUnit {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let mut global_items = Vec::new();

        // Initialize with empty context
        let mut context = Context {
            ctx: HashMap::new(),
            env: HashMap::new(),
            expected: ExpectedType {
                is_const: false,
                value_type: Type::Void,
                bound: None,
            },
            return_type: Type::Void,
            in_loop: false,
            depth: 0,
        };

        // Generate at least one global item
        for _ in 0..MAX_VEC_LEN {
            let item: GlobalItems = context.arbitrary(u)?;
            item.resolve(&mut context);
            global_items.push(item);
            if u.arbitrary()? {
                break;
            }
        }
        Ok(CompUnit { global_items })
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
    Decl(VarDecl),
    FuncDef(FuncDef),
}

impl Resolve for GlobalItems {
    fn resolve(&self, ctx: &mut Context) {
        match self {
            GlobalItems::Decl(a) => a.resolve(ctx),
            GlobalItems::FuncDef(a) => a.resolve(ctx),
        }
    }
}

impl<'a> ArbitraryTo<'a, GlobalItems> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<GlobalItems> {
        // Generate variable or function at random
        match u.int_in_range(0..=1)? {
            0 => Ok(GlobalItems::Decl(self.arbitrary(u)?)),
            1 => Ok(GlobalItems::FuncDef(self.arbitrary(u)?)),
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
