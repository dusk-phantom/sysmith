use super::*;

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

impl<'a> ArbitraryInContext<'a> for GlobalItems {
    fn arbitrary(u: &mut Unstructured<'a>, c: &Context) -> Result<Self> {
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
