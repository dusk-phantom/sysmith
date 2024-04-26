use super::*;

#[derive(Debug, Clone)]
pub struct Index(Vec<Exp>);

impl Index {
    pub fn arbitrary(u: &mut Unstructured, c: &Context) -> Result<Self> {
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

    pub fn apply(&self, mut base_type: Type, c: &Context) -> Type {
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