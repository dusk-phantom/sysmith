use super::*;

#[derive(Debug, Clone)]
pub struct Index(pub Vec<Exp>);

impl<'a> ArbitraryTo<'a, Index> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Index> {
        let mut index = Vec::new();
        for _ in 0..MAX_VEC_LEN {
            // Create zero or more array indicies
            if u.arbitrary()? {
                break;
            }

            // Generate a random integer
            let mut c = self.clone();
            c.expected = ExpectedType {
                is_const: true,
                value_type: Type::Int,
                bound: IntBound::new(0, MAX_ARR_LEN),
            };
            let exp = c.arbitrary(u)?;
            index.push(exp);
        }
        Ok(Index(index))
    }
}

impl Index {
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
