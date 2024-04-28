use super::*;

#[derive(Debug, Clone)]
pub struct Index(pub Vec<Exp>);

impl<'a> ArbitraryTo<'a, Index> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Index> {
        let mut index = Vec::new();
        // Create zero or more array indicies
        for _ in 0..MAX_VEC_LEN {
            if u.arbitrary()? {
                break;
            }

            // Generate a random integer as dimension size
            let mut c = self.clone();
            c.expected = ExpectedType {
                is_const: true,
                value_type: Type::Int,
                bound: NumBound::new(0, MAX_ARR_LEN),
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
            let i = exp.eval(c).as_int();
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
