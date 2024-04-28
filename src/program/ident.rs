use super::*;

/// Positive vector, element count >= 1
#[derive(Debug, Clone)]
pub struct PVec<T>(pub Vec<T>);

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
#[derive(Debug, Clone)]
pub struct Ident(pub Vec<IdentChar>);

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(s
            .chars()
            .map(|c| IdentChar(c as u8))
            .collect::<Vec<IdentChar>>())
    }
}

impl<'a> ArbitraryTo<'a, Ident> for Context {
    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Ident> {
        let mut ident = Ident(Vec::new());

        // Generate at least one character
        for _ in 0..MAX_VEC_LEN {
            let c = IdentChar::arbitrary(u)?;
            ident.0.push(c);
            if u.arbitrary()? {
                break;
            }
        }

        // While identifier exists in context, add another random character
        while self.ctx.contains_key(&ident.to_string()) {
            let c = IdentChar::arbitrary(u)?;
            ident.0.push(c);
        }
        Ok(ident)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id: Vec<u8> = self.0.iter().map(|x| x.0).collect();
        write!(f, "{}", String::from_utf8_lossy(&id))
    }
}

/// Character that can appear in an identifier
/// TODO add support for number
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
