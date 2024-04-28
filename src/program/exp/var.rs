use super::*;

/// SingleVarContext generates an arbitrary expression
/// from a given root variable in the context.
/// Example: `f(1)` when id is `f`.
#[derive(Clone, Debug)]
pub struct SingleVarContext<'a> {
    ctx: &'a Context,
    id: Ident,
    ty: Type,
}

impl<'a> ArbitraryTo<'a, Exp> for SingleVarContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        // If expected constant, make sure variable is constant
        if self.ctx.expected.is_const && !self.ctx.env.contains_key(&self.id.to_string()) {
            return false;
        }

        // If type match, using this variable is OK
        if self.ty == self.ctx.expected.value_type {
            return true;
        }

        // If type is function, check if return type matches
        // If matches, check if argument can be filled
        if let Type::Func(ret_type, param) = &self.ty {
            if **ret_type == self.ctx.expected.value_type {
                let mut can_arbitrary = true;
                for x in param.iter() {
                    let mut c = self.ctx.clone();
                    c.expected = ExpectedType {
                        is_const: c.expected.is_const,
                        value_type: x.clone(),
                        bound: NumBound::None,
                    };
                    if !c.can_arbitrary(PhantomData::<Exp>) {
                        can_arbitrary = false;
                        break;
                    }
                }
                if can_arbitrary {
                    return true;
                }
            }
        }

        // If type is array, recursively check if content type matches
        // until content type is no longer an array
        let mut current_type = &self.ty;
        while let Type::Array(content_type, len) = current_type {
            // If array length is 0, it contains nothing
            if *len == 0 {
                return false;
            }

            // Otherwise check if content type matches
            current_type = content_type;
            if *current_type == self.ctx.expected.value_type {
                return true;
            }
        }

        // No variable found in context
        false
    }

    fn arbitrary(&self, u: &mut Unstructured) -> Result<Exp> {
        let c: &Context = self.ctx;
        let id = &self.id;
        let ty = &self.ty;
        let id = id.clone();

        // If type match, directly add as candidate
        if *ty == c.expected.value_type {
            return Ok(Exp::LVal(LVal {
                id,
                index: Index(Vec::new()),
            }));
        }

        // If type is function, check if return type matches
        // If matches, add as candidate
        if let Type::Func(ret_type, param) = ty {
            if **ret_type == c.expected.value_type {
                // Map parameters to arbitrary instances of its type
                return param
                    .iter()
                    .map(|x| {
                        let mut c = c.clone();
                        c.expected = ExpectedType {
                            is_const: c.expected.is_const,
                            value_type: x.clone(),
                            bound: NumBound::None,
                        };
                        c.arbitrary(u)
                    })
                    .collect::<Result<_, _>>()
                    .map(|exp_vec| Exp::FuncCall((id, FuncRParams { exp_vec })));
            }
        }

        // If type is array, recursively check if content type matches
        // until content type is no longer an array
        let mut current_type = ty;
        let mut current_exp = LVal {
            id: id.clone(),
            index: Index(Vec::new()),
        };
        while let Type::Array(content_type, len) = current_type {
            // Generate a random index in bound
            let mut c = self.ctx.clone();
            c.expected = ExpectedType {
                is_const: c.expected.is_const,
                value_type: Type::Int,
                bound: NumBound::new(0, len - 1),
            };
            let exp = c.arbitrary(u)?;

            // Update current processing type and expression
            current_type = content_type;
            current_exp.index.0.push(exp);
            if *current_type == c.expected.value_type {
                return Ok(Exp::LVal(current_exp));
            }
        }

        // Otherwise, it's impossible for this identifier to transform
        // to the expected type
        panic!("impossible to construct variable; you should check can_arbitrary first");
    }
}

/// VarContext generates an arbitrary expression
/// from an arbitrary root variable in the context.
/// Example: `f(1)` when `f` is in context
#[derive(Clone, Debug)]
pub struct VarContext<'a>(pub &'a Context);

impl<'a> ArbitraryTo<'a, Exp> for VarContext<'_> {
    fn can_arbitrary(&self, _: PhantomData<Exp>) -> bool {
        let contexts: Vec<_> = self
            .0
            .ctx
            .iter()
            .map(|(id, ty)| {
                Box::new(SingleVarContext {
                    ctx: self.0,
                    id: id.clone().into(),
                    ty: ty.clone(),
                }) as Box<dyn ArbitraryTo<Exp>>
            })
            .collect();
        can_arbitrary_any(contexts.as_slice())
    }

    fn arbitrary(&self, u: &mut Unstructured<'a>) -> Result<Exp> {
        let contexts: Vec<_> = self
            .0
            .ctx
            .iter()
            .map(|(id, ty)| {
                Box::new(SingleVarContext {
                    ctx: self.0,
                    id: id.clone().into(),
                    ty: ty.clone(),
                }) as Box<dyn ArbitraryTo<Exp>>
            })
            .collect();
        arbitrary_any(u, contexts.as_slice())
    }
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub id: Ident,
    pub index: Index,
}

impl Eval for LVal {
    fn eval(&self, ctx: &Context) -> Value {
        let mut array_type = ctx.ctx.get(&self.id.to_string()).unwrap().clone();
        let mut array_value = ctx.env.get(&self.id.to_string()).unwrap().clone();
        for exp in &self.index.0 {
            let index_value: Value = exp.eval(ctx);
            match (array_type, array_value) {
                (Type::Array(content_type, content_len), Value::Array(content_value)) => {
                    let i = index_value.as_int();
                    if i < 0 || i >= content_len {
                        panic!("Index out of range")
                    }
                    array_type = *content_type;
                    array_value = content_value[i as usize].clone();
                }
                _ => panic!("Not an array"),
            }
        }
        array_value
    }
}

impl Display for LVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.id, self.index,)
    }
}

/// Actual argument for function
#[derive(Debug, Clone)]
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
