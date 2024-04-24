use super::*;

/// A type.
/// Example: *int
#[derive(Clone, PartialEq, Debug, Arbitrary)]
pub enum Type {
    /// Nothing. Can only be function return type.
    Void,

    /// 32-bit integer.
    Int32,

    /// 32-bit floating-point number.
    Float32,

    /// String.
    String,

    /// Character.
    Char,

    /// Boolean.
    Boolean,

    /// Pointer to given type.
    /// Example:
    /// `int *` is `Pointer(Int32)`
    Pointer(Box<Type>),

    /// Array of given type.
    /// Example:
    /// `int x[4]` is `Array(Int32, 4)`
    Array(Box<Type>, usize),

    /// Function to given type.
    /// Example:
    /// `void (*x)(int)` is `Pointer(Function(Void, [Int32]))`
    Function(Box<Type>, Vec<TypedIdent>),

    /// Enum of given name.
    /// Example:
    /// `enum fruits` is `Enum("fruits")`
    Enum(String),

    /// Union of given name.
    /// Example:
    /// `union numbers` is `Union("numbers")`
    Union(String),

    /// Struct of given name.
    /// Example:
    /// `struct numbers` is `Struct("numbers")`
    Struct(String),
}

/// A left value is an identifier with usage of its type.
/// If identifier is not null, it can be assigned to.
/// Example: `(*f)(int)` indicates that `f` should be used as `(*f)(some_int)`.
#[derive(Clone, PartialEq, Debug, Arbitrary)]
pub enum LVal {
    /// Nothing.
    /// Used when there's no target of usage.
    /// Example: `*(int)` has core `Nothing`.
    Nothing,

    /// A single variable.
    /// Example: `x`
    Var(String),

    /// Array indexing.
    /// Example: `x[8]`
    Index(Box<LVal>, usize),

    /// A function call.
    /// Example: `f(x, y)`
    Call(Box<LVal>, Vec<TypedIdent>),

    /// Application of indirection.
    /// Example: `*x`
    Pointer(Box<LVal>),
}

/// A typed identifier.
/// `ty`: type
/// `id`: identifier name
/// Example: `int *x` is `{ ty: Pointer(Int32), id: "x" }`
#[derive(Clone, PartialEq, Debug, Arbitrary)]
pub struct TypedIdent {
    pub ty: Type,
    pub id: Option<String>,
}
