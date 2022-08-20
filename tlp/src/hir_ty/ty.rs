//! Type information

/// Interned type information
pub type Ty = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    // UntypedInt,
    I32,
    F32,
}
