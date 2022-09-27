//! Common utilities

#[macro_export]
macro_rules! define_enum {
    ( $( #[ $meta:meta ] )* $ty:ident = $( $variant:ident )|* ; ) => {
        $( #[$meta] )*
        pub enum $ty {
            $( $variant($variant), )*
        }

        $(
            impl From<$variant> for $ty {
                fn from(x: $variant) -> $ty {
                    Self::$variant(x)
                }
            }
        )*
    }
}

pub use define_enum;

#[macro_export]
macro_rules! enum_impl_from {
    ( $ty:ty = $( $ty_from:ident )|* ; ) => {
        $(
            impl From<$ty_from> for $ty {
                fn from(x: $ty_from) -> $ty {
                    Self::$ty_from(x)
                }
            }
        )*
    }
}

pub use enum_impl_from;
