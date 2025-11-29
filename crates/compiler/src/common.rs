use ast::ast::Ident;

#[derive(Debug, Clone)]
pub enum Prim {
    Unit { value: () },
    Bool { value: bool },
    Int8 { value: i8 },
    Int16 { value: i16 },
    Int32 { value: i32 },
    Int64 { value: i64 },
    UInt8 { value: u8 },
    UInt16 { value: u16 },
    UInt32 { value: u32 },
    UInt64 { value: u64 },
    Float32 { value: f32 },
    Float64 { value: f64 },
    String { value: String },
}

impl std::fmt::Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prim::Unit { .. } => write!(f, "()"),
            Prim::Bool { value } => write!(f, "{}", value),
            Prim::Int8 { value } => write!(f, "{}", value),
            Prim::Int16 { value } => write!(f, "{}", value),
            Prim::Int32 { value } => write!(f, "{}", value),
            Prim::Int64 { value } => write!(f, "{}", value),
            Prim::UInt8 { value } => write!(f, "{}", value),
            Prim::UInt16 { value } => write!(f, "{}", value),
            Prim::UInt32 { value } => write!(f, "{}", value),
            Prim::UInt64 { value } => write!(f, "{}", value),
            Prim::Float32 { value } => write!(f, "{}", value),
            Prim::Float64 { value } => write!(f, "{}", value),
            Prim::String { value } => write!(f, "{:?}", value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumConstructor {
    pub type_name: Ident,
    pub variant: Ident,
    pub index: usize,
}

impl EnumConstructor {
    pub fn enum_index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, Clone)]
pub struct StructConstructor {
    pub type_name: Ident,
}

#[derive(Debug, Clone)]
pub enum Constructor {
    Enum(EnumConstructor),
    Struct(StructConstructor),
}

impl Constructor {
    pub fn name(&self) -> &Ident {
        match self {
            Constructor::Enum(constructor) => &constructor.variant,
            Constructor::Struct(constructor) => &constructor.type_name,
        }
    }

    pub fn type_name(&self) -> &Ident {
        match self {
            Constructor::Enum(constructor) => &constructor.type_name,
            Constructor::Struct(constructor) => &constructor.type_name,
        }
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Constructor::Struct(_))
    }

    pub fn as_enum(&self) -> Option<&EnumConstructor> {
        if let Constructor::Enum(constructor) = self {
            Some(constructor)
        } else {
            None
        }
    }

    pub fn as_struct(&self) -> Option<&StructConstructor> {
        if let Constructor::Struct(constructor) = self {
            Some(constructor)
        } else {
            None
        }
    }
}
