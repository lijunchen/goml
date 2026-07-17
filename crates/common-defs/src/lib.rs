#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    And,
    Or,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Eq,
    NotEq,
}

impl BinaryOp {
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::Shl => "<<",
            Self::Shr => ">>",
            Self::And => "&&",
            Self::Or => "||",
            Self::Less => "<",
            Self::Greater => ">",
            Self::LessEq => "<=",
            Self::GreaterEq => ">=",
            Self::Eq => "==",
            Self::NotEq => "!=",
        }
    }

    pub fn method_name(self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::Rem => "rem",
            Self::BitAnd => "bit_and",
            Self::BitOr => "bit_or",
            Self::BitXor => "bit_xor",
            Self::Shl => "shl",
            Self::Shr => "shr",
            Self::And => "and",
            Self::Or => "or",
            Self::Less => "less",
            Self::Greater => "greater",
            Self::LessEq => "less_eq",
            Self::GreaterEq => "greater_eq",
            Self::Eq => "eq",
            Self::NotEq => "not_eq",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
}

impl UnaryOp {
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Neg => "-",
            Self::Not => "!",
            Self::BitNot => "~",
        }
    }

    pub fn method_name(self) -> &'static str {
        match self {
            Self::Neg => "neg",
            Self::Not => "not",
            Self::BitNot => "bit_not",
        }
    }
}
