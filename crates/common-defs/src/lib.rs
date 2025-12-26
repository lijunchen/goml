#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Neg => "-",
            Self::Not => "!",
        }
    }

    pub fn method_name(self) -> &'static str {
        match self {
            Self::Neg => "neg",
            Self::Not => "not",
        }
    }
}
