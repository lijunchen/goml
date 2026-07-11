use std::num::IntErrorKind;

use parser::Diagnostics;
use text_size::TextRange;

use crate::{
    common::Prim,
    tast::{self},
};

use super::util::push_error_with_range;

pub(crate) fn parse_integer_literal_with_ty(
    diagnostics: &mut Diagnostics,
    literal: &str,
    ty: &tast::Ty,
    range: Option<TextRange>,
) -> Option<Prim> {
    match ty {
        tast::Ty::TInt8 => parse_signed_integer(diagnostics, literal, "int8", range)
            .map(|value| Prim::Int8 { value }),
        tast::Ty::TInt16 => parse_signed_integer(diagnostics, literal, "int16", range)
            .map(|value| Prim::Int16 { value }),
        tast::Ty::TInt32 => parse_signed_integer(diagnostics, literal, "int32", range)
            .map(|value| Prim::Int32 { value }),
        tast::Ty::TInt64 => parse_signed_integer(diagnostics, literal, "int64", range)
            .map(|value| Prim::Int64 { value }),
        tast::Ty::TUint8 => parse_unsigned_integer(diagnostics, literal, "uint8", range)
            .map(|value| Prim::UInt8 { value }),
        tast::Ty::TUint16 => parse_unsigned_integer(diagnostics, literal, "uint16", range)
            .map(|value| Prim::UInt16 { value }),
        tast::Ty::TUint32 => parse_unsigned_integer(diagnostics, literal, "uint32", range)
            .map(|value| Prim::UInt32 { value }),
        tast::Ty::TUint64 => parse_unsigned_integer(diagnostics, literal, "uint64", range)
            .map(|value| Prim::UInt64 { value }),
        _ => None,
    }
}

pub(crate) fn parse_char_literal(
    diagnostics: &mut Diagnostics,
    literal: &str,
    range: Option<TextRange>,
) -> Option<char> {
    if literal.is_empty() {
        push_error_with_range(diagnostics, "Char literal cannot be empty", range);
        return None;
    }

    if let Some(rest) = literal.strip_prefix('\\') {
        let mut chars = rest.chars();
        let Some(tag) = chars.next() else {
            report_invalid_char_literal(diagnostics, literal, range);
            return None;
        };

        let out = match tag {
            '\'' => Some('\''),
            '"' => Some('"'),
            '\\' => Some('\\'),
            '/' => Some('/'),
            'b' => Some('\u{0008}'),
            'f' => Some('\u{000C}'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            'u' => {
                let hex = chars.by_ref().take(4).collect::<String>();
                if hex.chars().count() != 4 || chars.next().is_some() {
                    None
                } else if let Ok(code) = u32::from_str_radix(&hex, 16) {
                    char::from_u32(code)
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(ch) = out
            && chars.next().is_none()
        {
            return Some(ch);
        }

        report_invalid_char_literal(diagnostics, literal, range);
        return None;
    }

    let mut chars = literal.chars();
    match (chars.next(), chars.next()) {
        (Some(ch), None) => Some(ch),
        _ => {
            report_invalid_char_literal(diagnostics, literal, range);
            None
        }
    }
}

fn parse_signed_integer<T>(
    diagnostics: &mut Diagnostics,
    literal: &str,
    ty_name: &str,
    range: Option<TextRange>,
) -> Option<T>
where
    T: std::str::FromStr<Err = std::num::ParseIntError>,
{
    match literal.parse::<T>() {
        Ok(value) => Some(value),
        Err(err) => {
            report_integer_parse_error(diagnostics, literal, ty_name, err.kind(), range);
            None
        }
    }
}

fn parse_unsigned_integer<T>(
    diagnostics: &mut Diagnostics,
    literal: &str,
    ty_name: &str,
    range: Option<TextRange>,
) -> Option<T>
where
    T: std::str::FromStr<Err = std::num::ParseIntError>,
{
    if literal.starts_with('-') {
        report_integer_overflow(diagnostics, literal, ty_name, range);
        return None;
    }

    match literal.parse::<T>() {
        Ok(value) => Some(value),
        Err(err) => {
            report_integer_parse_error(diagnostics, literal, ty_name, err.kind(), range);
            None
        }
    }
}

fn report_integer_parse_error(
    diagnostics: &mut Diagnostics,
    literal: &str,
    ty_name: &str,
    error: &IntErrorKind,
    range: Option<TextRange>,
) {
    match error {
        IntErrorKind::Empty | IntErrorKind::InvalidDigit => {
            push_error_with_range(
                diagnostics,
                format!("Invalid integer literal: {literal}"),
                range,
            );
        }
        _ => report_integer_overflow(diagnostics, literal, ty_name, range),
    }
}

fn report_integer_overflow(
    diagnostics: &mut Diagnostics,
    literal: &str,
    ty_name: &str,
    range: Option<TextRange>,
) {
    push_error_with_range(
        diagnostics,
        format!("Integer literal {literal} does not fit in {ty_name}"),
        range,
    );
}

fn report_invalid_char_literal(
    diagnostics: &mut Diagnostics,
    literal: &str,
    range: Option<TextRange>,
) {
    push_error_with_range(
        diagnostics,
        format!("Invalid char literal: {literal}"),
        range,
    );
}

pub(crate) fn ensure_float_literal_fits(
    diagnostics: &mut Diagnostics,
    value: f64,
    ty: &tast::Ty,
    range: Option<TextRange>,
) {
    if !value.is_finite() {
        push_error_with_range(diagnostics, "Float literal must be finite", range);
        return;
    }

    if matches!(ty, tast::Ty::TFloat32) && (value < f32::MIN as f64 || value > f32::MAX as f64) {
        push_error_with_range(
            diagnostics,
            format!("Float literal {value} does not fit in float32"),
            range,
        );
    }
}

pub(crate) fn parse_float_literal_with_ty(
    diagnostics: &mut Diagnostics,
    literal: &str,
    ty: &tast::Ty,
    range: Option<TextRange>,
) -> Option<Prim> {
    match literal.parse::<f64>() {
        Ok(value) => {
            ensure_float_literal_fits(diagnostics, value, ty, range);
            match ty {
                tast::Ty::TFloat32 => Some(Prim::Float32 {
                    value: value as f32,
                }),
                tast::Ty::TFloat64 => Some(Prim::Float64 { value }),
                _ => None,
            }
        }
        Err(_) => {
            push_error_with_range(
                diagnostics,
                format!("Invalid float literal: {literal}"),
                range,
            );
            None
        }
    }
}
