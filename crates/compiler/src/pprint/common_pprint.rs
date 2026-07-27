use crate::common::Constructor;
use crate::env::StructDef;
use pretty::RcDoc;

/// Helper function to print a Constructor with its arguments.
/// `args_docs` should be an iterator of already-converted RcDoc elements.
/// `struct_def` is optional and used for printing struct field names.
pub fn constructor_to_doc<'a>(
    constructor: &Constructor,
    args_docs: impl Iterator<Item = RcDoc<'a, ()>> + Clone,
    struct_def: Option<&StructDef>,
) -> RcDoc<'a, ()> {
    match constructor {
        Constructor::Enum(enum_constructor) => {
            let name_doc = RcDoc::text(format!(
                "{}::{}",
                enum_constructor.type_name.0, enum_constructor.variant.0
            ));

            let args: Vec<_> = args_docs.collect();
            if args.is_empty() {
                name_doc
            } else {
                let args_doc = RcDoc::intersperse(args, RcDoc::text(", "));

                name_doc
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
        }
        Constructor::Struct(struct_constructor) => {
            let name_doc = RcDoc::text(struct_constructor.type_name.0.clone());
            let args: Vec<_> = args_docs.collect();

            if let Some(struct_def) = struct_def {
                if struct_def.fields.is_empty() {
                    name_doc.append(RcDoc::space()).append(RcDoc::text("{}"))
                } else if struct_def.fields.len() == args.len() {
                    let fields_doc = RcDoc::intersperse(
                        struct_def.fields.iter().zip(args).map(|((fname, _), arg)| {
                            RcDoc::text(fname.0.clone())
                                .append(RcDoc::text(": "))
                                .append(arg)
                        }),
                        RcDoc::text(", "),
                    );

                    name_doc
                        .append(RcDoc::space())
                        .append(RcDoc::text("{ "))
                        .append(fields_doc)
                        .append(RcDoc::text(" }"))
                } else if args.is_empty() {
                    name_doc
                } else {
                    let args_doc = RcDoc::intersperse(args, RcDoc::text(", "));

                    name_doc
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
            } else if args.is_empty() {
                name_doc
            } else {
                let args_doc = RcDoc::intersperse(args, RcDoc::text(", "));

                name_doc
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
        }
    }
}

/// Helper function to print a field accessor for EConstrGet.
/// Returns the accessor doc like `Type::Variant._0` or `Type.field_name`.
pub fn constr_get_accessor_doc<'a>(
    constructor: &Constructor,
    field_index: usize,
    struct_def: Option<&StructDef>,
) -> RcDoc<'a, ()> {
    match constructor {
        Constructor::Enum(enum_constructor) => RcDoc::text(format!(
            "{}::{}._{}",
            enum_constructor.type_name.0, enum_constructor.variant.0, field_index
        )),
        Constructor::Struct(struct_constructor) => {
            let field_name = struct_def
                .and_then(|def| def.fields.get(field_index))
                .map(|(fname, _)| fname.0.clone())
                .unwrap_or_else(|| format!("_{}", field_index));
            RcDoc::text(format!(
                "{}.{field}",
                struct_constructor.type_name.0,
                field = field_name
            ))
        }
    }
}
