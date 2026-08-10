import pathlib
import unicodedata


def string_literal(value):
    return '"' + value.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\r', '\\r').replace('\t', '\\t') + '"'


def char_literal(value):
    return "'" + value.replace('\\', '\\\\').replace("'", "\\'") + "'"


if unicodedata.unidata_version != "15.0.0":
    raise SystemExit("Python Unicode data must be 15.0.0")

folds = []
for codepoint in range(0x110000):
    value = chr(codepoint)
    folded = value.casefold()
    if folded != value:
        folds.append((value, folded))

lines = [
    "package unicode;",
    "",
    "use std::text;",
    "",
    "fn case_fold_scalar(value: char) -> string {",
    "    match value {",
]
for source, folded in folds:
    lines.append(f"        {char_literal(source)} => {string_literal(folded)},")
lines.extend([
    "        _ => value.to_string(),",
    "    }",
    "}",
    "",
    "pub fn case_fold(value: string) -> string {",
    "    let result = text::StringBuilder::new();",
    "    for character in value.chars() {",
    "        result.write_string(case_fold_scalar(character))",
    "    }",
    "    result.to_string()",
    "}",
    "",
])

target = pathlib.Path(__file__).resolve().parents[1] / "lib/std/unicode/casefold.gom"
target.write_text("\n".join(lines), encoding="utf-8")
