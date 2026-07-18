# gomlang-parser

`gomlang-parser` is a self-hosted GoML frontend that mirrors the current Rust frontend in `goml`.

The implementation provides:

- byte-accurate lexing with the same token kinds, trivia, UTF-8 ranges, and error partitioning as `crates/lexer`
- event-based parsing with the same lossless CST shape and recoverable diagnostics as `crates/parser` and `crates/cst`
- CST-to-AST lowering with the same AST variants, normalized values, source spans, constructor classification, desugaring, and lower diagnostics as `crates/ast`
- AST-to-HIR lowering with the same definitions, local resolution, constructor resolution, derive expansion, and canonical HIR output as `crates/compiler`

Build the command-line tool with:

```sh
goml build .
```

The resulting `artifact/bin/parser` accepts a stage and a source file:

```sh
artifact/bin/parser lex path/to/file.gom
artifact/bin/parser cst path/to/file.gom
artifact/bin/parser ast path/to/file.gom
artifact/bin/parser hir path/to/file.gom
```

Run the package tests serially:

```sh
goml test lexer
goml test parser
goml test ast
goml test hir
```

The differential tools build a Rust oracle from the sibling `goml` checkout and compare canonical outputs byte for byte:

```sh
tools/diff-lexer-corpus.sh
tools/diff-cst-corpus.sh
tools/diff-ast-corpus.sh
tools/diff-hir-corpus.sh
tools/diff-generated.sh
```

Set `GOML_REPO` when the Rust repository is not available at `../goml`.
