# gomlang-parser

`gomlang-parser` is a self-hosted GoML frontend that mirrors the current Rust frontend in `goml`.

The implementation provides:

- byte-accurate lexing with the same token kinds, trivia, UTF-8 ranges, and error partitioning as `crates/lexer`
- event-based parsing with the same lossless CST shape and recoverable diagnostics as `crates/parser` and `crates/cst`
- CST-to-AST lowering with the same AST variants, normalized values, source spans, constructor classification, desugaring, and lower diagnostics as `crates/ast`
- AST-to-HIR lowering with the same definitions, local resolution, constructor resolution, derive expansion, and canonical HIR output as `crates/compiler`
- HIR-to-TAST checking with type inference, callable resolution, coercions, pattern checking, and canonical typed output
- TAST-to-Core lowering with explicit calls, let chains, structured control flow, and canonical Core output
- Core-to-Mono lowering with a canonical Mono output stage

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
artifact/bin/parser tast path/to/file.gom
artifact/bin/parser core path/to/file.gom
artifact/bin/parser mono path/to/file.gom
```

Run the package tests serially:

```sh
goml test lexer
goml test parser
goml test ast
goml test hir
goml test tast
goml test core
goml test mono
```

The compiler bootstrap tests build the bootstrap compiler and compare generated inputs, the repository corpus, compiler test suites, and pipeline snapshots byte for byte:

```sh
cargo test -p compiler bootstrap:: -- --test-threads=1
```

Set `GOML_REPO` to run the tests against a different `goml` checkout.
