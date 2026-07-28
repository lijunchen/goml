# gomlc

`gomlc` is the complete self-hosted GoML compiler and language server. It implements:

```text
lexer → parser → CST → AST → HIR → TAST → Core → Mono → Lift → ANF → Go
```

On Linux amd64, a fresh checkout downloads the checksum-pinned binary stage0 and builds the self-hosted compiler:

```sh
just bootstrap
```

This compiles stage1 and stage2 from the GoML sources and verifies the fixed point. The stage1 tools are:

```text
bin/stage1/gomlc
bin/stage1/gomllsp
bin/stage1/goml
```

Run a single source or inspect an IR stage:

```sh
bin/stage1/gomlc run-single file.gom
bin/stage1/gomlc anf file.gom
bin/stage1/gomlc run-single --dump-go file.gom
```

The regression corpus and every generated golden file live in `gomlc/testdata`. Verify or update them with:

```sh
just verify-golden
just update-golden
```

Run the compiler and language-server suites with:

```sh
just test-bootstrap-all
just test-bootstrap-compiler
just test-bootstrap-pipeline
just test-bootstrap-lsp
```
