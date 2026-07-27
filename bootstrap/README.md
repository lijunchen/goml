# gomlc

`bootstrap` is the complete self-hosted GoML compiler and language server. It implements:

```text
lexer → parser → CST → AST → HIR → TAST → Core → Mono → Lift → ANF → Go
```

The repository contains version-controlled Go stage0 sources in `stage0/`. A fresh checkout needs only Go:

```sh
just bootstrap
```

This builds the stage0 tools, compiles stage1 and stage2 from the GoML sources, and verifies the fixed point. The stage1 tools are:

```text
bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc
bootstrap/_bootstrap/stage1/bin/cmd/gomllsp/gomllsp
bootstrap-goml/_bootstrap/stage1/bin/cmd/goml/goml
```

Run a single source or inspect an IR stage:

```sh
bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc run-single file.gom
bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc anf file.gom
bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc run-single --dump-go file.gom
```

The regression corpus and every generated golden file live in `bootstrap/testdata`. Verify or update them with:

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
