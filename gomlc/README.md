# gomlc

`gomlc` is the complete self-hosted GoML compiler and language server. It implements:

```text
lexer → parser → CST → AST → HIR → TAST → Core → Mono → Lift → ANF → Go
```

On Linux amd64, a fresh checkout downloads the checksum-pinned binary stage0 and builds the stable stage2 toolchain:

```sh
just make
```

Use `just bootstrap` when a clean stage2/stage3 fixed-point verification is required. The stable tools are:

```text
bin/stage2/gomlc
bin/stage2/gomllsp
bin/stage2/goml
```

Run a single source or inspect an IR stage:

```sh
bin/stage2/gomlc run-single file.gom
bin/stage2/gomlc anf file.gom
bin/stage2/gomlc run-single --dump-go file.gom
```

The regression corpus and every generated golden file live in `gomlc/testdata`. Verify or update them with:

```sh
just verify-golden
just update-golden
```

Run all self-hosted compiler, pipeline, query, and language-server tests with
`just test`.
