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
stage2/bin/gomlc
stage2/bin/gomllsp
stage2/bin/goml
```

Each stage is a complete toolchain prefix. The compiler loads builtin and standard-library sources from `stage2/lib`, derived only from the executable's location.

Run a single source or inspect an IR stage:

```sh
stage2/bin/gomlc run-single file.gom
stage2/bin/gomlc anf file.gom
stage2/bin/gomlc run-single --dump-go file.gom
```

The regression corpus and every generated golden file live in `gomlc/testdata`. Verify or update them with:

```sh
just verify-golden
just update-golden
```

Run all self-hosted compiler, pipeline, query, and language-server tests with
`just test`.
