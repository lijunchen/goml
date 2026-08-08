# gomlgo

`gomlgo` is an independent GoML module implementing a Go 1.26 scanner, recursive-descent parser, package type checker, and source importer. The compatibility target is Go 1.26.5 at `/usr/lib/go-1.26`. Production code does not call `go/scanner`, `go/parser`, `go/types`, `go/constant`, or `go/importer`.

The frontend accepts source as `std::bytes::Bytes`, preserving invalid UTF-8 and physical byte offsets. The scanner implements the complete Go token set, literal validation, comments, BOM and NUL handling, Unicode identifiers, and automatic semicolon insertion. Tokens retain kind, literal, start/end offsets, and whether a semicolon is synthetic.

The parser is organized as declarations/files, types, expressions, statements, comments, and build constraints. It uses a comment-skipping token cursor, explicit recovery points, progress guards, a diagnostic cap, and a nesting guard. Its normalized AST mirrors every Go 1.26 `go/ast` expression, statement, declaration, field, comment, and file node used by `parser.ParseFile` with `ParseComments | AllErrors | SkipObjectResolution`. Positions are 0-based physical byte offsets and `-1` represents `token.NoPos`.

The type checker lowers the parser AST into a strongly typed syntax arena with stable file and node IDs. Types, objects, packages, scopes, constants, selections, instances, and initialization dependencies use ID-based arenas and side tables. It supports package and file scopes, declarations, imports, named and alias types, interfaces and method sets, constants, expressions and builtins, generics and inference, statements and control flow, initialization ordering, and Go 1.26 `new(expr)` behavior. Constants use arbitrary-precision integers and rationals plus a 512-bit binary floating representation for values that exceed the exact rational resource boundary.

The source importer evaluates `//go:build` and legacy `// +build` constraints, GOOS/GOARCH and release tags, custom tags, cgo selection, platform filename suffixes, import cycles, and source dependencies. It selects ordinary, internal-test, and external-test package sources separately. Its default target is `linux/amd64`, cgo disabled, with `/usr/lib/go-1.26/src` as the source root.

`checker::CheckConfig` exposes `go_version`, target `sizes`, `ignore_func_bodies`, `fake_import_c`, `disable_unused_import_check`, `enable_alias`, and explicit import availability/failure inputs. The default is Go 1.26, gc/amd64 sizes, function bodies enabled, cgo import emulation disabled, unused-import checks enabled, and materialized aliases.

`oracle/` is test-only Go code built with `/usr/lib/go-1.26/bin/go`. It exposes `version`, `scan`, `parse-file`, `parse-expr`, `type-check-package`, and `constant-eval` over stdin/stdout JSON. Type-check requests contain a package path, Go version, target, source files, and checker configuration; responses contain diagnostics, package metadata, normalized scopes and objects, and all `go/types.Info` sections. The test protocol also has `collect_info`; acceptance-only GOROOT runs set it to false so the oracle does not normalize data that will not be compared. GoML differential runners invoke the oracle only as an independent reference process.

## Commands

From the repository root:

```bash
just gofront-test
just gofront-build
just gofront-diff
just gofront-type-test
just gofront-type-diff
just gofront-type-diff-official
just gofront-type-diagnostics-official
just gofront-type-diff-goroot 10 std
just gofront-type-diff-goroot -1 cmd acceptance
just gofront-type-diff-goroot -1 std info
just gofront-type-diff-goroot 1 std acceptance linux arm64 0 none
just gofront-type-diff-goroot-matrix 16
just gofront-type-diff-goroot-tests 16 internal
just gofront-type-diff-goroot-tests 16 external
just gofront-type-mutate-diff 25
just gofront-diff-goroot
just gofront-parse-diff-goroot
just gofront-ast-shape-goroot
just gofront-ast-position-goroot
just gofront-ast-comments-goroot
just gofront-mutate-diff 25
```

The built CLI supports:

```bash
_artifact/gomlgo-build/bin/cmd/gomlgo/gomlgo scan FILE
_artifact/gomlgo-build/bin/cmd/gomlgo/gomlgo parse FILE
_artifact/gomlgo-build/bin/cmd/gomlgo/gomlgo parse-expr 'a + b*c'
_artifact/gomlgo-build/bin/cmd/gomlgo/gomlgo check FILE...
_artifact/gomlgo-build/bin/cmd/gomlgo/gomlgo check-package DIRECTORY
```

The parser differential binary accepts `parse-acceptance`, `ast-shape`, `ast-position`, `ast-comments`, or `diagnostic-strict`. A mismatch reports the file, normalized AST path, surrounding node offset, expected value, and actual value.

Mutation differential failures are stored under `_artifact/gomlgo-diff/failures/<hash>/` with `input.go`, `gomlgo.json`, `oracle.json`, and `diff.txt`.

Type-check mutation failures are stored under `_artifact/gomlgo-type-diff/failures/<hash>/` with `request.json`, `sources/`, `gomlgo.json`, `oracle.json`, and `diff.txt`.

The GOROOT type differential recipes run their checker in a user systemd scope with `MemoryHigh=6G`, `MemoryMax=8G`, `MemorySwapMax=1G`, `TasksMax=512`, and `CPUQuota=300%`. They accept GOOS, GOARCH, cgo, and `none|internal|external` test-package modes, and reset the loader session every 16 target packages. `acceptance` compares accept/reject status and diagnostics needed to determine it; `info` additionally compares normalized `TypeInfo`.

The official type corpus recipes also run in a resource-limited user systemd scope. They invoke one checker process per source file so retained dependency state is released between cases; the hard limits are 10 GiB memory, 2 GiB swap, 256 tasks, and 300% CPU.

`testdata/invalid/` contains recovery smoke cases. `testdata/regressions/` contains minimized differential failures and is checked by `just gofront-diff`.

## Current parity

Against all 7,710 `.go` files under `/usr/lib/go-1.26/src`:

- Scanner: 17,255,942 tokens, zero kind/literal/start/end/synthetic mismatches.
- Parse acceptance: zero mismatches, with 7,666 accepted and 44 rejected files.
- Valid AST shape and important data: zero mismatches.
- Valid AST byte positions: zero mismatches.
- Comments, doc/trailing attachment, `File.Comments`, and `GoVersion`: zero mismatches.
- Mutation sample: 25 valid seeds and 272 generated cases had zero acceptance mismatches; 8 cases differed only in the first diagnostic byte offset.
- Local type-check fixtures: 3 files, with 2 accepted and 1 rejected, have zero acceptance differences; accepted files have complete normalized TypeInfo parity and the rejected fixture has exact diagnostic parity.
- Official Go type-check corpus: 363 files have zero acceptance and exact-diagnostic differences. The result is 83 accepted and 280 rejected files; all 83 accepted files have complete normalized package, type, Defs, Uses, Implicits, Selections, Scopes, Instances, InitOrder, FileVersions, Universe, and unsafe parity, and all 280 rejected files have partial normalized TypeInfo parity.
  - `internal/types/testdata/check`: 79 files, 13 accepted and 66 rejected.
  - `internal/types/testdata/spec`: 12 files, 1 accepted and 11 rejected.
  - `internal/types/testdata/examples`: 8 files, all rejected.
  - `internal/types/testdata/fixedbugs`: 262 files, 68 accepted and 194 rejected.
  - `go/types/testdata/local`: 2 files, 1 accepted and 1 rejected.
- Type-check mutation smoke corpus: 2 accepted seeds and 9 semantic mutations have zero acceptance differences.
- GOROOT `std` on linux/amd64 with cgo disabled: 339 selected packages, 336 accepted and 3 rejected, with zero acceptance differences. Complete normalized TypeInfo matches for 335 accepted source packages; the remaining accepted package is the built-in `unsafe` package.
- GOROOT `cmd/...` on linux/amd64 with cgo disabled: 210 selected packages, 188 accepted and 22 rejected, with zero acceptance differences.
- Targeted extension smoke checks have zero acceptance differences for `internal/goarch` on linux/arm64, `internal/goos` on windows/amd64, `runtime/cgo` with cgo enabled, the internal tests of `bytes`, and the external test package `cmp_test`.

Strict parser diagnostic count and message parity for invalid files is still best-effort. Complete `cmd/...` Info, the full cgo-enabled corpus, broad internal/external test-package corpora, and full GOOS/GOARCH matrices have not been established as zero-diff.
