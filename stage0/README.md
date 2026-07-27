# Go bootstrap stage0

`gomlc.go` and `goml.go` are generated Go sources for the self-hosted compiler
and project driver. A fresh checkout builds them with the Go toolchain, then
uses those stage0 binaries to compile the GoML sources into stage1 and stage2.

Run `just bootstrap` to build both stages and verify that their compiler,
driver, and language-server artifacts are identical.

Run `just regenerate-stage0` after an intentional compiler or driver change.
The command first completes the fixed-point build, then replaces stage0 with
the verified stage2 output.
