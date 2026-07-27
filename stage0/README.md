# Go bootstrap stage0

`stage0/gomlc/gomlc.go` and `stage0/goml/goml.go` are generated Go sources for
the self-hosted compiler and project driver.

The initial compiler seed is produced while the Rust implementation still
exists:

```sh
just generate-stage0-compiler-from-rust
```

That recipe builds the GoML compiler with Rust, recompiles it twice with the
generated compiler, verifies the self-hosted fixed point, and writes
`stage0/gomlc/gomlc.go`.

After the required standard-library host hooks are available, the generated
compiler produces the driver seed:

```sh
just generate-stage0-driver-from-compiler
```

Once both files are checked in, a cold bootstrap only needs Go:

```sh
just bootstrap
```

The cold bootstrap builds stage1 and stage2 and verifies that their compiler
and driver artifacts are identical to each other and to stage0.
