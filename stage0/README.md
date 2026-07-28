# Go bootstrap stage0

`stage0/gomlc/gomlc.go` and `stage0/goml/goml.go` are generated Go sources for
the self-hosted compiler and project driver.

The initial compiler seed was produced while the Rust implementation still
existed. Its generating commit records this recipe:

```sh
just generate-stage0-compiler-from-rust
```

The recipe builds the GoML compiler with Rust, recompiles it twice with the
generated compiler, verifies the self-hosted fixed point, and writes
`stage0/gomlc/gomlc.go`. It is committed immediately before the generated compiler
seed, so that historical checkout remains reproducible.

After the required standard-library host hooks became available, the generated
compiler produced the driver seed with the next recorded recipe:

```sh
just generate-stage0-driver-from-compiler
```

That recipe is likewise committed immediately before `stage0/goml/goml.go`.
Once both files are checked in, a cold bootstrap only needs Go:

```sh
just bootstrap
```

Generated executables are published under `bin/stage0`, `bin/stage1`, and
`bin/stage2`; none of them are tracked by Git.

The cold bootstrap builds stage1 and stage2 and verifies that their compiler
and driver artifacts are identical to each other and to stage0.
