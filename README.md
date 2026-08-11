# goml

goml is a statically typed programming language inspired by Go and Rust.

The "ml" in goml nods to the [ML (programming language)](https://en.wikipedia.org/wiki/ML_(programming_language)), whose descendants have deeply influenced Rust.

goml aims to empower gophers with a more powerful type system but without leaving the Go ecosystem.

The compiler, project driver, tests, and language server are implemented in GoML. The main development commands are:

```sh
just make
just test
just all
just clean
```

`just make` builds the stage2 toolchain, `just test` runs the self-hosted compiler and driver tests, `just all` builds and tests, and `just clean` removes local build caches and generated toolchains.

On Linux amd64, the bootstrap uses Bash, curl, tar, and sha256sum to download the checksum-pinned stage0 compiler recorded in `bootstrap/stage0.env`. `just bootstrap` performs a clean fixed-point build: it uses stage0 to build stage1, builds stage2 from stage1, builds stage3 from stage2, then compares the stage2 and stage3 compiler and driver artifacts. Set `GOML_STAGE0_ARCHIVE` to a previously downloaded stage0 archive for an offline bootstrap.

Generated toolchains are published under `stage1`, `stage2`, and `stage3`, with executables in each stage's `bin` directory, GoML toolchain projects in `lib`, and the installed compiler world in `lib/compiler`. Stage2 is the stable self-compiled toolchain, while stage3 verifies its fixed point. The downloaded stage0 toolchain uses the same layout under `stage0`; downloaded archives and build artifacts are stored under `_bootstrap/`. These generated directories are ignored by Git.

Release versions and the binary bootstrap chain are documented in [docs/releasing.md](docs/releasing.md).

Explore [gomlc/testdata/pipeline](gomlc/testdata/pipeline) for source programs and every compiler-stage golden file. Use `just verify-golden` to check the corpus or `just update-golden` to regenerate it through the self-hosted compiler.

## Disclaimer

This project is a **personal project** and is **NOT** affiliated with, endorsed by, or connected to any organization.

⚠️Do not use this project or any of its derivatives in production environments.

The author assumes no responsibility for any risk or damage resulting from the use of this project.

## Ownership and License

This project does not currently have an open-source license. Until a license is explicitly provided, all rights to the project — including code, documentation, design, and related resources — are reserved by the author. No copying, distribution, modification, or commercial use is permitted without prior authorization.
