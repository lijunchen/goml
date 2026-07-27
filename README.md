# goml

goml is a statically typed programming language inspired by Go and Rust.

The "ml" in goml nods to the [ML (programming language)](https://en.wikipedia.org/wiki/ML_(programming_language)), whose descendants have deeply influenced Rust.

goml aims to empower gophers with a more powerful type system but without leaving the Go ecosystem.

The compiler, project driver, tests, and language server are implemented in GoML. The version-controlled Go stage0 sources in `stage0/` make a cold bootstrap possible with only the Go toolchain:

```sh
just bootstrap
just test-selfhost
```

The fixed-point build compiles the stage0 sources, builds stage1 from GoML and stage2 from stage1, then compares the generated compiler and driver artifacts.

Generated executables are published under `bin/stage0`, `bin/stage1`, and `bin/stage2`. The entire `bin` directory is ignored by Git.

Explore [bootstrap/testdata/pipeline](bootstrap/testdata/pipeline) for source programs and every compiler-stage golden file. Use `just verify-golden` to check the corpus or `just update-golden` to regenerate it through the self-hosted compiler.

## Disclaimer

This project is a **personal project** and is **NOT** affiliated with, endorsed by, or connected to any organization.

⚠️Do not use this project or any of its derivatives in production environments.

The author assumes no responsibility for any risk or damage resulting from the use of this project.

## Ownership and License

This project does not currently have an open-source license. Until a license is explicitly provided, all rights to the project — including code, documentation, design, and related resources — are reserved by the author. No copying, distribution, modification, or commercial use is permitted without prior authorization.
