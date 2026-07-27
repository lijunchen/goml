# Self-hosting migration

The migration is complete:

- `bootstrap/` owns the compiler, query engine, language server, standard-library generator, and compiler tests.
- `bootstrap-goml/` owns the project driver, package manager, dependency resolver, and CLI tests.
- `bootstrap/testdata/` owns all source fixtures and generated golden files.
- `stage0/` contains the version-controlled trusted Go sources.
- `just bootstrap` verifies the stage1/stage2 fixed point.
- CI, installation, and the VS Code extension use only the self-hosted toolchain.

The former Rust workspace and web playground have been removed.
