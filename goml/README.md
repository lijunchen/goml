# goml project driver

`goml` is the self-hosted project driver. It provides project creation, package discovery, check/build/run/test plans, dependency resolution, registry cache management, incremental artifact fingerprints, native linking, and parallel test execution.

Build and verify the complete toolchain:

```sh
just all
```

`just test` includes the project-driver tests.

Use the stage1 driver directly:

```sh
cd goml
../bin/stage1/goml check \
  --compiler ../bin/stage1/gomlc
../bin/stage1/goml test \
  --compiler ../bin/stage1/gomlc \
  --jobs 4 \
  --timeout 30s
```

`goml test --nocapture` inherits test output, while `--timeout` accepts positive `ms`, `s`, or `m` durations. Compiler, linker, and Go build steps are skipped only when their compiler identity, arguments, inputs, and recorded output digests all match.

The driver resolves `gomlc` from `--compiler`, `GOMLC`, a sibling binary, `GOML_HOME/bin`, then `PATH`.

Package-management commands are:

```sh
goml update
goml add owner::module
goml add owner::module@1.2.3
goml remove owner::module
```

`update`, `add`, and `remove` accept `--local-registry <path>`. Registry state is stored under `$GOML_HOME/cache/registry`, defaulting to `~/.goml/cache/registry`.

Driver tests live beside the CLI in `cmd/goml/cli_migration_test.gom`. Their isolated workspaces are written below `_artifact/test-work`.
