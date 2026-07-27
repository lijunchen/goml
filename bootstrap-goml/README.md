# goml project driver

`bootstrap-goml` is the self-hosted project driver. It provides project creation, package discovery, check/build/run/test plans, dependency resolution, registry cache management, incremental artifact fingerprints, native linking, and parallel test execution.

Build and verify the complete toolchain:

```sh
just bootstrap
just test-bootstrap-driver
```

Use the stage1 driver directly:

```sh
cd bootstrap-goml
_bootstrap/stage1/bin/cmd/goml/goml check \
  --compiler ../bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc
_bootstrap/stage1/bin/cmd/goml/goml test \
  --compiler ../bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc \
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
