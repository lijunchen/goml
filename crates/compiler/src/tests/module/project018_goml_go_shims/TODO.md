project018 goml go shims todo

- Add high-level facades for raw-only bindings that are already present, especially `io.SectionReader`, `io.OffsetWriter`, `io.LimitedReader`, `io/fs.PathError`, `os/exec.Error`, and `os/exec.ExitError`.
- Add typed downcast helpers from interface-like foreign values such as `GoError`, `io.Reader`, and `any` into concrete foreign types so raw bindings like `*fs.PathError`, `*exec.Error`, `*exec.ExitError`, and `*io.LimitedReader` become ergonomic.
- Add typed nil and zero-value helpers for foreign interface, pointer, func, and named types so fields like `Cmd.Err`, `Cmd.Cancel`, and `Cmd.Process` can be reset cleanly.
- Add named-type conversion helpers for common Go aliases such as `time.Duration` and similar values so raw fields can be surfaced as plain goml scalars when appropriate.
- Add more end-to-end coverage for callback-heavy APIs that are now bindable, including custom `bufio.SplitFunc`, `strings.ContainsFunc`, `strings.FieldsFunc`, `strings.Map`, and `strings.TrimFunc`.
- Keep `project018` smoke tests on package-level helpers by default; cross-package inherent method use on very large shim facades still puts more pressure on monolithic module compilation.
- Add `stringsshim` and extend the module test to cover the `strings` package.
