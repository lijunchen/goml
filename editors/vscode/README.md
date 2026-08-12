# GoML VS Code extension

The extension provides syntax highlighting, diagnostics, hover, completion, go-to-definition, signature help, inlay hints, formatting, test code lenses, and quick fixes.

The status bar shows whether the language server is starting, ready, stopped, or busy. While it is busy, it shows the current server operation and elapsed time. Hover over the status item to see queued client requests, or click it to open the language server output. Operations taking at least two seconds are recorded there as warnings.

Diagnostics run after editing pauses, while completion reuses the latest checked types immediately. Saving requests diagnostics without waiting for the edit delay.

Build the self-hosted language server and extension:

```sh
just vscode-ext
```

Press F5 from VS Code to launch the Extension Development Host.

Use **Format Document** to format the current GoML buffer. The language server uses GoML's fixed formatting rules and leaves syntactically invalid documents unchanged.

Configuration:

- `goml.serverPath` overrides the bundled or `PATH`-resolved `gomllsp`.
- `goml.trace.server` controls language-server tracing.

Package a `.vsix` with:

```sh
just package-vscode-ext
```
