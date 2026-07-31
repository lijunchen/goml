# GoML VS Code extension

The extension provides syntax highlighting, diagnostics, hover, completion, go-to-definition, signature help, inlay hints, formatting, test code lenses, and quick fixes.

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
