# GoML VS Code Extension

This extension provides language support for GoML (`.gom` files).

## Features

- Syntax highlighting for current GoML syntax, including multiline strings, attributes, chars, and generic declarations
- Hover type information
- Autocompletion (dot, `::`, and value completions)
- Go-to-definition
- Signature help and inlay hints
- Test code lenses and diagnostic quick fixes
- Diagnostics (errors and warnings)

## Installation

### Development

1. Build the LSP server:
   ```bash
   cargo build -p lsp-server --release
   ```

2. Copy the binary to the extension's `bin` folder:
   ```bash
   mkdir -p editors/vscode/bin
   cp target/release/goml-lsp editors/vscode/bin/
   ```

3. Install dependencies and compile the extension:
   ```bash
   cd editors/vscode
   npm install
   npm run compile
   ```

4. Open VS Code in the extension folder and press F5 to launch a new Extension Development Host.

To build and install the bootstrap server alongside the Rust server:

```bash
just install-bootstrap-lsp
```

This also installs the standard-library and builtin sources used by
go-to-definition.

### Configuration

- `goml.serverImplementation`: Selects `rust` (`goml-lsp`) or `bootstrap` (`gomllsp`). The default remains `rust`.
- `goml.serverPath`: Path to a language server executable. When set, this overrides `goml.serverImplementation`.
- `goml.trace.server`: Traces communication between VS Code and the language server.

## Building for Distribution

```bash
cd editors/vscode
npm run package
```

This creates a `.vsix` file that can be installed in VS Code.
