# GoML VS Code Extension

This extension provides language support for GoML (`.gom` files).

## Features

- Syntax highlighting
- Hover type information
- Autocompletion (dot, `::`, and value completions)
- Go-to-definition
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

### Configuration

- `goml.serverPath`: Path to the goml-lsp executable. If empty, uses the bundled server or searches PATH.
- `goml.trace.server`: Traces communication between VS Code and the language server.

## Building for Distribution

```bash
cd editors/vscode
npm run package
```

This creates a `.vsix` file that can be installed in VS Code.
