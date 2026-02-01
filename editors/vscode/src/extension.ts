import * as path from 'path';
import * as fs from 'fs';
import { workspace, ExtensionContext, window } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
    const serverPath = findServerPath(context);

    if (!serverPath) {
        window.showErrorMessage(
            'GoML language server (goml-lsp) not found. Please install it or set goml.serverPath in settings.'
        );
        return;
    }

    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio,
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio,
        },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'goml' }],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/*.gom'),
        },
        outputChannelName: 'GoML Language Server',
    };

    client = new LanguageClient(
        'goml',
        'GoML Language Server',
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

function findServerPath(context: ExtensionContext): string | undefined {
    const config = workspace.getConfiguration('goml');
    const configPath = config.get<string>('serverPath');

    if (configPath && configPath.length > 0 && fs.existsSync(configPath)) {
        return configPath;
    }

    const bundledPath = path.join(context.extensionPath, 'bin', 'goml-lsp');
    if (fs.existsSync(bundledPath)) {
        return bundledPath;
    }

    const bundledPathExe = path.join(context.extensionPath, 'bin', 'goml-lsp.exe');
    if (fs.existsSync(bundledPathExe)) {
        return bundledPathExe;
    }

    const envPath = process.env.PATH;
    if (envPath) {
        const pathDirs = envPath.split(path.delimiter);
        for (const dir of pathDirs) {
            const candidate = path.join(dir, 'goml-lsp');
            if (fs.existsSync(candidate)) {
                return candidate;
            }
            const candidateExe = path.join(dir, 'goml-lsp.exe');
            if (fs.existsSync(candidateExe)) {
                return candidateExe;
            }
        }
    }

    return undefined;
}
