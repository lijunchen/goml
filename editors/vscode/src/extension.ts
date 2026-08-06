import * as path from 'path';
import * as fs from 'fs';
import {
    commands,
    ExtensionContext,
    ProcessExecution,
    Task,
    TaskPanelKind,
    TaskRevealKind,
    TaskScope,
    tasks,
    Uri,
    ViewColumn,
    window,
    workspace,
} from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
    context.subscriptions.push(
        commands.registerCommand(
            'goml.runTest',
            async (uriText: string, testName: string, kind: string) => {
                const uri = Uri.parse(uriText);
                const document = workspace.textDocuments.find(
                    candidate => candidate.uri.toString() === uri.toString()
                );
                if (document?.isDirty) {
                    await document.save();
                }
                const folder = workspace.getWorkspaceFolder(uri);
                const scope = folder ?? TaskScope.Workspace;
                const cwd = folder?.uri.fsPath ?? path.dirname(uri.fsPath);
                const task = new Task(
                    { type: 'goml', task: 'test' },
                    scope,
                    `test ${testName}`,
                    'goml',
                    new ProcessExecution(
                        'goml',
                        ['test', testName, '--kind', kind],
                        { cwd }
                    )
                );
                task.presentationOptions = {
                    reveal: TaskRevealKind.Always,
                    panel: TaskPanelKind.Dedicated,
                    clear: true,
                };
                await tasks.executeTask(task);
            }
        )
    );

    context.subscriptions.push(
        commands.registerCommand('goml.showExpandedDerive', async () => {
            const editor = window.activeTextEditor;
            if (!editor || editor.document.languageId !== 'goml') {
                window.showErrorMessage('Open a GoML source file to show expanded derives.');
                return;
            }
            if (!client) {
                window.showErrorMessage('GoML language server is not running.');
                return;
            }
            const text = await client.sendRequest<string>('goml/expandedDerive', {
                textDocument: { uri: editor.document.uri.toString() },
            });
            const document = await workspace.openTextDocument({ language: 'goml', content: text });
            await window.showTextDocument(document, {
                preview: true,
                viewColumn: ViewColumn.Beside,
            });
        })
    );

    const serverPath = findServerPath(context);

    if (!serverPath) {
        window.showErrorMessage(
            'GoML language server not found. Please install gomllsp or set goml.serverPath in settings.'
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

    const binary = 'gomllsp';
    const bundledPath = path.join(context.extensionPath, 'bin', binary);
    if (fs.existsSync(bundledPath)) {
        return bundledPath;
    }

    const bundledPathExe = path.join(context.extensionPath, 'bin', `${binary}.exe`);
    if (fs.existsSync(bundledPathExe)) {
        return bundledPathExe;
    }

    const envPath = process.env.PATH;
    if (envPath) {
        const pathDirs = envPath.split(path.delimiter);
        for (const dir of pathDirs) {
            const candidate = path.join(dir, binary);
            if (fs.existsSync(candidate)) {
                return candidate;
            }
            const candidateExe = path.join(dir, `${binary}.exe`);
            if (fs.existsSync(candidateExe)) {
                return candidateExe;
            }
        }
    }

    return undefined;
}
