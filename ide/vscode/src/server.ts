import { spawn, execSync } from 'child_process';
import * as path from "path";
import * as fsapi from 'fs-extra';

import { Disposable, env, LogOutputChannel, workspace, window, ProgressLocation, Uri } from 'vscode';
import { State, integer } from 'vscode-languageclient';
import {
    LanguageClient,
    LanguageClientOptions,
    RevealOutputChannelOn,
    ServerOptions,
} from 'vscode-languageclient/node';

import { CACHE_DIR } from './common/constants';
import { traceError, traceInfo, traceVerbose } from './common/log/logging';
import { getExtensionSettings, getGlobalSettings, getWorkspaceSettings, ISettings } from './common/settings';
import { getLSClientTraceLevel, getProjectRoot } from './common/utilities';
import { isVirtualWorkspace } from './common/vscodeapi';


export type IInitOptions = { settings: ISettings[]; globalSettings: ISettings };


export async function installServer(workspaceSetting: ISettings): Promise<boolean> {
    const installLogFile = "install.log";
    const installLogPath = path.join(CACHE_DIR, installLogFile);

    const truncateString = (string = '', maxLength: integer): string => {
        return string.length > maxLength ? `${string.substring(0, maxLength)}…` : string;
    };

    await fsapi.remove(installLogPath);
    await fsapi.createFile(installLogPath);

    return await window.withProgress({
        location: ProgressLocation.Notification,
        title: "RecordFlux language server installation ([details](" + Uri.file(installLogPath) + "))",
        cancellable: true
    }, (progress, token) => {
        const promise = new Promise<boolean>(resolve => {
            const logging = fsapi.createWriteStream(installLogPath, { flags: "a" });

            const process = spawn(workspaceSetting.interpreter[0], ["-m", "pip", "install", "RecordFlux"]);

            process.stdout.pipe(logging);
            process.stderr.pipe(logging);

            process.stdout.on("data", data => progress.report({
                message: truncateString(`${data}`, 70)
            }));

            process.on("close", async (code, _signal) => {
                if (code == 0) {
                    resolve(true);
                    return;
                }

                window.showErrorMessage(
                    "Failed to install the server ([details](" + Uri.file(installLogPath) + "))\n"
                );

                resolve(false);
            });

            token.onCancellationRequested(_e => process.kill());
        });
        return promise;
    });
}

export async function isServerInstalled(serverId: string): Promise<boolean> {
    const projectRoot = await getProjectRoot();
    const workspaceSetting = await getWorkspaceSettings(serverId, projectRoot, true);

    try {
        execSync(`${workspaceSetting.interpreter[0]} -m rflx --version`);
    }
    catch (_error) {
        return false;
    }

    return true;
}

async function createServer(
    settings: ISettings,
    serverId: string,
    serverName: string,
    outputChannel: LogOutputChannel,
    initializationOptions: IInitOptions,
): Promise<LanguageClient> {
    const command = settings.interpreter[0];
    const cwd = settings.cwd;

    const newEnv = { ...process.env };
    newEnv.USE_DEBUGPY = 'False';
    newEnv.LS_IMPORT_STRATEGY = settings.importStrategy;
    newEnv.LS_SHOW_NOTIFICATION = settings.showNotifications;

    const args = settings.interpreter.slice(1).concat(["-m", "rflx", "ls"]);
    traceInfo(`Server run command: ${[command, ...args].join(' ')}`);

    const serverOptions: ServerOptions = {
        command,
        args,
        options: { cwd, env: newEnv },
    };

    const clientOptions: LanguageClientOptions = {
        // Register the server for recordflux documents
        documentSelector: isVirtualWorkspace()
            ? [{ language: 'recordflux' }]
            : [
                { scheme: 'file', language: 'recordflux' },
                { scheme: 'untitled', language: 'recordflux' },
            ],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher("**/.clientrc")
        },
        outputChannel: outputChannel,
        traceOutputChannel: outputChannel,
        revealOutputChannelOn: RevealOutputChannelOn.Never,
        initializationOptions,
    };

    return new LanguageClient(serverId, serverName, serverOptions, clientOptions);
}

let _disposables: Disposable[] = [];
export async function restartServer(
    serverId: string,
    serverName: string,
    outputChannel: LogOutputChannel,
    lsClient?: LanguageClient,
): Promise<LanguageClient | undefined> {
    if (lsClient) {
        traceInfo(`Server: Stop requested`);
        await lsClient.stop();
        _disposables.forEach((d) => d.dispose());
        _disposables = [];
    }

    const projectRoot = await getProjectRoot();
    const workspaceSetting = await getWorkspaceSettings(serverId, projectRoot, true);

    const newLSClient = await createServer(workspaceSetting, serverId, serverName, outputChannel, {
        settings: await getExtensionSettings(serverId, true),
        globalSettings: await getGlobalSettings(serverId, false),
    });
    traceInfo(`Server: Start requested.`);
    _disposables.push(
        newLSClient.onDidChangeState((e) => {
            switch (e.newState) {
                case State.Stopped:
                    traceVerbose(`Server State: Stopped`);
                    break;
                case State.Starting:
                    traceVerbose(`Server State: Starting`);
                    break;
                case State.Running:
                    traceVerbose(`Server State: Running`);
                    break;
            }
        }),
    );
    try {
        await newLSClient.start();
    } catch (ex) {
        traceError(`Server: Start failed: ${ex}`);
        return undefined;
    }

    const level = getLSClientTraceLevel(outputChannel.logLevel, env.logLevel);
    await newLSClient.setTrace(level);
    return newLSClient;
}
