import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

import { restartServer, isServerInstalled, installServer } from "./server";
import { cleanupMessageGraphs, updateMessageGraphs } from "./graph";
import {
    initializePython,
    checkVersion,
    onDidChangePythonInterpreter,
    resolveInterpreter,
    getInterpreterDetails,
} from "./common/python";
import {
    checkIfConfigurationChanged,
    getInterpreterFromSetting,
    getWorkspaceSettings,
} from "./common/settings";
import {
    registerLogger,
    traceLog,
    traceError,
    traceVerbose,
} from "./common/log/logging";
import { getLSClientTraceLevel, getProjectRoot } from "./common/utilities";
import {
    createOutputChannel,
    onDidChangeConfiguration,
    registerCommand,
} from "./common/vscodeapi";
import { loadServerDefaults, IServerInfo } from "./common/setup";

let client: LanguageClient;

export async function activate(context: vscode.ExtensionContext) {
    const serverInfo = loadServerDefaults();

    const outputChannel = setupLogging(context, serverInfo);

    traceLog(`Name: ${serverInfo.name}`);
    traceLog(`Module: ${serverInfo.module}`);

    context.subscriptions.push(
        onDidChangePythonInterpreter(async () => {
            await runServer(serverInfo, outputChannel);
        }),
        onDidChangeConfiguration(async (e: vscode.ConfigurationChangeEvent) => {
            if (checkIfConfigurationChanged(e, serverInfo.module)) {
                await runServer(serverInfo, outputChannel);
            }
        }),
        registerCommand(`${serverInfo.module}.restart`, async () => {
            await runServer(serverInfo, outputChannel);
        })
    );

    setInterval(() => updateMessageGraphs(), 500);

    setImmediate(async () => {
        const interpreter = getInterpreterFromSetting(serverInfo.module);
        if (interpreter === undefined || interpreter.length === 0) {
            traceLog(`Python extension loading`);
            await initializePython(context.subscriptions);
            traceLog(`Python extension loaded`);
        } else {
            await runServer(serverInfo, outputChannel);
        }
    });
}

export function deactivate(): Thenable<void> {
    cleanupMessageGraphs();

    if (!client) {
        return undefined;
    }
    return client.stop();
}

function setupLogging(
    context: vscode.ExtensionContext,
    serverInfo: IServerInfo
): vscode.LogOutputChannel {
    const outputChannel = createOutputChannel(serverInfo.name);
    context.subscriptions.push(outputChannel, registerLogger(outputChannel));

    const changeLogLevel = async (c: vscode.LogLevel, g: vscode.LogLevel) => {
        const level = getLSClientTraceLevel(c, g);
        await client?.setTrace(level);
    };

    context.subscriptions.push(
        outputChannel.onDidChangeLogLevel(async (e) => {
            await changeLogLevel(e, vscode.env.logLevel);
        }),
        vscode.env.onDidChangeLogLevel(async (e) => {
            await changeLogLevel(outputChannel.logLevel, e);
        })
    );

    return outputChannel;
}

async function runServer(
    serverInfo: IServerInfo,
    outputChannel: vscode.LogOutputChannel
): Promise<void> {
    const interpreter = getInterpreterFromSetting(serverInfo.module);
    if (
        interpreter &&
        interpreter.length > 0 &&
        checkVersion(await resolveInterpreter(interpreter))
    ) {
        traceVerbose(
            `Using interpreter from ${
                serverInfo.module
            }.interpreter: ${interpreter.join(" ")}`
        );
    } else {
        const interpreterDetails = await getInterpreterDetails();
        if (interpreterDetails.path) {
            traceVerbose(
                `Using interpreter from Python extension: ${interpreterDetails.path.join(
                    " "
                )}`
            );
        } else {
            traceError(
                "Python interpreter missing:\r\n" +
                    "[Option 1] Select python interpreter using the ms-python.python.\r\n" +
                    `[Option 2] Set an interpreter using "${serverInfo.module}.interpreter" setting.\r\n` +
                    "Please use Python 3.8 or greater."
            );
            return;
        }
    }

    if (!(await isServerInstalled(serverInfo.module))) {
        const projectRoot = await getProjectRoot();
        const workspaceSetting = await getWorkspaceSettings(
            serverInfo.module,
            projectRoot,
            true
        );

        const result = await vscode.window.showWarningMessage(
            "RecordFlux language server is not installed in the selected environment",
            "Install",
            "Select Environment"
        );

        switch (result) {
            case "Install":
                if (!(await installServer(workspaceSetting))) {
                    return await runServer(serverInfo, outputChannel);
                }
                break;
            case "Select Environment":
                await vscode.commands.executeCommand("python.setInterpreter");
                return await runServer(serverInfo, outputChannel);
        }
    }

    client = await restartServer(
        serverInfo.module,
        serverInfo.name,
        outputChannel,
        client
    );
}
