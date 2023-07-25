import * as path from "path";
import { readdir, unlink } from "fs";
import { window, ViewColumn, Uri } from "vscode";
import { CACHE_DIR } from "./common/constants";


let graph_list: string[] = [];
const directory = path.join(CACHE_DIR, "graphs");


export function updateMessageGraphs() {
	readdir(directory, (error, files) => {
		if (error) throw error;
		files.forEach(file => {
			if (file.endsWith(".svg") && !graph_list.includes(file)) {
				createGraphWebView(directory, file);
			}
		});
	});
}

export function cleanupMessageGraphs() {
	readdir(directory, (error, files) => {
		if (error) throw error;
		files.forEach(file => {
			if (!file.endsWith(".svg")) return;
			unlink(path.join(directory, file), (error) => {
				if (error) throw error;
			});
		});
	});
}

function createGraphWebView(directory: string, file: string) {
	graph_list.push(file);

	const imagePath = path.join(directory, file);
	const imageUri = Uri.file(imagePath);
	const messageName = file.slice(0, -4);

	const panel = window.createWebviewPanel(
		`${messageName}Graph`,
		`${messageName} graph`,
		ViewColumn.Two,
		{
			localResourceRoots: [Uri.file(directory)]
		}
	);

	panel.webview.html = getWebviewContent(panel.webview.asWebviewUri(imageUri));

	panel.onDidDispose(() => {
		graph_list = graph_list.filter(f => f != file);
		unlink(imagePath, (error) => {
			if (error) throw error;
		});
	});
}


function getWebviewContent(imageSource: Uri) {
	return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Test</title>
</head>
<body>
	<img src="${imageSource}" width="100%" />	
</body>
<style>
body {
	display: flex;
	align-items: center;
	height: 100%;
}
html {
	height: 100%;
}
</style>
</html>`;
}