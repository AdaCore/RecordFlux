import * as vscode from "vscode";
import * as assert from "assert";
import { getDocUri, activate } from "./helper";

describe("Should provide semantic tokens", () => {
    const docUri = getDocUri("universal.rflx");

    it("Provides semantic tokens for highlighting a document", async () => {
        await testSemanticTokens(
            docUri,
            new vscode.SemanticTokens(
                new Uint32Array([
                    0, 8, 9, 7, 0, 2, 8, 12, 1, 0, 0, 17, 7, 2, 0, 1, 25, 7, 2,
                    0, 1, 25, 8, 2, 0, 1, 25, 9, 2, 0, 1, 25, 15, 2, 0, 1, 25,
                    10, 2, 0, 1, 25, 21, 2, 0, 1, 25, 24, 2, 0, 2, 8, 6, 0, 0,
                    2, 8, 5, 0, 0, 2, 8, 6, 0, 0, 0, 22, 5, 0, 0, 2, 8, 11, 1,
                    0, 0, 16, 7, 2, 0, 0, 14, 7, 2, 0, 0, 46, 4, 2, 0, 2, 8, 12,
                    0, 0, 0, 28, 11, 1, 0, 2, 8, 6, 3, 0, 2, 9, 11, 4, 0, 0, 14,
                    11, 1, 0, 2, 18, 11, 4, 0, 0, 14, 7, 2, 0, 1, 17, 6, 4, 0,
                    1, 18, 11, 4, 0, 0, 14, 7, 2, 0, 1, 9, 6, 4, 0, 0, 9, 6, 0,
                    0, 1, 17, 4, 4, 0, 1, 28, 6, 4, 0, 1, 9, 4, 4, 0, 3, 8, 7,
                    0, 0, 0, 23, 6, 3, 0, 2, 8, 7, 3, 0, 2, 9, 12, 4, 0, 0, 15,
                    12, 1, 0, 2, 18, 12, 4, 0, 0, 15, 7, 2, 0, 1, 17, 4, 4, 0,
                    1, 28, 7, 3, 0, 0, 15, 12, 4, 0, 1, 18, 12, 4, 0, 0, 15, 21,
                    2, 0, 1, 17, 6, 4, 0, 1, 18, 12, 4, 0, 0, 16, 7, 2, 0, 1,
                    22, 12, 4, 0, 0, 16, 21, 2, 0, 1, 22, 12, 4, 0, 0, 16, 24,
                    2, 0, 1, 17, 7, 4, 0, 1, 28, 7, 3, 0, 0, 15, 12, 4, 0, 1,
                    18, 12, 4, 0, 0, 15, 24, 2, 0, 1, 9, 6, 4, 0, 0, 9, 6, 0, 0,
                    1, 17, 4, 4, 0, 1, 28, 6, 4, 0, 1, 18, 12, 4, 0, 0, 15, 7,
                    2, 0, 1, 17, 12, 4, 0, 1, 28, 6, 4, 0, 1, 18, 12, 4, 0, 0,
                    15, 15, 2, 0, 1, 17, 7, 4, 0, 1, 28, 6, 4, 0, 1, 18, 12, 4,
                    0, 0, 15, 10, 2, 0, 1, 17, 5, 4, 0, 1, 18, 12, 4, 0, 0, 15,
                    8, 2, 0, 1, 22, 6, 4, 0, 0, 9, 9, 7, 0, 0, 11, 5, 0, 0, 1,
                    17, 6, 4, 0, 1, 28, 6, 4, 0, 1, 18, 12, 4, 0, 0, 15, 9, 2,
                    0, 1, 9, 4, 4, 0, 2, 9, 12, 4, 0, 0, 15, 12, 0, 0, 2, 9, 7,
                    4, 0, 0, 10, 7, 0, 0, 2, 9, 5, 4, 0, 0, 8, 5, 0, 0, 2, 9, 6,
                    4, 0, 0, 9, 6, 0, 0, 3, 7, 7, 3, 0, 0, 13, 4, 4, 0, 0, 8, 6,
                    3, 0, 2, 4, 9, 7, 0,
                ])
            )
        );
    });
});

async function testSemanticTokens(
    docUri: vscode.Uri,
    expectedSemanticTokens: vscode.SemanticTokens
) {
    await activate(docUri);

    const actualSemanticTokens = (await vscode.commands.executeCommand(
        "vscode.provideDocumentSemanticTokens",
        docUri
    )) as vscode.SemanticTokens;

    assert.equal(
        actualSemanticTokens.data.length,
        expectedSemanticTokens.data.length
    );
    expectedSemanticTokens.data.forEach((expectedItem, i) => {
        const actualItem = actualSemanticTokens.data[i];
        assert.equal(actualItem, expectedItem);
    });
}
