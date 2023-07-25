#!/usr/bin/env bash

export CODE_TESTS_PATH="$(pwd)/out/test"
export CODE_TESTS_WORKSPACE="$(pwd)/testFixture"

node "$(pwd)/out/test/runTest.js" 