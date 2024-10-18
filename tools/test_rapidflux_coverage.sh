#!/bin/bash

# TODO(rust-lang/rust#80549): Unreachable lines cannot yet be ignored in coverage calculation

UNREACHABLE=$(git grep --line-number --color "unreachable\!()" | cut -f1-2 -d:)
UNREACHABLE_COUNT=$(echo "$UNREACHABLE" | wc -l)

cargo llvm-cov nextest --package librapidflux --no-fail-fast --fail-uncovered-lines $UNREACHABLE_COUNT --show-missing-lines --skip-functions

echo -e "Ignored $UNREACHABLE_COUNT unreachable lines:\n$UNREACHABLE"

