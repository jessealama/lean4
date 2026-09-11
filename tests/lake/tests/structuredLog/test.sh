#!/usr/bin/env bash
source ../common.sh

./clean.sh

# ---
# Tests that Lake preserves the structured fields of Lean messages in log entries.
# ---

TRACE=.lake/build/lib/lean/Foo.trace

echo "# TEST: Structure is captured in the trace"

test_out "not explicitly referenced" build
test_exp -f $TRACE
match_text '"kind": "linter.unusedVariables"' $TRACE
match_text '"fileName": "Foo.lean"' $TRACE
match_text '"data":' $TRACE
match_pat '"line": 7' $TRACE
