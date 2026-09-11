#!/usr/bin/env bash
source ../common.sh

./clean.sh

# ---
# Tests `lake build --json`: log entries as newline-delimited JSON on stdout,
# progress on stderr.
#
# The JSON stream is captured in `produced.json`, never `produced.out`:
# `test_cmd_eq` redirects its command's stdout to `produced.out`, which would
# truncate the file before a `grep` inside the command could read it.
# ---

echo "# TEST: JSON log entries go to stdout"

echo '$' lake build --json
"$LAKE" build --json >produced.json 2>produced.err
cat produced.json
match_text '"kind":"linter.unusedVariables"' produced.json
match_text '"target":"Foo"' produced.json
match_text '"level":"warning"' produced.json
match_text '"fileName":"Foo.lean"' produced.json
no_match_text 'Built Foo' produced.json

echo "# TEST: Progress and the summary go to stderr"

cat produced.err
match_text 'Built Foo' produced.err
match_text 'Build completed successfully' produced.err
no_match_text '"kind"' produced.err

echo "# TEST: Every stdout line is a JSON object"

test_cmd_eq 0 sh -c 'grep -cv "^{.*}$" produced.json || true'

echo "# TEST: Lake entries carry no structured fields"

"$LAKE" build --json -v >produced.json 2>/dev/null
cat produced.json
match_text '"level":"trace"' produced.json
test_cmd_eq 0 sh -c 'grep "\"level\":\"trace\"" produced.json | grep -c "\"kind\"" || true'

echo "# TEST: Text mode is unaffected"

./clean.sh
test_out "Built Foo" build --no-ansi

echo "# TEST: Verbosity still filters the JSON stream"

./clean.sh
"$LAKE" build --json -q >produced.json 2>/dev/null
cat produced.json
no_match_text '"level":"trace"' produced.json
match_text '"level":"warning"' produced.json
