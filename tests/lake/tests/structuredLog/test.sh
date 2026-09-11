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

# Diff Lake's combined output against `$1` after stripping the job counter
# (`[2/3]`) and the timing (`(141ms)`) that Lake appends to captions when ANSI
# is off. Runs under `pipefail`, so a failing `lake` fails the helper.
build_diff() {
  expected=$1; shift
  echo '$' lake "$@"
  "$LAKE" "$@" 2>&1 | sed -E -e 's/^(.) \[[0-9]+\/[0-9]+\]/\1/' -e 's/ \([0-9.]+m?s\)$//' > produced.out
  cat produced.out
  diff -u --strip-trailing-cr "$expected" produced.out
}

echo "# TEST: Text output is unchanged"

./clean.sh
cat << 'EOF' > produced.expected
⚠ Built Foo
warning: Foo.lean:7:7: Variable name `x` is not explicitly referenced.

Hint: The binding can be removed (if unused) or named `_` (if used implicitly). Alternatively, prefix the name with `_` to silence this warning:
  [apply] _x

Note: This linter can be disabled with `set_option linter.unusedVariables false`
EOF
build_diff produced.expected build --no-ansi -q

echo "# TEST: Structure survives a replay from cache"

cat << 'EOF' > produced.expected
⚠ Replayed Foo
warning: Foo.lean:7:7: Variable name `x` is not explicitly referenced.

Hint: The binding can be removed (if unused) or named `_` (if used implicitly). Alternatively, prefix the name with `_` to silence this warning:
  [apply] _x

Note: This linter can be disabled with `set_option linter.unusedVariables false`
EOF
build_diff produced.expected build --no-ansi -q
match_text '"kind": "linter.unusedVariables"' $TRACE

echo "# TEST: Lake's own entries gain no fields"

# The module log has exactly two entries: the `lean` command line at trace
# level (a Lake-created entry) and the warning. Exactly one `"kind"` in the
# file proves the `?`-suffix omission on the Lake-created one.
test_cmd_eq 1 sh -c 'grep -c "\"kind\"" '"$TRACE"

echo "# TEST: Traces written before structured entries still parse"

if command -v jq > /dev/null; then # skip if no jq found
  ./clean.sh
  test_out "not explicitly referenced" build
  # Rewrite the fresh trace into the pre-change format: only `level` and
  # `message` on each log entry. `schemaVersion`, `depHash`, `inputs`, and
  # `outputs` stay as produced, so the trace is genuinely up to date and the
  # replay path actually runs.
  jq '.log |= map({level, message})' $TRACE > old.trace
  test_cmd cat old.trace
  cp old.trace $TRACE
  test_run build --no-build
  test_not_out "unknown trace format" build --no-build
  test_not_out "invalid trace" build --no-build
  test_out "not explicitly referenced" build
fi
