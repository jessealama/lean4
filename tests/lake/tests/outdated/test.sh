#!/usr/bin/env bash
source ../common.sh

./clean.sh

export ELAN_TOOLCHAIN=test

# Test: outdated without manifest should error
echo "# TEST: No manifest"
test_err "no manifest found" -f require.toml outdated

# Test: outdated after update (Reservoir package)
echo "# TEST: Reservoir package"
./clean.sh
test_run -f require.toml update --keep-toolchain
test_out "Cli" -f require.toml outdated

# Test: JSON output
echo "# TEST: JSON output"
test_out '"name":"Cli"' -f require.toml outdated --json

# Test: Filter by package name
echo "# TEST: Package filter"
test_out "Cli" -f require.toml outdated Cli

# Test: Filter by non-existent package (should show nothing)
echo "# TEST: Non-existent package filter"
test_not_out "Cli" -f require.toml outdated NonExistent

# Test: Git dependency
echo "# TEST: Git dependency"
./clean.sh
test_run -f git.toml update --keep-toolchain
test_out "Cli" -f git.toml outdated

# cleanup
./clean.sh
rm -f produced.out
