/-!
Fixture for the `lake build --json` tests. The unused binder `x` makes
`linter.unusedVariables` fire, giving the JSON stream an entry with a known
kind, target, and position.
-/

def f (x : Nat) : Nat := 0
