/-!
Fixture for the structured-log tests. The unused binder `x` makes
`linter.unusedVariables` fire, so the module's build log carries a Lean
message with a known `kind`, position, and body.
-/

def f (x : Nat) : Nat := 0
