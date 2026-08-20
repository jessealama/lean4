/-!
# What can actually be proved about `Float` now that it is not opaque?

Since `Float` is a `structure` wrapping `Float.Model` (a subtype of `UInt64`), every
`Float` operation with a logical model is a *definition the kernel can unfold*. This file
demonstrates what that buys, and where it stops.

Three levels are exercised:

* concrete claims about particular floats, decided by kernel reduction (`rfl` / `decide`);
* universally quantified claims that follow from *structural symmetry* of the model, such as
  commutativity of `+` and `*`, which hold for **all** floats including `NaN` and the signed
  zeroes;
* a universally quantified claim whose proof genuinely depends on the *numeric* hypotheses
  `0 < x < 1` — here, that `x + y` is not `NaN`.

Counterexamples to the "expected" algebraic laws (associativity, `x + 0 = x`,
commutativity of `==`) are included, since the point of a logical model is that it lets you
refute as well as prove.
-/

open Float.Model UnpackedFloat

/-- `Float.nan` postdates 4.33.0, so name the canonical `NaN` locally. -/
private def NaN : Float := 0.0 / 0.0

/-! ## Concrete facts are decided by kernel reduction -/

example : (0.1 : Float) + 0.2 ≠ 0.3 := by decide
example : (1.0 : Float) + 1.0 = 2.0 := by decide
example : (16.0 : Float) * 9.0 = 144.0 := by decide

-- `NaN` has a single logical representative: `ofBits` canonicalizes every `NaN` bit pattern.
example : Float.ofBits 0x7FF0000000000001 = NaN := by decide
example : (Float.ofBits 0x7FF0000000000001).toBits ≠ 0x7FF0000000000001 := by decide

/-! ## Commutativity of `+`, for every `Float`

The proof is by case analysis on the unpacked representation. The `add` clauses are
symmetric on the nose except for `min` on the exponents and `+` on the signed mantissas,
so the whole thing reduces to `Int.min_comm` and `Int.add_comm`.
-/

theorem UnpackedFloat.add_comm' (spec : Format) (x y : UnpackedFloat) :
    UnpackedFloat.add spec x y = UnpackedFloat.add spec y x := by
  cases x with
  | notANumber => cases y <;> rfl
  | infinity s₁ =>
    cases y with
    | infinity s₂ => cases s₁ <;> cases s₂ <;> rfl
    | _ => rfl
  | zero s₁ =>
    cases y with
    | zero s₂ => cases s₁ <;> cases s₂ <;> rfl
    | _ => rfl
  | finite s₁ m₁ e₁ h₁ =>
    cases y with
    | finite s₂ m₂ e₂ h₂ =>
      simp only [UnpackedFloat.add]
      rw [Int.min_comm e₂ e₁, Int.add_comm]
    | _ => rfl

protected theorem Float.Model.add_comm (a b : Float.Model) : a + b = b + a := by
  show Float.Model.add a b = Float.Model.add b a
  simp only [Float.Model.add, UnpackedFloat.add_comm']

protected theorem Float.add_comm (a b : Float) : a + b = b + a := by
  show Float.add a b = Float.add b a
  simp only [Float.add, Float.Model.add_comm]

-- No hypotheses are needed, so the statement restricted to the open unit interval is a
-- special case. `NaN` is included, because all `NaN`s are propositionally equal.
example (x y : Float) (_ : 0 < x) (_ : x < 1) (_ : 0 < y) (_ : y < 1) :
    x + y = y + x :=
  Float.add_comm x y

example (x : Float) : x + NaN = NaN + x := Float.add_comm ..

/-! ## Commutativity of `*`, likewise -/

theorem UnpackedFloat.mul_comm' (spec : Format) (x y : UnpackedFloat) :
    UnpackedFloat.mul spec x y = UnpackedFloat.mul spec y x := by
  have hs : ∀ s₁ s₂ : Sign, s₁ * s₂ = s₂ * s₁ := by
    intro s₁ s₂; cases s₁ <;> cases s₂ <;> rfl
  cases x with
  | notANumber => cases y <;> rfl
  | infinity s₁ =>
    cases y with
    | infinity s₂ => rw [UnpackedFloat.mul, UnpackedFloat.mul, hs]
    | finite s₂ => rw [UnpackedFloat.mul, UnpackedFloat.mul, hs]
    | _ => rfl
  | zero s₁ =>
    cases y with
    | zero s₂ => rw [UnpackedFloat.mul, UnpackedFloat.mul, hs]
    | finite s₂ => rw [UnpackedFloat.mul, UnpackedFloat.mul, hs]
    | _ => rfl
  | finite s₁ m₁ e₁ h₁ =>
    cases y with
    | infinity s₂ => rw [UnpackedFloat.mul, UnpackedFloat.mul, hs]
    | zero s₂ => rw [UnpackedFloat.mul, UnpackedFloat.mul, hs]
    | finite s₂ m₂ e₂ h₂ =>
      simp only [UnpackedFloat.mul]
      rw [hs, Nat.mul_comm, Int.add_comm]
    | _ => rfl

protected theorem Float.mul_comm (a b : Float) : a * b = b * a := by
  show Float.mul a b = Float.mul b a
  show Float.ofModel (Float.Model.mul _ _) = Float.ofModel (Float.Model.mul _ _)
  simp only [Float.Model.mul, UnpackedFloat.mul_comm']

/-! ## The laws that are false are refutable too

This is the other half of having a logical model: `decide` settles these without
`native_decide`, so the refutations do not rest on the compiler or the hardware.
-/

-- Addition is not associative.
example : (0.1 : Float) + 0.2 + 0.3 ≠ 0.1 + (0.2 + 0.3) := by decide

-- `0` is not a right unit: it maps `-0.0` to `0.0`, and those are distinct in the model.
example : (-0.0 : Float) + 0.0 = 0.0 := by decide
example : (-0.0 : Float) ≠ 0.0 := by decide

-- ... even though IEEE comparison identifies them.
example : ((-0.0 : Float) == 0.0) = true := by decide

-- `Float.add_comm` above is stated with `=`, not `==`, and that is essential: the `==`
-- version is false, because `NaN == NaN` is `false`.
example : (NaN + 1.0 == 1.0 + NaN) = false := by decide

-- IEEE comparison is not reflexive, but propositional equality is.
example : (NaN ≤ NaN) = false := by decide
example : NaN = NaN * 5.0 := by decide
example : Float.ofBits 0x7FF0000000000001 = Float.ofBits 0xFFFFFFFFFFFFFFFF := by decide

/-! ## A theorem that really does depend on `0 < x < 1`

Everything above followed from symmetry of the model. This section proves something whose
proof consumes the interval hypotheses: for `x, y ∈ (0, 1)`, the sum `x + y` is not `NaN`.

The work splits into two halves. First, the hypotheses pin down the shape of the unpacked
representation: `0 < x` rules out `NaN`, `-∞`, zero and negative finites, and `x < 1` rules
out `+∞`. Second, `NaN` is unreachable from a finite/finite addition: `UnpackedFloat.add`
routes through `normalize`, which only ever returns `zero` or `finite`, and `pack`/`unpack`
turns those into either a finite float or an infinity, never a `NaN`.
-/

section

theorem unpack_zero : (Float.toModel 0).unpack = .zero .positive := rfl

theorem unpack_one : (Float.toModel 1).unpack =
    .finite .positive 4503599627370496 (-52) (by decide) := rfl

/-- On `(0, 1)`, a `Float` unpacks to a positive finite float. -/
theorem mem_unit_interval {x : Float} (h0 : 0 < x) (h1 : x < 1) :
    ∃ m e h, x.toModel.unpack = .finite .positive m e h := by
  have h0' : UnpackedFloat.lt (.zero .positive) x.toModel.unpack = true := by
    rw [← unpack_zero]; exact of_decide_eq_true h0
  have h1' : UnpackedFloat.lt x.toModel.unpack
      (.finite .positive 4503599627370496 (-52) (by decide)) = true := by
    rw [← unpack_one]; exact of_decide_eq_true h1
  revert h0' h1'
  cases hx : x.toModel.unpack with
  | notANumber => simp [UnpackedFloat.lt, UnpackedFloat.compare]
  | infinity s => cases s <;> simp [UnpackedFloat.lt, UnpackedFloat.compare]
  | zero s => simp [UnpackedFloat.lt, UnpackedFloat.compare]
  | finite s m e h =>
    cases s
    · simp [UnpackedFloat.lt, UnpackedFloat.compare]
    · intro _ _; exact ⟨m, e, h, rfl⟩

theorem unpack_eq_nan_iff {spec : Format} {b : BitVec spec.numBits} :
    UnpackedFloat.unpack spec b = .notANumber ↔
      unpackExponent b = -1#_ ∧ unpackMantissa b ≠ 0#_ := by
  simp only [UnpackedFloat.unpack]
  split
  · split <;> simp_all
  · split
    · split <;> simp_all
    · simp_all

/-- Packing anything other than `NaN` and unpacking it again does not produce a `NaN`.
Overflow in `pack` yields an infinity, not a `NaN`. -/
theorem unpack_pack_ne_nan {spec : Format} {u : UnpackedFloat} (h : u ≠ .notANumber) :
    UnpackedFloat.unpack spec (pack spec u) ≠ .notANumber := by
  simp only [ne_eq, unpack_eq_nan_iff, not_and, Decidable.not_not]
  fun_cases pack spec u with
  | case1 => exact absurd rfl h
  | case2 => simp [packedInfinity]
  | case3 => simp [packedZero]
  | case4 => simp [packedInfinity]
  | case5 _ _ _ _ _ biasedExponent h₁ _ =>
    have hne : biasedExponent % 2 ^ spec.exponentBits ≠ 2 ^ spec.exponentBits - 1 := by
      rw [Nat.mod_eq_of_lt (by omega)]; omega
    simp [BitVec.neg_one_eq_allOnes, ← BitVec.toNat_inj, hne]
  | case6 =>
    have := spec.he
    have : 0 < 2 ^ spec.exponentBits := Nat.two_pow_pos _
    simp [BitVec.neg_one_eq_allOnes, ← BitVec.toNat_inj]
    omega

theorem roundWithAccuracy_ne_nan {spec : Format} {s : Sign} {m : Nat} {e : Int} {a : Accuracy} :
    roundWithAccuracy spec s m e a ≠ .notANumber := by
  simp only [roundWithAccuracy]
  split <;> simp

theorem normalize_ne_nan {spec : Format} {m e : Int} {s : Sign} :
    normalize spec m e s ≠ .notANumber := by
  simp only [normalize]
  split
  · exact roundWithAccuracy_ne_nan
  · simp
  · exact roundWithAccuracy_ne_nan

theorem model_pack_unpack (u : UnpackedFloat) :
    (Float.Model.pack u).unpack = UnpackedFloat.unpack Format.binary64 (pack Format.binary64 u) := by
  simp [Float.Model.unpack, Float.Model.pack]

/-- The sum of two floats in the open unit interval is not `NaN`. -/
theorem add_ne_nan_of_mem_unit {x y : Float}
    (hx0 : 0 < x) (hx1 : x < 1) (hy0 : 0 < y) (hy1 : y < 1) :
    (x + y).isNaN = false := by
  obtain ⟨m₁, e₁, h₁, hx⟩ := mem_unit_interval hx0 hx1
  obtain ⟨m₂, e₂, h₂, hy⟩ := mem_unit_interval hy0 hy1
  have : (x + y).toModel.unpack ≠ .notANumber := by
    show (Float.Model.pack (UnpackedFloat.add Format.binary64 _ _)).unpack ≠ _
    rw [model_pack_unpack, hx, hy]
    refine unpack_pack_ne_nan ?_
    simp only [UnpackedFloat.add]
    exact normalize_ne_nan
  show (x + y).toModel.unpack.isNaN = false
  cases hn : (x + y).toModel.unpack <;> simp_all [UnpackedFloat.isNaN]

-- The hypotheses are not decoration: dropping them makes the statement false.
example : (NaN + 1.0).isNaN = true := by decide
example : (1.0 / 0.0 + (-1.0) / 0.0 : Float).isNaN = true := by decide

end

-- Everything above is proved from the model; nothing rests on `native_decide` or `sorry`.
/-- info: 'Float.add_comm' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms Float.add_comm

/-- info: 'Float.mul_comm' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms Float.mul_comm

/-- info: 'add_ne_nan_of_mem_unit' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms add_ne_nan_of_mem_unit
