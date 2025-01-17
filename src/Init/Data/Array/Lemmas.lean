/-
Copyright (c) 2022 Mario Carneiro. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mario Carneiro
-/
prelude
import Init.Data.Nat.Lemmas
import Init.Data.List.Impl
import Init.Data.List.Monadic
import Init.Data.List.Range
import Init.Data.List.Nat.TakeDrop
import Init.Data.List.Nat.Modify
import Init.Data.List.Nat.Erase
import Init.Data.List.Monadic
import Init.Data.List.OfFn
import Init.Data.Array.Mem
import Init.Data.Array.DecidableEq
import Init.TacticsExtra

/-!
## Theorems about `Array`.
-/


/-! ### Preliminaries about `Array` needed for `List.toArray` lemmas.

This section contains only the bare minimum lemmas about `Array`
that we need to write lemmas about `List.toArray`.
-/
namespace Array

@[simp] theorem getElem?_eq_getElem {a : Array α} {i : Nat} (h : i < a.size) : a[i]? = some a[i] :=
  getElem?_pos ..

@[simp] theorem getElem?_eq_none_iff {a : Array α} : a[i]? = none ↔ a.size ≤ i := by
  by_cases h : i < a.size
  · simp [getElem?_eq_getElem, h]
  · rw [getElem?_neg a i h]
    simp_all

@[simp] theorem get_eq_getElem (a : Array α) (i : Nat) (h) : a.get i h = a[i] := rfl

@[simp] theorem get!_eq_getElem! [Inhabited α] (a : Array α) (i : Nat) : a.get! i = a[i]! := by
  simp [getElem!_def, get!, getD]
  split <;> rename_i h
  · simp [getElem?_eq_getElem h]
  · simp [getElem?_eq_none_iff.2 (by simpa using h)]

@[simp] theorem mem_toArray {a : α} {l : List α} : a ∈ l.toArray ↔ a ∈ l := by
  simp [mem_def]

end Array

/-! ### Lemmas about `List.toArray`.

We prefer to pull `List.toArray` outwards.
-/
namespace List

open Array

theorem toArray_inj {a b : List α} (h : a.toArray = b.toArray) : a = b := by
  cases a with
  | nil => simpa using h
  | cons a as =>
    cases b with
    | nil => simp at h
    | cons b bs => simpa using h

@[simp] theorem size_toArrayAux {a : List α} {b : Array α} :
    (a.toArrayAux b).size = b.size + a.length := by
  simp [size]

@[simp] theorem push_toArray (l : List α) (a : α) : l.toArray.push a = (l ++ [a]).toArray := by
  apply ext'
  simp

/-- Unapplied variant of `push_toArray`, useful for monadic reasoning. -/
@[simp] theorem push_toArray_fun (l : List α) : l.toArray.push = fun a => (l ++ [a]).toArray := by
  funext a
  simp

@[simp] theorem isEmpty_toArray (l : List α) : l.toArray.isEmpty = l.isEmpty := by
  cases l <;> simp

@[simp] theorem toArray_singleton (a : α) : (List.singleton a).toArray = singleton a := rfl

@[simp] theorem back!_toArray [Inhabited α] (l : List α) : l.toArray.back! = l.getLast! := by
  simp only [back!, size_toArray, Array.get!_eq_getElem!, getElem!_toArray, getLast!_eq_getElem!]

@[simp] theorem back?_toArray (l : List α) : l.toArray.back? = l.getLast? := by
  simp [back?, List.getLast?_eq_getElem?]

@[simp] theorem set_toArray (l : List α) (i : Nat) (a : α) (h : i < l.length) :
    (l.toArray.set i a) = (l.set i a).toArray := rfl

@[simp] theorem forIn'_loop_toArray [Monad m] (l : List α) (f : (a : α) → a ∈ l.toArray → β → m (ForInStep β)) (i : Nat)
    (h : i ≤ l.length) (b : β) :
    Array.forIn'.loop l.toArray f i h b =
      forIn' (l.drop (l.length - i)) b (fun a m b => f a (by simpa using mem_of_mem_drop m) b) := by
  induction i generalizing l b with
  | zero =>
    simp [Array.forIn'.loop]
  | succ i ih =>
    simp only [Array.forIn'.loop, size_toArray, getElem_toArray, ih]
    have t : drop (l.length - (i + 1)) l = l[l.length - i - 1] :: drop (l.length - i) l := by
      simp only [Nat.sub_add_eq]
      rw [List.drop_sub_one (by omega), List.getElem?_eq_getElem (by omega)]
      simp only [Option.toList_some, singleton_append]
    simp [t]
    have t : l.length - 1 - i = l.length - i - 1 := by omega
    simp only [t]
    congr

@[simp] theorem forIn'_toArray [Monad m] (l : List α) (b : β) (f : (a : α) → a ∈ l.toArray → β → m (ForInStep β)) :
    forIn' l.toArray b f = forIn' l b (fun a m b => f a (mem_toArray.mpr m) b) := by
  change Array.forIn' _ _ _ = List.forIn' _ _ _
  rw [Array.forIn', forIn'_loop_toArray]
  simp

@[simp] theorem forIn_toArray [Monad m] (l : List α) (b : β) (f : α → β → m (ForInStep β)) :
    forIn l.toArray b f = forIn l b f := by
  simpa using forIn'_toArray l b fun a m b => f a b

theorem foldrM_toArray [Monad m] (f : α → β → m β) (init : β) (l : List α) :
    l.toArray.foldrM f init = l.foldrM f init := by
  rw [foldrM_eq_reverse_foldlM_toList]
  simp

theorem foldlM_toArray [Monad m] (f : β → α → m β) (init : β) (l : List α) :
    l.toArray.foldlM f init = l.foldlM f init := by
  rw [foldlM_toList]

theorem foldr_toArray (f : α → β → β) (init : β) (l : List α) :
    l.toArray.foldr f init = l.foldr f init := by
  rw [foldr_toList]

theorem foldl_toArray (f : β → α → β) (init : β) (l : List α) :
    l.toArray.foldl f init = l.foldl f init := by
  rw [foldl_toList]

/-- Variant of `foldrM_toArray` with a side condition for the `start` argument. -/
@[simp] theorem foldrM_toArray' [Monad m] (f : α → β → m β) (init : β) (l : List α)
    (h : start = l.toArray.size) :
    l.toArray.foldrM f init start 0 = l.foldrM f init := by
  subst h
  rw [foldrM_eq_reverse_foldlM_toList]
  simp

/-- Variant of `foldlM_toArray` with a side condition for the `stop` argument. -/
@[simp] theorem foldlM_toArray' [Monad m] (f : β → α → m β) (init : β) (l : List α)
    (h : stop = l.toArray.size) :
    l.toArray.foldlM f init 0 stop = l.foldlM f init := by
  subst h
  rw [foldlM_toList]

/-- Variant of `foldr_toArray` with a side condition for the `start` argument. -/
@[simp] theorem foldr_toArray' (f : α → β → β) (init : β) (l : List α)
    (h : start = l.toArray.size) :
    l.toArray.foldr f init start 0 = l.foldr f init := by
  subst h
  rw [foldr_toList]

/-- Variant of `foldl_toArray` with a side condition for the `stop` argument. -/
@[simp] theorem foldl_toArray' (f : β → α → β) (init : β) (l : List α)
    (h : stop = l.toArray.size) :
    l.toArray.foldl f init 0 stop = l.foldl f init := by
  subst h
  rw [foldl_toList]

@[simp] theorem append_toArray (l₁ l₂ : List α) :
    l₁.toArray ++ l₂.toArray = (l₁ ++ l₂).toArray := by
  apply ext'
  simp

@[simp] theorem push_append_toArray {as : Array α} {a : α} {bs : List α} : as.push a ++ bs.toArray = as ++ (a ::bs).toArray := by
  cases as
  simp

@[simp] theorem foldl_push {l : List α} {as : Array α} : l.foldl Array.push as = as ++ l.toArray := by
  induction l generalizing as <;> simp [*]

@[simp] theorem foldr_push {l : List α} {as : Array α} : l.foldr (fun a b => push b a) as = as ++ l.reverse.toArray := by
  rw [foldr_eq_foldl_reverse, foldl_push]

@[simp] theorem findSomeM?_toArray [Monad m] [LawfulMonad m] (f : α → m (Option β)) (l : List α) :
    l.toArray.findSomeM? f = l.findSomeM? f := by
  rw [Array.findSomeM?]
  simp only [bind_pure_comp, map_pure, forIn_toArray]
  induction l with
  | nil => simp
  | cons a l ih =>
    simp only [forIn_cons, LawfulMonad.bind_assoc, findSomeM?]
    congr
    ext1 (_|_) <;> simp [ih]

theorem findSomeRevM?_find_toArray [Monad m] [LawfulMonad m] (f : α → m (Option β)) (l : List α)
    (i : Nat) (h) :
    findSomeRevM?.find f l.toArray i h = (l.take i).reverse.findSomeM? f := by
  induction i generalizing l with
  | zero => simp [Array.findSomeRevM?.find.eq_def]
  | succ i ih =>
    rw [size_toArray] at h
    rw [Array.findSomeRevM?.find, take_succ, getElem?_eq_getElem (by omega)]
    simp only [ih, reverse_append]
    congr
    ext1 (_|_) <;> simp

-- This is not marked as `@[simp]` as later we simplify all occurrences of `findSomeRevM?`.
theorem findSomeRevM?_toArray [Monad m] [LawfulMonad m] (f : α → m (Option β)) (l : List α) :
    l.toArray.findSomeRevM? f = l.reverse.findSomeM? f := by
  simp [Array.findSomeRevM?, findSomeRevM?_find_toArray]

-- This is not marked as `@[simp]` as later we simplify all occurrences of `findRevM?`.
theorem findRevM?_toArray [Monad m] [LawfulMonad m] (f : α → m Bool) (l : List α) :
    l.toArray.findRevM? f = l.reverse.findM? f := by
  rw [Array.findRevM?, findSomeRevM?_toArray, findM?_eq_findSomeM?]

@[simp] theorem findM?_toArray [Monad m] [LawfulMonad m] (f : α → m Bool) (l : List α) :
    l.toArray.findM? f = l.findM? f := by
  rw [Array.findM?]
  simp only [bind_pure_comp, map_pure, forIn_toArray]
  induction l with
  | nil => simp
  | cons a l ih =>
    simp only [forIn_cons, LawfulMonad.bind_assoc, findM?]
    congr
    ext1 (_|_) <;> simp [ih]

@[simp] theorem findSome?_toArray (f : α → Option β) (l : List α) :
    l.toArray.findSome? f = l.findSome? f := by
  rw [Array.findSome?, ← findSomeM?_id, findSomeM?_toArray, Id.run]

@[simp] theorem find?_toArray (f : α → Bool) (l : List α) :
    l.toArray.find? f = l.find? f := by
  rw [Array.find?]
  simp only [Id.run, Id, Id.pure_eq, Id.bind_eq, forIn_toArray]
  induction l with
  | nil => simp
  | cons a l ih =>
    simp only [forIn_cons, Id.pure_eq, Id.bind_eq, find?]
    by_cases f a <;> simp_all

theorem isPrefixOfAux_toArray_succ [BEq α] (l₁ l₂ : List α) (hle : l₁.length ≤ l₂.length) (i : Nat) :
    Array.isPrefixOfAux l₁.toArray l₂.toArray hle (i + 1) =
      Array.isPrefixOfAux l₁.tail.toArray l₂.tail.toArray (by simp; omega) i := by
  rw [Array.isPrefixOfAux]
  conv => rhs; rw [Array.isPrefixOfAux]
  simp only [size_toArray, getElem_toArray, Bool.if_false_right, length_tail, getElem_tail]
  split <;> rename_i h₁ <;> split <;> rename_i h₂
  · rw [isPrefixOfAux_toArray_succ]
  · omega
  · omega
  · rfl

theorem isPrefixOfAux_toArray_succ' [BEq α] (l₁ l₂ : List α) (hle : l₁.length ≤ l₂.length) (i : Nat) :
    Array.isPrefixOfAux l₁.toArray l₂.toArray hle (i + 1) =
      Array.isPrefixOfAux (l₁.drop (i+1)).toArray (l₂.drop (i+1)).toArray (by simp; omega) 0 := by
  induction i generalizing l₁ l₂ with
  | zero => simp [isPrefixOfAux_toArray_succ]
  | succ i ih =>
    rw [isPrefixOfAux_toArray_succ, ih]
    simp

theorem isPrefixOfAux_toArray_zero [BEq α] (l₁ l₂ : List α) (hle : l₁.length ≤ l₂.length) :
    Array.isPrefixOfAux l₁.toArray l₂.toArray hle 0 =
      l₁.isPrefixOf l₂ := by
  rw [Array.isPrefixOfAux]
  match l₁, l₂ with
  | [], _ => rw [dif_neg] <;> simp
  | _::_, [] => simp at hle
  | a::l₁, b::l₂ =>
    simp [isPrefixOf_cons₂, isPrefixOfAux_toArray_succ', isPrefixOfAux_toArray_zero]

@[simp] theorem isPrefixOf_toArray [BEq α] (l₁ l₂ : List α) :
    l₁.toArray.isPrefixOf l₂.toArray = l₁.isPrefixOf l₂ := by
  rw [Array.isPrefixOf]
  split <;> rename_i h
  · simp [isPrefixOfAux_toArray_zero]
  · simp only [Bool.false_eq]
    induction l₁ generalizing l₂ with
    | nil => simp at h
    | cons a l₁ ih =>
      cases l₂ with
      | nil => simp
      | cons b l₂ =>
        simp only [isPrefixOf_cons₂, Bool.and_eq_false_imp]
        intro w
        rw [ih]
        simp_all

theorem zipWithAux_toArray_succ (as : List α) (bs : List β) (f : α → β → γ) (i : Nat) (cs : Array γ) :
    zipWithAux as.toArray bs.toArray f (i + 1) cs = zipWithAux as.tail.toArray bs.tail.toArray f i cs := by
  rw [zipWithAux]
  conv => rhs; rw [zipWithAux]
  simp only [size_toArray, getElem_toArray, length_tail, getElem_tail]
  split <;> rename_i h₁
  · split <;> rename_i h₂
    · rw [dif_pos (by omega), dif_pos (by omega), zipWithAux_toArray_succ]
    · rw [dif_pos (by omega)]
      rw [dif_neg (by omega)]
  · rw [dif_neg (by omega)]

theorem zipWithAux_toArray_succ' (as : List α) (bs : List β) (f : α → β → γ) (i : Nat) (cs : Array γ) :
    zipWithAux as.toArray bs.toArray f (i + 1) cs = zipWithAux (as.drop (i+1)).toArray (bs.drop (i+1)).toArray f 0 cs := by
  induction i generalizing as bs cs with
  | zero => simp [zipWithAux_toArray_succ]
  | succ i ih =>
    rw [zipWithAux_toArray_succ, ih]
    simp

theorem zipWithAux_toArray_zero (f : α → β → γ) (as : List α) (bs : List β) (cs : Array γ) :
    zipWithAux as.toArray bs.toArray f 0 cs = cs ++ (List.zipWith f as bs).toArray := by
  rw [Array.zipWithAux]
  match as, bs with
  | [], _ => simp
  | _, [] => simp
  | a :: as, b :: bs =>
    simp [zipWith_cons_cons, zipWithAux_toArray_succ', zipWithAux_toArray_zero, push_append_toArray]

@[simp] theorem zipWith_toArray (as : List α) (bs : List β) (f : α → β → γ) :
    Array.zipWith as.toArray bs.toArray f = (List.zipWith f as bs).toArray := by
  rw [Array.zipWith]
  simp [zipWithAux_toArray_zero]

@[simp] theorem zip_toArray (as : List α) (bs : List β) :
    Array.zip as.toArray bs.toArray = (List.zip as bs).toArray := by
  simp [Array.zip, zipWith_toArray, zip]

theorem zipWithAll_go_toArray (as : List α) (bs : List β) (f : Option α → Option β → γ) (i : Nat) (cs : Array γ) :
    zipWithAll.go f as.toArray bs.toArray i cs = cs ++ (List.zipWithAll f (as.drop i) (bs.drop i)).toArray := by
  unfold zipWithAll.go
  split <;> rename_i h
  · rw [zipWithAll_go_toArray]
    simp at h
    simp only [getElem?_toArray, push_append_toArray]
    if ha : i < as.length then
      if hb : i < bs.length then
        rw [List.drop_eq_getElem_cons ha, List.drop_eq_getElem_cons hb]
        simp only [ha, hb, getElem?_eq_getElem, zipWithAll_cons_cons]
      else
        simp only [Nat.not_lt] at hb
        rw [List.drop_eq_getElem_cons ha]
        rw [(drop_eq_nil_iff (l := bs)).mpr (by omega), (drop_eq_nil_iff (l := bs)).mpr (by omega)]
        simp only [zipWithAll_nil, map_drop, map_cons]
        rw [getElem?_eq_getElem ha]
        rw [getElem?_eq_none hb]
    else
      if hb : i < bs.length then
        simp only [Nat.not_lt] at ha
        rw [List.drop_eq_getElem_cons hb]
        rw [(drop_eq_nil_iff (l := as)).mpr (by omega), (drop_eq_nil_iff (l := as)).mpr (by omega)]
        simp only [nil_zipWithAll, map_drop, map_cons]
        rw [getElem?_eq_getElem hb]
        rw [getElem?_eq_none ha]
      else
        omega
  · simp only [size_toArray, Nat.not_lt] at h
    rw [drop_eq_nil_of_le (by omega), drop_eq_nil_of_le (by omega)]
    simp
  termination_by max as.length bs.length - i
  decreasing_by simp_wf; decreasing_trivial_pre_omega

@[simp] theorem zipWithAll_toArray (f : Option α → Option β → γ) (as : List α) (bs : List β) :
    Array.zipWithAll as.toArray bs.toArray f = (List.zipWithAll f as bs).toArray := by
  simp [Array.zipWithAll, zipWithAll_go_toArray]

@[simp] theorem toArray_appendList (l₁ l₂ : List α) :
    l₁.toArray ++ l₂ = (l₁ ++ l₂).toArray := by
  apply ext'
  simp

@[simp] theorem pop_toArray (l : List α) : l.toArray.pop = l.dropLast.toArray := by
  apply ext'
  simp

theorem takeWhile_go_succ (p : α → Bool) (a : α) (l : List α) (i : Nat) :
    takeWhile.go p (a :: l).toArray (i+1) r = takeWhile.go p l.toArray i r := by
  rw [takeWhile.go, takeWhile.go]
  simp only [size_toArray, length_cons, Nat.add_lt_add_iff_right, Array.get_eq_getElem,
    getElem_toArray, getElem_cons_succ]
  split
  rw [takeWhile_go_succ]
  rfl

theorem takeWhile_go_toArray (p : α → Bool) (l : List α) (i : Nat) :
    Array.takeWhile.go p l.toArray i r = r ++ (takeWhile p (l.drop i)).toArray := by
  induction l generalizing i r with
  | nil => simp [takeWhile.go]
  | cons a l ih =>
    rw [takeWhile.go]
    cases i with
    | zero =>
      simp [takeWhile_go_succ, ih, takeWhile_cons]
      split <;> simp
    | succ i =>
      simp only [size_toArray, length_cons, Nat.add_lt_add_iff_right, Array.get_eq_getElem,
        getElem_toArray, getElem_cons_succ, drop_succ_cons]
      split <;> rename_i h₁
      · rw [takeWhile_go_succ, ih]
        rw [← getElem_cons_drop_succ_eq_drop h₁, takeWhile_cons]
        split <;> simp_all
      · simp_all [drop_eq_nil_of_le]

@[simp] theorem takeWhile_toArray (p : α → Bool) (l : List α) :
    l.toArray.takeWhile p = (l.takeWhile p).toArray := by
  simp [Array.takeWhile, takeWhile_go_toArray]

end List


namespace Array

/-! ## Preliminaries -/

/-! ### toList -/

theorem toList_inj {a b : Array α} (h : a.toList = b.toList) : a = b := by
  cases a; cases b; simpa using h

/-! ### empty -/

@[simp] theorem empty_eq {xs : Array α} : #[] = xs ↔ xs = #[] := by
  cases xs <;> simp

/-! ### size -/

theorem eq_empty_of_size_eq_zero (h : l.size = 0) : l = #[] := by
  cases l
  simp_all

theorem ne_empty_of_size_eq_add_one (h : l.size = n + 1) : l ≠ #[] := by
  cases l
  simpa using List.ne_nil_of_length_eq_add_one h

theorem ne_empty_of_size_pos (h : 0 < l.size) : l ≠ #[] := by
  cases l
  simpa using List.ne_nil_of_length_pos h

@[simp] theorem size_eq_zero : l.size = 0 ↔ l = #[] :=
  ⟨eq_empty_of_size_eq_zero, fun h => h ▸ rfl⟩

theorem size_pos_of_mem {a : α} {l : Array α} (h : a ∈ l) : 0 < l.size := by
  cases l
  simp only [mem_toArray] at h
  simpa using List.length_pos_of_mem h

theorem exists_mem_of_size_pos {l : Array α} (h : 0 < l.size) : ∃ a, a ∈ l := by
  cases l
  simpa using List.exists_mem_of_length_pos h

theorem size_pos_iff_exists_mem {l : Array α} : 0 < l.size ↔ ∃ a, a ∈ l :=
  ⟨exists_mem_of_size_pos, fun ⟨_, h⟩ => size_pos_of_mem h⟩

theorem exists_mem_of_size_eq_add_one {l : Array α} (h : l.size = n + 1) : ∃ a, a ∈ l := by
  cases l
  simpa using List.exists_mem_of_length_eq_add_one h

theorem size_pos {l : Array α} : 0 < l.size ↔ l ≠ #[] :=
  Nat.pos_iff_ne_zero.trans (not_congr size_eq_zero)

theorem size_eq_one {l : Array α} : l.size = 1 ↔ ∃ a, l = #[a] := by
  cases l
  simpa using List.length_eq_one

/-! ### push -/

theorem push_ne_empty {a : α} {xs : Array α} : xs.push a ≠ #[] := by
  cases xs
  simp

@[simp] theorem push_ne_self {a : α} {xs : Array α} : xs.push a ≠ xs := by
  cases xs
  simp

@[simp] theorem ne_push_self {a : α} {xs : Array α} : xs ≠ xs.push a := by
  rw [ne_eq, eq_comm]
  simp

theorem back_eq_of_push_eq {a b : α} {xs ys : Array α} (h : xs.push a = ys.push b) : a = b := by
  cases xs
  cases ys
  simp only [List.push_toArray, mk.injEq] at h
  replace h := List.append_inj_right' h (by simp)
  simpa using h

theorem pop_eq_of_push_eq {a b : α} {xs ys : Array α} (h : xs.push a = ys.push b) : xs = ys := by
  cases xs
  cases ys
  simp at h
  replace h := List.append_inj_left' h (by simp)
  simp [h]

theorem push_inj_left {a : α} {xs ys : Array α} : xs.push a = ys.push a ↔ xs = ys :=
  ⟨pop_eq_of_push_eq, fun h => by simp [h]⟩

theorem push_inj_right {a b : α} {xs : Array α} : xs.push a = xs.push b ↔ a = b :=
  ⟨back_eq_of_push_eq, fun h => by simp [h]⟩

theorem push_eq_push {a b : α} {xs ys : Array α} : xs.push a = ys.push b ↔ a = b ∧ xs = ys := by
  constructor
  · intro h
    exact ⟨back_eq_of_push_eq h, pop_eq_of_push_eq h⟩
  · rintro ⟨rfl, rfl⟩
    rfl

theorem exists_push_of_ne_empty {xs : Array α} (h : xs ≠ #[]) :
    ∃ (ys : Array α) (a : α), xs = ys.push a := by
  rcases xs with ⟨xs⟩
  simp only [ne_eq, mk.injEq] at h
  exact ⟨(xs.take (xs.length - 1)).toArray, xs.getLast h, by simp⟩

theorem ne_empty_iff_exists_push {xs : Array α} :
    xs ≠ #[] ↔ ∃ (ys : Array α) (a : α), xs = ys.push a :=
  ⟨exists_push_of_ne_empty, fun ⟨_, _, eq⟩ => eq.symm ▸ push_ne_empty⟩

theorem exists_push_of_size_pos {xs : Array α} (h : 0 < xs.size) :
    ∃ (ys : Array α) (a : α), xs = ys.push a := by
  replace h : xs ≠ #[] := size_pos.mp h
  exact exists_push_of_ne_empty h

theorem size_pos_iff_exists_push {xs : Array α} :
    0 < xs.size ↔ ∃ (ys : Array α) (a : α), xs = ys.push a :=
  ⟨exists_push_of_size_pos, fun ⟨_, _, eq⟩ => by simp [eq]⟩

theorem exists_push_of_size_eq_add_one {xs : Array α} (h : xs.size = n + 1) :
    ∃ (ys : Array α) (a : α), xs = ys.push a :=
  exists_push_of_size_pos (by simp [h])

/-! ## L[i] and L[i]? -/

-- getElem?_eq_none_iff is above.

@[simp] theorem none_eq_getElem?_iff {a : Array α} {i : Nat} : none = a[i]? ↔ a.size ≤ i := by
  simp [eq_comm (a := none)]

theorem getElem?_eq_some_iff {a : Array α} : a[i]? = some b ↔ ∃ h : i < a.size, a[i] = b := by
  simp [getElem?_def]

theorem getElem?_eq_none {a : Array α} (h : a.size ≤ i) : a[i]? = none := by
  simp [getElem?_eq_none_iff, h]

-- getElem?_eq_getElem is above.

theorem some_eq_getElem?_iff {a : Array α} : some b = a[i]? ↔ ∃ h : i < a.size, a[i] = b := by
  rw [eq_comm, getElem?_eq_some_iff]

-- getElem?_eq_some_iff is above.

@[simp] theorem some_getElem_eq_getElem?_iff (a : Array α) (i : Nat) (h : i < a.size) :
    (some a[i] = a[i]?) ↔ True := by
  simp [h]

@[simp] theorem getElem?_eq_some_getElem_iff (a : Array α) (i : Nat) (h : i < a.size) :
    (a[i]? = some a[i]) ↔ True := by
  simp [h]

theorem getElem_eq_iff {a : Array α} {n : Nat} {h : n < a.size} : a[n] = x ↔ a[n]? = some x := by
  simp only [getElem?_eq_some_iff]
  exact ⟨fun w => ⟨h, w⟩, fun h => h.2⟩

theorem getElem_eq_getElem?_get (a : Array α) (i : Nat) (h : i < a.size) :
    a[i] = a[i]?.get (by simp [getElem?_eq_getElem, h]) := by
  simp [getElem_eq_iff]

@[simp] theorem getElem?_empty {n : Nat} : (#[] : Array α)[n]? = none := rfl

theorem getElem_push_lt (a : Array α) (x : α) (i : Nat) (h : i < a.size) :
    have : i < (a.push x).size := by simp [*, Nat.lt_succ_of_le, Nat.le_of_lt]
    (a.push x)[i] = a[i] := by
  simp only [push, ← getElem_toList, List.concat_eq_append, List.getElem_append_left, h]

@[simp] theorem getElem_push_eq (a : Array α) (x : α) : (a.push x)[a.size] = x := by
  simp only [push, ← getElem_toList, List.concat_eq_append]
  rw [List.getElem_append_right] <;> simp [← getElem_toList, Nat.zero_lt_one]

theorem getElem_push (a : Array α) (x : α) (i : Nat) (h : i < (a.push x).size) :
    (a.push x)[i] = if h : i < a.size then a[i] else x := by
  by_cases h' : i < a.size
  · simp [getElem_push_lt, h']
  · simp at h
    simp [getElem_push_lt, Nat.le_antisymm (Nat.le_of_lt_succ h) (Nat.ge_of_not_lt h')]

theorem getElem?_push {a : Array α} {x} : (a.push x)[i]? = if i = a.size then some x else a[i]? := by
  simp [getElem?_def, getElem_push]
  (repeat' split) <;> first | rfl | omega

@[simp] theorem getElem?_push_size {a : Array α} {x} : (a.push x)[a.size]? = some x := by
  simp [getElem?_push]

@[simp] theorem mem_push {a : Array α} {x y : α} : x ∈ a.push y ↔ x ∈ a ∨ x = y := by
  simp [mem_def]

theorem mem_push_self {a : Array α} {x : α} : x ∈ a.push x :=
  mem_push.2 (Or.inr rfl)

theorem mem_push_of_mem {a : Array α} {x : α} (y : α) (h : x ∈ a) : x ∈ a.push y :=
  mem_push.2 (Or.inl h)

theorem getElem_of_mem {a} {l : Array α} (h : a ∈ l) : ∃ (n : Nat) (h : n < l.size), l[n]'h = a := by
  cases l
  simp [List.getElem_of_mem (by simpa using h)]

theorem getElem?_of_mem {a} {l : Array α} (h : a ∈ l) : ∃ n : Nat, l[n]? = some a :=
  let ⟨n, _, e⟩ := getElem_of_mem h; ⟨n, e ▸ getElem?_eq_getElem _⟩

theorem mem_of_getElem? {l : Array α} {n : Nat} {a : α} (e : l[n]? = some a) : a ∈ l :=
  let ⟨_, e⟩ := getElem?_eq_some_iff.1 e; e ▸ getElem_mem ..

theorem mem_iff_getElem {a} {l : Array α} : a ∈ l ↔ ∃ (n : Nat) (h : n < l.size), l[n]'h = a :=
  ⟨getElem_of_mem, fun ⟨_, _, e⟩ => e ▸ getElem_mem ..⟩

theorem mem_iff_getElem? {a} {l : Array α} : a ∈ l ↔ ∃ n : Nat, l[n]? = some a := by
  simp [getElem?_eq_some_iff, mem_iff_getElem]

theorem forall_getElem {l : Array α} {p : α → Prop} :
    (∀ (n : Nat) h, p (l[n]'h)) ↔ ∀ a, a ∈ l → p a := by
  cases l; simp [List.forall_getElem]

theorem singleton_inj : #[a] = #[b] ↔ a = b := by
  simp

theorem singleton_eq_toArray_singleton (a : α) : #[a] = [a].toArray := rfl

@[simp] theorem singleton_def (v : α) : singleton v = #[v] := rfl

-- This is a duplicate of `List.toArray_toList`.
-- It's confusing to guess which namespace this theorem should live in,
-- so we provide both.
@[simp] theorem toArray_toList (a : Array α) : a.toList.toArray = a := rfl

@[simp] theorem length_toList {l : Array α} : l.toList.length = l.size := rfl

@[simp] theorem mkEmpty_eq (α n) : @mkEmpty α n = #[] := rfl

@[simp] theorem size_mk (as : List α) : (Array.mk as).size = as.length := by simp [size]

@[simp] theorem isEmpty_toList {l : Array α} : l.toList.isEmpty = l.isEmpty := by
  rcases l with ⟨_ | _⟩ <;> simp

theorem foldrM_push [Monad m] (f : α → β → m β) (init : β) (arr : Array α) (a : α) :
    (arr.push a).foldrM f init = f a init >>= arr.foldrM f := by
  simp only [foldrM_eq_reverse_foldlM_toList, push_toList, List.reverse_append, List.reverse_cons,
    List.reverse_nil, List.nil_append, List.singleton_append, List.foldlM_cons, List.foldlM_reverse]

/--
Variant of `foldrM_push` with `h : start = arr.size + 1`
rather than `(arr.push a).size` as the argument.
-/
@[simp] theorem foldrM_push' [Monad m] (f : α → β → m β) (init : β) (arr : Array α) (a : α)
    {start} (h : start = arr.size + 1) :
    (arr.push a).foldrM f init start = f a init >>= arr.foldrM f := by
  simp [← foldrM_push, h]

theorem foldr_push (f : α → β → β) (init : β) (arr : Array α) (a : α) :
    (arr.push a).foldr f init = arr.foldr f (f a init) := foldrM_push ..

/--
Variant of `foldr_push` with the `h : start = arr.size + 1`
rather than `(arr.push a).size` as the argument.
-/
@[simp] theorem foldr_push' (f : α → β → β) (init : β) (arr : Array α) (a : α) {start}
    (h : start = arr.size + 1) : (arr.push a).foldr f init start = arr.foldr f (f a init) :=
  foldrM_push' _ _ _ _ h

/-- A more efficient version of `arr.toList.reverse`. -/
@[inline] def toListRev (arr : Array α) : List α := arr.foldl (fun l t => t :: l) []

@[simp] theorem toListRev_eq (arr : Array α) : arr.toListRev = arr.toList.reverse := by
  rw [toListRev, ← foldl_toList, ← List.foldr_reverse, List.foldr_cons_nil]

theorem mapM_eq_foldlM [Monad m] [LawfulMonad m] (f : α → m β) (arr : Array α) :
    arr.mapM f = arr.foldlM (fun bs a => bs.push <$> f a) #[] := by
  rw [mapM, aux, ← foldlM_toList]; rfl
where
  aux (i r) :
      mapM.map f arr i r = (arr.toList.drop i).foldlM (fun bs a => bs.push <$> f a) r := by
    unfold mapM.map; split
    · rw [← List.getElem_cons_drop_succ_eq_drop ‹_›]
      simp only [aux (i + 1), map_eq_pure_bind, length_toList, List.foldlM_cons, bind_assoc,
        pure_bind]
      rfl
    · rw [List.drop_of_length_le (Nat.ge_of_not_lt ‹_›)]; rfl
  termination_by arr.size - i
  decreasing_by decreasing_trivial_pre_omega

@[simp] theorem toList_map (f : α → β) (arr : Array α) : (arr.map f).toList = arr.toList.map f := by
  rw [map, mapM_eq_foldlM]
  apply congrArg toList (foldl_toList (fun bs a => push bs (f a)) #[] arr).symm |>.trans
  have H (l arr) : List.foldl (fun bs a => push bs (f a)) arr l = ⟨arr.toList ++ l.map f⟩ := by
    induction l generalizing arr <;> simp [*]
  simp [H]

@[simp] theorem size_map (f : α → β) (arr : Array α) : (arr.map f).size = arr.size := by
  simp only [← length_toList]
  simp

@[simp] theorem mapM_empty [Monad m] (f : α → m β) : mapM f #[] = pure #[] := by
  rw [mapM, mapM.map]; rfl

@[simp] theorem map_empty (f : α → β) : map f #[] = #[] := mapM_empty f

@[simp] theorem appendList_nil (arr : Array α) : arr ++ ([] : List α) = arr := Array.ext' (by simp)

@[simp] theorem appendList_cons (arr : Array α) (a : α) (l : List α) :
    arr ++ (a :: l) = arr.push a ++ l := Array.ext' (by simp)

@[simp] theorem toList_appendList (arr : Array α) (l : List α) :
    (arr ++ l).toList = arr.toList ++ l := by
  cases arr
  simp

theorem foldl_toList_eq_flatMap (l : List α) (acc : Array β)
    (F : Array β → α → Array β) (G : α → List β)
    (H : ∀ acc a, (F acc a).toList = acc.toList ++ G a) :
    (l.foldl F acc).toList = acc.toList ++ l.flatMap G := by
  induction l generalizing acc <;> simp [*, List.flatMap]

theorem foldl_toList_eq_map (l : List α) (acc : Array β) (G : α → β) :
    (l.foldl (fun acc a => acc.push (G a)) acc).toList = acc.toList ++ l.map G := by
  induction l generalizing acc <;> simp [*]

theorem anyM_eq_anyM_loop [Monad m] (p : α → m Bool) (as : Array α) (start stop) :
    anyM p as start stop = anyM.loop p as (min stop as.size) (Nat.min_le_right ..) start := by
  simp only [anyM, Nat.min_def]; split <;> rfl

theorem anyM_stop_le_start [Monad m] (p : α → m Bool) (as : Array α) (start stop)
    (h : min stop as.size ≤ start) : anyM p as start stop = pure false := by
  rw [anyM_eq_anyM_loop, anyM.loop, dif_neg (Nat.not_lt.2 h)]

@[simp] theorem not_mem_empty (a : α) : ¬(a ∈ #[]) := by
  simp [mem_def]

/-! # uset -/

attribute [simp] uset

theorem size_uset (a : Array α) (v i h) : (uset a i v h).size = a.size := by simp

/-! # get -/

theorem getElem?_lt
    (a : Array α) {i : Nat} (h : i < a.size) : a[i]? = some a[i] := dif_pos h

theorem getElem?_ge
    (a : Array α) {i : Nat} (h : i ≥ a.size) : a[i]? = none := dif_neg (Nat.not_lt_of_le h)

@[simp] theorem get?_eq_getElem? (a : Array α) (i : Nat) : a.get? i = a[i]? := rfl

theorem getElem?_len_le (a : Array α) {i : Nat} (h : a.size ≤ i) : a[i]? = none := by
  simp [getElem?_ge, h]

theorem getD_get? (a : Array α) (i : Nat) (d : α) :
  Option.getD a[i]? d = if p : i < a.size then a[i]'p else d := by
  if h : i < a.size then
    simp [setIfInBounds, h, getElem?_def]
  else
    have p : i ≥ a.size := Nat.le_of_not_gt h
    simp [setIfInBounds, getElem?_len_le _ p, h]

@[simp] theorem getD_eq_get? (a : Array α) (n d) : a.getD n d = (a[n]?).getD d := by
  simp only [getD, get_eq_getElem, get?_eq_getElem?]; split <;> simp [getD_get?, *]

theorem get!_eq_getD [Inhabited α] (a : Array α) : a.get! n = a.getD n default := rfl

@[simp] theorem get!_eq_getElem? [Inhabited α] (a : Array α) (i : Nat) :
    a.get! i = (a.get? i).getD default := by
  by_cases p : i < a.size <;>
  simp only [get!_eq_getD, getD_eq_get?, getD_get?, p, get?_eq_getElem?]

/-! # set -/

@[simp] theorem getElem_set_eq (a : Array α) (i : Nat) (h : i < a.size) (v : α) {j : Nat}
      (eq : i = j) (p : j < (a.set i v).size) :
    (a.set i v)[j]'p = v := by
  cases a
  simp
  simp [set, ← getElem_toList, ←eq]

@[simp] theorem getElem_set_ne (a : Array α) (i : Nat) (h' : i < a.size) (v : α) {j : Nat}
    (pj : j < (a.set i v).size) (h : i ≠ j) :
    (a.set i v)[j]'pj = a[j]'(size_set a i v _ ▸ pj) := by
  simp only [set, ← getElem_toList, List.getElem_set_ne h]

theorem getElem_set (a : Array α) (i : Nat) (h' : i < a.size) (v : α) (j : Nat)
    (h : j < (a.set i v).size) :
    (a.set i v)[j]'h = if i = j then v else a[j]'(size_set a i v _ ▸ h) := by
  by_cases p : i = j <;> simp [p]

@[simp] theorem getElem?_set_eq (a : Array α) (i : Nat) (h : i < a.size) (v : α) :
    (a.set i v)[i]? = v := by simp [getElem?_lt, h]

@[simp] theorem getElem?_set_ne (a : Array α) (i : Nat) (h : i < a.size) {j : Nat} (v : α)
    (ne : i ≠ j) : (a.set i v)[j]? = a[j]? := by
  by_cases h : j < a.size <;> simp [getElem?_lt, getElem?_ge, Nat.ge_of_not_lt, ne, h]

/-! # setIfInBounds -/

@[simp] theorem set!_is_setIfInBounds : @set! = @setIfInBounds := rfl

@[simp] theorem size_setIfInBounds (a : Array α) (index : Nat) (val : α) :
    (Array.setIfInBounds a index val).size = a.size := by
  if h : index < a.size  then
    simp [setIfInBounds, h]
  else
    simp [setIfInBounds, h]

theorem getElem_setIfInBounds (a : Array α) (i : Nat) (v : α) (j : Nat)
    (hj : j < (setIfInBounds a i v).size) :
  (setIfInBounds a i v)[j]'hj = if i = j then v else a[j]'(by simpa using hj) := by
  simp only [setIfInBounds]
  split
  · simp [getElem_set]
  · simp only [size_setIfInBounds] at hj
    rw [if_neg]
    omega

@[simp] theorem getElem_setIfInBounds_eq (a : Array α) {i : Nat} (v : α) (h : _) :
    (setIfInBounds a i v)[i]'h = v := by
  simp at h
  simp only [setIfInBounds, h, ↓reduceDIte, getElem_set_eq]

@[simp] theorem getElem_setIfInBounds_ne (a : Array α) {i : Nat} (v : α) {j : Nat}
    (hj : j < (setIfInBounds a i v).size) (h : i ≠ j) :
    (setIfInBounds a i v)[j]'hj = a[j]'(by simpa using hj) := by
  simp [getElem_setIfInBounds, h]

@[simp]
theorem getElem?_setIfInBounds_eq (a : Array α) {i : Nat} (p : i < a.size) (v : α) :
    (a.setIfInBounds i v)[i]? = some v := by
  simp [getElem?_lt, p]

/-- Simplifies a normal form from `get!` -/
@[simp] theorem getD_get?_setIfInBounds (a : Array α) (i : Nat) (v d : α) :
    Option.getD (setIfInBounds a i v)[i]? d = if i < a.size then v else d := by
  by_cases h : i < a.size <;>
    simp [setIfInBounds, Nat.not_lt_of_le, h,  getD_get?]

/-! # ofFn -/

@[simp] theorem size_ofFn_go {n} (f : Fin n → α) (i acc) :
    (ofFn.go f i acc).size = acc.size + (n - i) := by
  if hin : i < n then
    unfold ofFn.go
    have : 1 + (n - (i + 1)) = n - i :=
      Nat.sub_sub .. ▸ Nat.add_sub_cancel' (Nat.le_sub_of_add_le (Nat.add_comm .. ▸ hin))
    rw [dif_pos hin, size_ofFn_go f (i+1), size_push, Nat.add_assoc, this]
  else
    have : n - i = 0 := Nat.sub_eq_zero_of_le (Nat.le_of_not_lt hin)
    unfold ofFn.go
    simp [hin, this]
termination_by n - i

@[simp] theorem size_ofFn (f : Fin n → α) : (ofFn f).size = n := by simp [ofFn]

theorem getElem_ofFn_go (f : Fin n → α) (i) {acc k}
    (hki : k < n) (hin : i ≤ n) (hi : i = acc.size)
    (hacc : ∀ j, ∀ hj : j < acc.size, acc[j] = f ⟨j, Nat.lt_of_lt_of_le hj (hi ▸ hin)⟩) :
    haveI : acc.size + (n - acc.size) = n := Nat.add_sub_cancel' (hi ▸ hin)
    (ofFn.go f i acc)[k]'(by simp [*]) = f ⟨k, hki⟩ := by
  unfold ofFn.go
  if hin : i < n then
    have : 1 + (n - (i + 1)) = n - i :=
      Nat.sub_sub .. ▸ Nat.add_sub_cancel' (Nat.le_sub_of_add_le (Nat.add_comm .. ▸ hin))
    simp only [dif_pos hin]
    rw [getElem_ofFn_go f (i+1) _ hin (by simp [*]) (fun j hj => ?hacc)]
    cases (Nat.lt_or_eq_of_le <| Nat.le_of_lt_succ (by simpa using hj)) with
    | inl hj => simp [getElem_push, hj, hacc j hj]
    | inr hj => simp [getElem_push, *]
  else
    simp [hin, hacc k (Nat.lt_of_lt_of_le hki (Nat.le_of_not_lt (hi ▸ hin)))]
termination_by n - i

@[simp] theorem getElem_ofFn (f : Fin n → α) (i : Nat) (h) :
    (ofFn f)[i] = f ⟨i, size_ofFn f ▸ h⟩ :=
  getElem_ofFn_go _ _ _ (by simp) (by simp) nofun

theorem getElem?_ofFn (f : Fin n → α) (i : Nat) :
    (ofFn f)[i]? = if h : i < n then some (f ⟨i, h⟩) else none := by
  simp [getElem?_def]

@[simp] theorem ofFn_zero (f : Fin 0 → α) : ofFn f = #[] := rfl

theorem ofFn_succ (f : Fin (n+1) → α) :
    ofFn f = (ofFn (fun (i : Fin n) => f i.castSucc)).push (f ⟨n, by omega⟩) := by
  ext i h₁ h₂
  · simp
  · simp [getElem_push]
    split <;> rename_i h₃
    · rfl
    · congr
      simp at h₁ h₂
      omega

/-! # mkArray -/

@[simp] theorem size_mkArray (n : Nat) (v : α) : (mkArray n v).size = n :=
  List.length_replicate ..

@[simp] theorem toList_mkArray (n : Nat) (v : α) : (mkArray n v).toList = List.replicate n v := rfl

theorem mkArray_eq_toArray_replicate (n : Nat) (v : α) : mkArray n v = (List.replicate n v).toArray := rfl

@[simp] theorem getElem_mkArray (n : Nat) (v : α) (h : i < (mkArray n v).size) :
    (mkArray n v)[i] = v := by simp [← getElem_toList]

theorem getElem?_mkArray (n : Nat) (v : α) (i : Nat) :
    (mkArray n v)[i]? = if i < n then some v else none := by
  simp [getElem?_def]

/-! # mem -/

@[simp] theorem mem_toList {a : α} {l : Array α} : a ∈ l.toList ↔ a ∈ l := mem_def.symm

theorem not_mem_nil (a : α) : ¬ a ∈ #[] := nofun

@[simp] theorem mem_dite_empty_left {x : α} [Decidable p] {l : ¬ p → Array α} :
    (x ∈ if h : p then #[] else l h) ↔ ∃ h : ¬ p, x ∈ l h := by
  split <;> simp_all

@[simp] theorem mem_dite_empty_right {x : α} [Decidable p] {l : p → Array α} :
    (x ∈ if h : p then l h else #[]) ↔ ∃ h : p, x ∈ l h := by
  split <;> simp_all

@[simp] theorem mem_ite_empty_left {x : α} [Decidable p] {l : Array α} :
    (x ∈ if p then #[] else l) ↔ ¬ p ∧ x ∈ l := by
  split <;> simp_all

@[simp] theorem mem_ite_empty_right {x : α} [Decidable p] {l : Array α} :
    (x ∈ if p then l else #[]) ↔ p ∧ x ∈ l := by
  split <;> simp_all

/-! # get lemmas -/

theorem lt_of_getElem {x : α} {a : Array α} {idx : Nat} {hidx : idx < a.size} (_ : a[idx] = x) :
    idx < a.size :=
  hidx

theorem getElem_fin_eq_getElem_toList (a : Array α) (i : Fin a.size) : a[i] = a.toList[i] := rfl

@[simp] theorem ugetElem_eq_getElem (a : Array α) {i : USize} (h : i.toNat < a.size) :
  a[i] = a[i.toNat] := rfl

theorem getElem?_size_le (a : Array α) (i : Nat) (h : a.size ≤ i) : a[i]? = none := by
  simp [getElem?_neg, h]

@[deprecated getElem?_size_le (since := "2024-10-21")] abbrev get?_len_le := @getElem?_size_le

theorem getElem_mem_toList (a : Array α) (h : i < a.size) : a[i] ∈ a.toList := by
  simp only [← getElem_toList, List.getElem_mem]

theorem get?_eq_get?_toList (a : Array α) (i : Nat) : a.get? i = a.toList.get? i := by
  simp [← getElem?_toList]

theorem get!_eq_get? [Inhabited α] (a : Array α) : a.get! n = (a.get? n).getD default := by
  simp only [get!_eq_getElem?, get?_eq_getElem?]

theorem back!_eq_back? [Inhabited α] (a : Array α) : a.back! = a.back?.getD default := by
  simp only [back!, get!_eq_getElem?, get?_eq_getElem?, back?]

@[simp] theorem back?_push (a : Array α) : (a.push x).back? = some x := by
  simp [back?, ← getElem?_toList]

@[simp] theorem back!_push [Inhabited α] (a : Array α) : (a.push x).back! = x := by
  simp [back!_eq_back?]

theorem mem_of_back?_eq_some {xs : Array α} {a : α} (h : xs.back? = some a) : a ∈ xs := by
  cases xs
  simpa using List.mem_of_getLast?_eq_some (by simpa using h)

theorem getElem?_push_lt (a : Array α) (x : α) (i : Nat) (h : i < a.size) :
    (a.push x)[i]? = some a[i] := by
  rw [getElem?_pos, getElem_push_lt]

@[deprecated getElem?_push_lt (since := "2024-10-21")] abbrev get?_push_lt := @getElem?_push_lt

theorem getElem?_push_eq (a : Array α) (x : α) : (a.push x)[a.size]? = some x := by
  rw [getElem?_pos, getElem_push_eq]

@[deprecated getElem?_push_eq (since := "2024-10-21")] abbrev get?_push_eq := @getElem?_push_eq

@[deprecated getElem?_push (since := "2024-10-21")] abbrev get?_push := @getElem?_push

@[simp] theorem getElem?_size {a : Array α} : a[a.size]? = none := by
  simp only [getElem?_def, Nat.lt_irrefl, dite_false]

@[deprecated getElem?_size (since := "2024-10-21")] abbrev get?_size := @getElem?_size

@[simp] theorem toList_set (a : Array α) (i v h) : (a.set i v).toList = a.toList.set i v := rfl

theorem get_set_eq (a : Array α) (i : Nat) (v : α) (h : i < a.size) :
    (a.set i v h)[i]'(by simp [h]) = v := by
  simp only [set, ← getElem_toList, List.getElem_set_self]

theorem get?_set_eq (a : Array α) (i : Nat) (v : α) (h : i < a.size) :
    (a.set i v)[i]? = v := by simp [getElem?_pos, h]

@[simp] theorem get?_set_ne (a : Array α) (i : Nat) (h' : i < a.size) {j : Nat} (v : α)
    (h : i ≠ j) : (a.set i v)[j]? = a[j]? := by
  by_cases j < a.size <;> simp [getElem?_pos, getElem?_neg, *]

theorem get?_set (a : Array α) (i : Nat) (h : i < a.size) (j : Nat) (v : α) :
    (a.set i v)[j]? = if i = j then some v else a[j]? := by
  if h : i = j then subst j; simp [*] else simp [*]

theorem get_set (a : Array α) (i : Nat) (hi : i < a.size) (j : Nat) (hj : j < a.size) (v : α) :
    (a.set i v)[j]'(by simp [*]) = if i = j then v else a[j] := by
  if h : i = j then subst j; simp [*] else simp [*]

@[simp] theorem get_set_ne (a : Array α) (i : Nat) (hi : i < a.size) {j : Nat} (v : α) (hj : j < a.size)
    (h : i ≠ j) : (a.set i v)[j]'(by simp [*]) = a[j] := by
  simp only [set, ← getElem_toList, List.getElem_set_ne h]

theorem set_set (a : Array α) (i : Nat) (h) (v v' : α) :
    (a.set i v h).set i v' (by simp [h]) = a.set i v' := by simp [set, List.set_set]

private theorem fin_cast_val (e : n = n') (i : Fin n) : e ▸ i = ⟨i.1, e ▸ i.2⟩ := by cases e; rfl

theorem swap_def (a : Array α) (i j : Nat) (hi hj) :
    a.swap i j hi hj = (a.set i a[j]).set j a[i] (by simpa using hj) := by
  simp [swap, fin_cast_val]

@[simp] theorem toList_swap (a : Array α) (i j : Nat) (hi hj) :
    (a.swap i j hi hj).toList = (a.toList.set i a[j]).set j a[i] := by simp [swap_def]

theorem getElem?_swap (a : Array α) (i j : Nat) (hi hj) (k : Nat) : (a.swap i j hi hj)[k]? =
    if j = k then some a[i] else if i = k then some a[j] else a[k]? := by
  simp [swap_def, get?_set, ← getElem_fin_eq_getElem_toList]

@[simp] theorem swapAt_def (a : Array α) (i : Nat) (v : α) (hi) :
    a.swapAt i v hi = (a[i], a.set i v) := rfl

@[simp] theorem size_swapAt (a : Array α) (i : Nat) (v : α) (hi) :
    (a.swapAt i v hi).2.size = a.size := by simp [swapAt_def]

@[simp]
theorem swapAt!_def (a : Array α) (i : Nat) (v : α) (h : i < a.size) :
    a.swapAt! i v = (a[i], a.set i v) := by simp [swapAt!, h]

@[simp] theorem size_swapAt! (a : Array α) (i : Nat) (v : α) :
    (a.swapAt! i v).2.size = a.size := by
  simp only [swapAt!]
  split
  · simp
  · rfl

@[simp] theorem toList_pop (a : Array α) : a.pop.toList = a.toList.dropLast := by simp [pop]

@[simp] theorem pop_empty : (#[] : Array α).pop = #[] := rfl

@[simp] theorem pop_push (a : Array α) : (a.push x).pop = a := by simp [pop]

@[simp] theorem getElem_pop (a : Array α) (i : Nat) (hi : i < a.pop.size) :
    a.pop[i] = a[i]'(Nat.lt_of_lt_of_le (a.size_pop ▸ hi) (Nat.sub_le _ _)) :=
  List.getElem_dropLast ..

theorem eq_push_pop_back!_of_size_ne_zero [Inhabited α] {as : Array α} (h : as.size ≠ 0) :
    as = as.pop.push as.back! := by
  apply ext
  · simp [Nat.sub_add_cancel (Nat.zero_lt_of_ne_zero h)]
  · intros i h h'
    if hlt : i < as.pop.size then
      rw [getElem_push_lt (h:=hlt), getElem_pop]
    else
      have heq : i = as.pop.size :=
        Nat.le_antisymm (size_pop .. ▸ Nat.le_pred_of_lt h) (Nat.le_of_not_gt hlt)
      cases heq; rw [getElem_push_eq, back!, ←size_pop, get!_eq_getD, getD, dif_pos h]; rfl

theorem eq_push_of_size_ne_zero {as : Array α} (h : as.size ≠ 0) :
    ∃ (bs : Array α) (c : α), as = bs.push c :=
  let _ : Inhabited α := ⟨as[0]⟩
  ⟨as.pop, as.back!, eq_push_pop_back!_of_size_ne_zero h⟩

theorem size_eq_length_toList (as : Array α) : as.size = as.toList.length := rfl

@[simp] theorem size_swapIfInBounds (a : Array α) (i j) :
    (a.swapIfInBounds i j).size = a.size := by unfold swapIfInBounds; split <;> (try split) <;> simp [size_swap]

@[deprecated size_swapIfInBounds (since := "2024-11-24")] abbrev size_swap! := @size_swapIfInBounds

@[simp] theorem size_reverse (a : Array α) : a.reverse.size = a.size := by
  let rec go (as : Array α) (i j) : (reverse.loop as i j).size = as.size := by
    rw [reverse.loop]
    if h : i < j then
      simp [(go · (i+1) ⟨j-1, ·⟩), h]
    else simp [h]
    termination_by j - i
  simp only [reverse]; split <;> simp [go]

@[simp] theorem size_range {n : Nat} : (range n).size = n := by
  induction n <;> simp [range]

@[simp] theorem toList_range (n : Nat) : (range n).toList = List.range n := by
  apply List.ext_getElem <;> simp [range]

@[simp]
theorem getElem_range {n : Nat} {x : Nat} (h : x < (Array.range n).size) : (Array.range n)[x] = x := by
  simp [← getElem_toList]

@[simp] theorem toList_reverse (a : Array α) : a.reverse.toList = a.toList.reverse := by
  let rec go (as : Array α) (i j hj)
      (h : i + j + 1 = a.size) (h₂ : as.size = a.size)
      (H : ∀ k, as.toList[k]? = if i ≤ k ∧ k ≤ j then a.toList[k]? else a.toList.reverse[k]?)
      (k : Nat) : (reverse.loop as i ⟨j, hj⟩).toList[k]? = a.toList.reverse[k]? := by
    rw [reverse.loop]; dsimp only; split <;> rename_i h₁
    · match j with | j+1 => ?_
      simp only [Nat.add_sub_cancel]
      rw [(go · (i+1) j)]
      · rwa [Nat.add_right_comm i]
      · simp [size_swap, h₂]
      · intro k
        rw [getElem?_toList, getElem?_swap]
        simp only [H, ← getElem_toList, ← List.getElem?_eq_getElem, Nat.le_of_lt h₁,
          ← getElem?_toList]
        split <;> rename_i h₂
        · simp only [← h₂, Nat.not_le.2 (Nat.lt_succ_self _), Nat.le_refl, and_false]
          exact (List.getElem?_reverse' (j+1) i (Eq.trans (by simp_arith) h)).symm
        split <;> rename_i h₃
        · simp only [← h₃, Nat.not_le.2 (Nat.lt_succ_self _), Nat.le_refl, false_and]
          exact (List.getElem?_reverse' i (j+1) (Eq.trans (by simp_arith) h)).symm
        simp only [Nat.succ_le, Nat.lt_iff_le_and_ne.trans (and_iff_left h₃),
          Nat.lt_succ.symm.trans (Nat.lt_iff_le_and_ne.trans (and_iff_left (Ne.symm h₂)))]
    · rw [H]; split <;> rename_i h₂
      · cases Nat.le_antisymm (Nat.not_lt.1 h₁) (Nat.le_trans h₂.1 h₂.2)
        cases Nat.le_antisymm h₂.1 h₂.2
        exact (List.getElem?_reverse' _ _ h).symm
      · rfl
    termination_by j - i
  simp only [reverse]
  split
  · match a with | ⟨[]⟩ | ⟨[_]⟩ => rfl
  · have := Nat.sub_add_cancel (Nat.le_of_not_le ‹_›)
    refine List.ext_getElem? <| go _ _ _ _ (by simp [this]) rfl fun k => ?_
    split
    · rfl
    · rename_i h
      simp only [← show k < _ + 1 ↔ _ from Nat.lt_succ (n := a.size - 1), this, Nat.zero_le,
        true_and, Nat.not_lt] at h
      rw [List.getElem?_eq_none_iff.2 ‹_›, List.getElem?_eq_none_iff.2 (a.toList.length_reverse ▸ ‹_›)]

/-! ### BEq -/

@[simp] theorem reflBEq_iff [BEq α] : ReflBEq (Array α) ↔ ReflBEq α := by
  constructor
  · intro h
    constructor
    intro a
    suffices (#[a] == #[a]) = true by
      simpa only [instBEq, isEqv, isEqvAux, Bool.and_true]
    simp
  · intro h
    constructor
    apply Array.isEqv_self_beq

@[simp] theorem lawfulBEq_iff [BEq α] : LawfulBEq (Array α) ↔ LawfulBEq α := by
  constructor
  · intro h
    constructor
    · intro a b h
      apply singleton_inj.1
      apply eq_of_beq
      simp only [instBEq, isEqv, isEqvAux]
      simpa
    · intro a
      suffices (#[a] == #[a]) = true by
        simpa only [instBEq, isEqv, isEqvAux, Bool.and_true]
      simp
  · intro h
    constructor
    · intro a b h
      obtain ⟨hs, hi⟩ := rel_of_isEqv h
      ext i h₁ h₂
      · exact hs
      · simpa using hi _ h₁
    · intro a
      apply Array.isEqv_self_beq

/-! ### take -/

@[simp] theorem size_take_loop (a : Array α) (n : Nat) : (take.loop n a).size = a.size - n := by
  induction n generalizing a with
  | zero => simp [take.loop]
  | succ n ih =>
    simp [take.loop, ih]
    omega

@[simp] theorem getElem_take_loop (a : Array α) (n : Nat) (i : Nat) (h : i < (take.loop n a).size) :
    (take.loop n a)[i] = a[i]'(by simp at h; omega) := by
  induction n generalizing a i with
  | zero => simp [take.loop]
  | succ n ih =>
    simp [take.loop, ih]

@[simp] theorem size_take (a : Array α) (n : Nat) : (a.take n).size = min n a.size  := by
  simp [take]
  omega

@[simp] theorem getElem_take (a : Array α) (n : Nat) (i : Nat) (h : i < (a.take n).size) :
    (a.take n)[i] = a[i]'(by simp at h; omega) := by
  simp [take]

@[simp] theorem toList_take (a : Array α) (n : Nat) : (a.take n).toList = a.toList.take n := by
  apply List.ext_getElem <;> simp

/-! ### forIn -/

@[simp] theorem forIn_toList [Monad m] (as : Array α) (b : β) (f : α → β → m (ForInStep β)) :
    forIn as.toList b f = forIn as b f := by
  cases as
  simp

@[simp] theorem forIn'_toList [Monad m] (as : Array α) (b : β) (f : (a : α) → a ∈ as.toList → β → m (ForInStep β)) :
    forIn' as.toList b f = forIn' as b (fun a m b => f a (mem_toList.mpr m) b) := by
  cases as
  simp

/-! ### foldl / foldr -/

@[simp] theorem foldlM_loop_empty [Monad m] (f : β → α → m β) (init : β) (i j : Nat) :
    foldlM.loop f #[] s h i j init = pure init := by
  unfold foldlM.loop; split
  · split
    · rfl
    · simp at h
      omega
  · rfl

@[simp] theorem foldlM_empty [Monad m] (f : β → α → m β) (init : β) :
    foldlM f init #[] start stop = return init := by
  simp [foldlM]

@[simp] theorem foldrM_fold_empty [Monad m] (f : α → β → m β) (init : β) (i j : Nat) (h) :
    foldrM.fold f #[] i j h init = pure init := by
  unfold foldrM.fold
  split <;> rename_i h₁
  · rfl
  · split <;> rename_i h₂
    · rfl
    · simp at h₂

@[simp] theorem foldrM_empty [Monad m] (f : α → β → m β) (init : β) :
    foldrM f init #[] start stop = return init := by
  simp [foldrM]

-- This proof is the pure version of `Array.SatisfiesM_foldlM` in Batteries,
-- reproduced to avoid a dependency on `SatisfiesM`.
theorem foldl_induction
    {as : Array α} (motive : Nat → β → Prop) {init : β} (h0 : motive 0 init) {f : β → α → β}
    (hf : ∀ i : Fin as.size, ∀ b, motive i.1 b → motive (i.1 + 1) (f b as[i])) :
    motive as.size (as.foldl f init) := by
  let rec go {i j b} (h₁ : j ≤ as.size) (h₂ : as.size ≤ i + j) (H : motive j b) :
    (motive as.size) (foldlM.loop (m := Id) f as as.size (Nat.le_refl _) i j b) := by
    unfold foldlM.loop; split
    · next hj =>
      split
      · cases Nat.not_le_of_gt (by simp [hj]) h₂
      · exact go hj (by rwa [Nat.succ_add] at h₂) (hf ⟨j, hj⟩ b H)
    · next hj => exact Nat.le_antisymm h₁ (Nat.ge_of_not_lt hj) ▸ H
  simpa [foldl, foldlM] using go (Nat.zero_le _) (Nat.le_refl _) h0

-- This proof is the pure version of `Array.SatisfiesM_foldrM` in Batteries,
-- reproduced to avoid a dependency on `SatisfiesM`.
theorem foldr_induction
    {as : Array α} (motive : Nat → β → Prop) {init : β} (h0 : motive as.size init) {f : α → β → β}
    (hf : ∀ i : Fin as.size, ∀ b, motive (i.1 + 1) b → motive i.1 (f as[i] b)) :
    motive 0 (as.foldr f init) := by
  let rec go {i b} (hi : i ≤ as.size) (H : motive i b) :
    (motive 0) (foldrM.fold (m := Id) f as 0 i hi b) := by
    unfold foldrM.fold; simp; split
    · next hi => exact (hi ▸ H)
    · next hi =>
      split; {simp at hi}
      · next i hi' =>
        exact go _ (hf ⟨i, hi'⟩ b H)
  simp [foldr, foldrM]; split; {exact go _ h0}
  · next h => exact (Nat.eq_zero_of_not_pos h ▸ h0)

@[congr]
theorem foldl_congr {as bs : Array α} (h₀ : as = bs) {f g : β → α → β} (h₁ : f = g)
     {a b : β} (h₂ : a = b) {start start' stop stop' : Nat} (h₃ : start = start') (h₄ : stop = stop') :
    as.foldl f a start stop = bs.foldl g b start' stop' := by
  congr

@[congr]
theorem foldr_congr {as bs : Array α} (h₀ : as = bs) {f g : α → β → β} (h₁ : f = g)
     {a b : β} (h₂ : a = b) {start start' stop stop' : Nat} (h₃ : start = start') (h₄ : stop = stop') :
    as.foldr f a start stop = bs.foldr g b start' stop' := by
  congr

theorem foldl_eq_foldlM (f : β → α → β) (b) (l : Array α) :
    l.foldl f b = l.foldlM (m := Id) f b := by
  cases l
  simp [List.foldl_eq_foldlM]

theorem foldr_eq_foldrM (f : α → β → β) (b) (l : Array α) :
    l.foldr f b = l.foldrM (m := Id) f b := by
  cases l
  simp [List.foldr_eq_foldrM]

@[simp] theorem id_run_foldlM (f : β → α → Id β) (b) (l : Array α) :
    Id.run (l.foldlM f b) = l.foldl f b := (foldl_eq_foldlM f b l).symm

@[simp] theorem id_run_foldrM (f : α → β → Id β) (b) (l : Array α) :
    Id.run (l.foldrM f b) = l.foldr f b := (foldr_eq_foldrM f b l).symm

theorem foldl_hom (f : α₁ → α₂) (g₁ : α₁ → β → α₁) (g₂ : α₂ → β → α₂) (l : Array β) (init : α₁)
    (H : ∀ x y, g₂ (f x) y = f (g₁ x y)) : l.foldl g₂ (f init) = f (l.foldl g₁ init) := by
  cases l
  simp
  rw [List.foldl_hom _ _ _ _ _ H]

theorem foldr_hom (f : β₁ → β₂) (g₁ : α → β₁ → β₁) (g₂ : α → β₂ → β₂) (l : Array α) (init : β₁)
    (H : ∀ x y, g₂ x (f y) = f (g₁ x y)) : l.foldr g₂ (f init) = f (l.foldr g₁ init) := by
  cases l
  simp
  rw [List.foldr_hom _ _ _ _ _ H]

/-! ### map -/

@[simp] theorem mem_map {f : α → β} {l : Array α} : b ∈ l.map f ↔ ∃ a, a ∈ l ∧ f a = b := by
  simp only [mem_def, toList_map, List.mem_map]

theorem exists_of_mem_map (h : b ∈ map f l) : ∃ a, a ∈ l ∧ f a = b := mem_map.1 h

theorem mem_map_of_mem (f : α → β) (h : a ∈ l) : f a ∈ map f l := mem_map.2 ⟨_, h, rfl⟩

theorem mapM_eq_mapM_toList [Monad m] [LawfulMonad m] (f : α → m β) (arr : Array α) :
    arr.mapM f = List.toArray <$> (arr.toList.mapM f) := by
  rw [mapM_eq_foldlM, ← foldlM_toList, ← List.foldrM_reverse]
  conv => rhs; rw [← List.reverse_reverse arr.toList]
  induction arr.toList.reverse with
  | nil => simp
  | cons a l ih => simp [ih]

@[simp] theorem toList_mapM [Monad m] [LawfulMonad m] (f : α → m β) (arr : Array α) :
    toList <$> arr.mapM f = arr.toList.mapM f := by
  simp [mapM_eq_mapM_toList]

theorem mapM_map_eq_foldl (as : Array α) (f : α → β) (i) :
    mapM.map (m := Id) f as i b = as.foldl (start := i) (fun r a => r.push (f a)) b := by
  unfold mapM.map
  split <;> rename_i h
  · simp only [Id.bind_eq]
    dsimp [foldl, Id.run, foldlM]
    rw [mapM_map_eq_foldl, dif_pos (by omega), foldlM.loop, dif_pos h]
    -- Calling `split` here gives a bad goal.
    have : size as - i = Nat.succ (size as - i - 1) := by omega
    rw [this]
    simp [foldl, foldlM, Id.run, Nat.sub_add_eq]
  · dsimp [foldl, Id.run, foldlM]
    rw [dif_pos (by omega), foldlM.loop, dif_neg h]
    rfl
termination_by as.size - i

theorem map_eq_foldl (as : Array α) (f : α → β) :
    as.map f = as.foldl (fun r a => r.push (f a)) #[] :=
  mapM_map_eq_foldl _ _ _

theorem map_induction (as : Array α) (f : α → β) (motive : Nat → Prop) (h0 : motive 0)
    (p : Fin as.size → β → Prop) (hs : ∀ i, motive i.1 → p i (f as[i]) ∧ motive (i+1)) :
    motive as.size ∧
      ∃ eq : (as.map f).size = as.size, ∀ i h, p ⟨i, h⟩ ((as.map f)[i]) := by
  have t := foldl_induction (as := as) (β := Array β)
    (motive := fun i arr => motive i ∧ arr.size = i ∧ ∀ i h2, p i arr[i.1])
    (init := #[]) (f := fun r a => r.push (f a)) ?_ ?_
  obtain ⟨m, eq, w⟩ := t
  · refine ⟨m, by simpa [map_eq_foldl] using eq, ?_⟩
    intro i h
    simp only [eq] at w
    specialize w ⟨i, h⟩ h
    simpa [map_eq_foldl] using w
  · exact ⟨h0, rfl, nofun⟩
  · intro i b ⟨m, ⟨eq, w⟩⟩
    refine ⟨?_, ?_, ?_⟩
    · exact (hs _ m).2
    · simp_all
    · intro j h
      simp at h ⊢
      by_cases h' : j < size b
      · rw [getElem_push]
        simp_all
      · rw [getElem_push, dif_neg h']
        simp only [show j = i by omega]
        exact (hs _ m).1

theorem map_spec (as : Array α) (f : α → β) (p : Fin as.size → β → Prop)
    (hs : ∀ i, p i (f as[i])) :
    ∃ eq : (as.map f).size = as.size, ∀ i h, p ⟨i, h⟩ ((as.map f)[i]) := by
  simpa using map_induction as f (fun _ => True) trivial p (by simp_all)

@[simp] theorem getElem_map (f : α → β) (as : Array α) (i : Nat) (h) :
    (as.map f)[i] = f (as[i]'(size_map .. ▸ h)) := by
  have := map_spec as f (fun i b => b = f (as[i]))
  simp only [implies_true, true_implies] at this
  obtain ⟨eq, w⟩ := this
  apply w
  simp_all

@[simp] theorem getElem?_map (f : α → β) (as : Array α) (i : Nat) :
    (as.map f)[i]? = as[i]?.map f := by
  simp [getElem?_def]

@[simp] theorem map_push {f : α → β} {as : Array α} {x : α} :
    (as.push x).map f = (as.map f).push (f x) := by
  ext
  · simp
  · simp only [getElem_map, getElem_push, size_map]
    split <;> rfl

@[simp] theorem map_pop {f : α → β} {as : Array α} :
    as.pop.map f = (as.map f).pop := by
  ext
  · simp
  · simp only [getElem_map, getElem_pop, size_map]

/-! ### modify -/

@[simp] theorem size_modify (a : Array α) (i : Nat) (f : α → α) : (a.modify i f).size = a.size := by
  unfold modify modifyM Id.run
  split <;> simp

theorem getElem_modify {as : Array α} {x i} (h : i < (as.modify x f).size) :
    (as.modify x f)[i] = if x = i then f (as[i]'(by simpa using h)) else as[i]'(by simpa using h) := by
  simp only [modify, modifyM, get_eq_getElem, Id.run, Id.pure_eq]
  split
  · simp only [Id.bind_eq, get_set _ _ _ _ (by simpa using h)]; split <;> simp [*]
  · rw [if_neg (mt (by rintro rfl; exact h) (by simp_all))]

@[simp] theorem toList_modify (as : Array α) (f : α → α) :
    (as.modify x f).toList = as.toList.modify f x := by
  apply List.ext_getElem
  · simp
  · simp [getElem_modify, List.getElem_modify]

theorem getElem_modify_self {as : Array α} {i : Nat} (f : α → α) (h : i < (as.modify i f).size) :
    (as.modify i f)[i] = f (as[i]'(by simpa using h)) := by
  simp [getElem_modify h]

theorem getElem_modify_of_ne {as : Array α} {i : Nat} (h : i ≠ j)
    (f : α → α) (hj : j < (as.modify i f).size) :
    (as.modify i f)[j] = as[j]'(by simpa using hj) := by
  simp [getElem_modify hj, h]

theorem getElem?_modify {as : Array α} {i : Nat} {f : α → α} {j : Nat} :
    (as.modify i f)[j]? = if i = j then as[j]?.map f else as[j]? := by
  simp only [getElem?_def, size_modify, getElem_modify, Option.map_dif]
  split <;> split <;> rfl

/-! ### filter -/

@[simp] theorem toList_filter (p : α → Bool) (l : Array α) :
    (l.filter p).toList = l.toList.filter p := by
  dsimp only [filter]
  rw [← foldl_toList]
  generalize l.toList = l
  suffices ∀ a, (List.foldl (fun r a => if p a = true then push r a else r) a l).toList =
      a.toList ++ List.filter p l by
    simpa using this #[]
  induction l with simp
  | cons => split <;> simp [*]

@[simp] theorem filter_filter (q) (l : Array α) :
    filter p (filter q l) = filter (fun a => p a && q a) l := by
  apply ext'
  simp only [toList_filter, List.filter_filter]

@[simp] theorem mem_filter : x ∈ filter p as ↔ x ∈ as ∧ p x := by
  simp only [mem_def, toList_filter, List.mem_filter]

theorem mem_of_mem_filter {a : α} {l} (h : a ∈ filter p l) : a ∈ l :=
  (mem_filter.mp h).1

@[congr]
theorem filter_congr {as bs : Array α} (h : as = bs)
    {f : α → Bool} {g : α → Bool} (h' : f = g) {start stop start' stop' : Nat}
    (h₁ : start = start') (h₂ : stop = stop') :
    filter f as start stop = filter g bs start' stop' := by
  congr

/-! ### filterMap -/

@[simp] theorem toList_filterMap (f : α → Option β) (l : Array α) :
    (l.filterMap f).toList = l.toList.filterMap f := by
  dsimp only [filterMap, filterMapM]
  rw [← foldlM_toList]
  generalize l.toList = l
  have this : ∀ a : Array β, (Id.run (List.foldlM (m := Id) ?_ a l)).toList =
    a.toList ++ List.filterMap f l := ?_
  exact this #[]
  induction l
  · simp_all [Id.run]
  · simp_all [Id.run, List.filterMap_cons]
    split <;> simp_all

@[simp] theorem mem_filterMap {f : α → Option β} {l : Array α} {b : β} :
    b ∈ filterMap f l ↔ ∃ a, a ∈ l ∧ f a = some b := by
  simp only [mem_def, toList_filterMap, List.mem_filterMap]

@[congr]
theorem filterMap_congr {as bs : Array α} (h : as = bs)
    {f : α → Option β} {g : α → Option β} (h' : f = g) {start stop start' stop' : Nat}
    (h₁ : start = start') (h₂ : stop = stop') :
    filterMap f as start stop = filterMap g bs start' stop' := by
  congr

/-! ### empty -/

theorem size_empty : (#[] : Array α).size = 0 := rfl

/-! ### append -/

theorem push_eq_append_singleton (as : Array α) (x) : as.push x = as ++ #[x] := rfl

@[simp] theorem mem_append {a : α} {s t : Array α} : a ∈ s ++ t ↔ a ∈ s ∨ a ∈ t := by
  simp only [mem_def, toList_append, List.mem_append]

theorem mem_append_left {a : α} {l₁ : Array α} (l₂ : Array α) (h : a ∈ l₁) : a ∈ l₁ ++ l₂ :=
  mem_append.2 (Or.inl h)

theorem mem_append_right {a : α} (l₁ : Array α) {l₂ : Array α} (h : a ∈ l₂) : a ∈ l₁ ++ l₂ :=
  mem_append.2 (Or.inr h)

@[simp] theorem size_append (as bs : Array α) : (as ++ bs).size = as.size + bs.size := by
  simp only [size, toList_append, List.length_append]

@[simp] theorem empty_append (as : Array α) : #[] ++ as = as := by
  cases as
  simp

@[simp] theorem append_empty (as : Array α) : as ++ #[] = as := by
  cases as
  simp

theorem getElem_append {as bs : Array α} (h : i < (as ++ bs).size) :
    (as ++ bs)[i] = if h' : i < as.size then as[i] else bs[i - as.size]'(by simp at h; omega) := by
  cases as; cases bs
  simp [List.getElem_append]

theorem getElem_append_left {as bs : Array α} {h : i < (as ++ bs).size} (hlt : i < as.size) :
    (as ++ bs)[i] = as[i] := by
  simp only [← getElem_toList]
  have h' : i < (as.toList ++ bs.toList).length := by rwa [← length_toList, toList_append] at h
  conv => rhs; rw [← List.getElem_append_left (bs := bs.toList) (h' := h')]
  apply List.get_of_eq; rw [toList_append]

theorem getElem_append_right {as bs : Array α} {h : i < (as ++ bs).size} (hle : as.size ≤ i)
    (hlt : i - as.size < bs.size := Nat.sub_lt_left_of_lt_add hle (size_append .. ▸ h)) :
    (as ++ bs)[i] = bs[i - as.size] := by
  simp only [← getElem_toList]
  have h' : i < (as.toList ++ bs.toList).length := by rwa [← length_toList, toList_append] at h
  conv => rhs; rw [← List.getElem_append_right (h₁ := hle) (h₂ := h')]
  apply List.get_of_eq; rw [toList_append]

theorem getElem?_append_left {as bs : Array α} {n : Nat} (hn : n < as.size) :
    (as ++ bs)[n]? = as[n]? := by
  have hn' : n < (as ++ bs).size := Nat.lt_of_lt_of_le hn <|
    size_append .. ▸ Nat.le_add_right ..
  simp_all [getElem?_eq_getElem, getElem_append]

theorem getElem?_append_right {as bs : Array α} {n : Nat} (h : as.size ≤ n) :
    (as ++ bs)[n]? = bs[n - as.size]? := by
  cases as
  cases bs
  simp at h
  simp [List.getElem?_append_right, h]

theorem getElem?_append {as bs : Array α} {n : Nat} :
    (as ++ bs)[n]? = if n < as.size then as[n]? else bs[n - as.size]? := by
  split <;> rename_i h
  · exact getElem?_append_left h
  · exact getElem?_append_right (by simpa using h)

@[simp] theorem toArray_eq_append_iff {xs : List α} {as bs : Array α} :
    xs.toArray = as ++ bs ↔ xs = as.toList ++ bs.toList := by
  cases as
  cases bs
  simp

@[simp] theorem append_eq_toArray_iff {as bs : Array α} {xs : List α} :
    as ++ bs = xs.toArray ↔ as.toList ++ bs.toList = xs := by
  cases as
  cases bs
  simp

/-! ### flatten -/

@[simp] theorem toList_flatten {l : Array (Array α)} :
    l.flatten.toList = (l.toList.map toList).flatten := by
  dsimp [flatten]
  simp only [← foldl_toList]
  generalize l.toList = l
  have : ∀ a : Array α, (List.foldl ?_ a l).toList = a.toList ++ ?_ := ?_
  exact this #[]
  induction l with
  | nil => simp
  | cons h => induction h.toList <;> simp [*]

theorem mem_flatten : ∀ {L : Array (Array α)}, a ∈ L.flatten ↔ ∃ l, l ∈ L ∧ a ∈ l := by
  simp only [mem_def, toList_flatten, List.mem_flatten, List.mem_map]
  intro l
  constructor
  · rintro ⟨_, ⟨s, m, rfl⟩, h⟩
    exact ⟨s, m, h⟩
  · rintro ⟨s, h₁, h₂⟩
    refine ⟨s.toList, ⟨⟨s, h₁, rfl⟩, h₂⟩⟩

/-! ### extract -/

theorem extract_loop_zero (as bs : Array α) (start : Nat) : extract.loop as 0 start bs = bs := by
  rw [extract.loop]; split <;> rfl

theorem extract_loop_succ (as bs : Array α) (size start : Nat) (h : start < as.size) :
    extract.loop as (size+1) start bs = extract.loop as size (start+1) (bs.push as[start]) := by
  rw [extract.loop, dif_pos h]; rfl

theorem extract_loop_of_ge (as bs : Array α) (size start : Nat) (h : start ≥ as.size) :
    extract.loop as size start bs = bs := by
  rw [extract.loop, dif_neg (Nat.not_lt_of_ge h)]

theorem extract_loop_eq_aux (as bs : Array α) (size start : Nat) :
    extract.loop as size start bs = bs ++ extract.loop as size start #[] := by
  induction size using Nat.recAux generalizing start bs with
  | zero => rw [extract_loop_zero, extract_loop_zero, append_nil]
  | succ size ih =>
    if h : start < as.size then
      rw [extract_loop_succ (h:=h), ih (bs.push _), push_eq_append_singleton]
      rw [extract_loop_succ (h:=h), ih (#[].push _), push_eq_append_singleton, nil_append]
      rw [append_assoc]
    else
      rw [extract_loop_of_ge (h:=Nat.le_of_not_lt h)]
      rw [extract_loop_of_ge (h:=Nat.le_of_not_lt h)]
      rw [append_nil]

theorem extract_loop_eq (as bs : Array α) (size start : Nat) (h : start + size ≤ as.size) :
  extract.loop as size start bs = bs ++ as.extract start (start + size) := by
  simp [extract]; rw [extract_loop_eq_aux, Nat.min_eq_left h, Nat.add_sub_cancel_left]

theorem size_extract_loop (as bs : Array α) (size start : Nat) :
    (extract.loop as size start bs).size = bs.size + min size (as.size - start) := by
  induction size using Nat.recAux generalizing start bs with
  | zero => rw [extract_loop_zero, Nat.zero_min, Nat.add_zero]
  | succ size ih =>
    if h : start < as.size then
      rw [extract_loop_succ (h:=h), ih, size_push, Nat.add_assoc, ←Nat.add_min_add_left,
        Nat.sub_succ, Nat.one_add, Nat.one_add, Nat.succ_pred_eq_of_pos (Nat.sub_pos_of_lt h)]
    else
      have h := Nat.le_of_not_gt h
      rw [extract_loop_of_ge (h:=h), Nat.sub_eq_zero_of_le h, Nat.min_zero, Nat.add_zero]

@[simp] theorem size_extract (as : Array α) (start stop : Nat) :
    (as.extract start stop).size = min stop as.size - start := by
  simp [extract]; rw [size_extract_loop, size_empty, Nat.zero_add, Nat.sub_min_sub_right,
    Nat.min_assoc, Nat.min_self]

theorem getElem_extract_loop_lt_aux (as bs : Array α) (size start : Nat) (hlt : i < bs.size) :
    i < (extract.loop as size start bs).size := by
  rw [size_extract_loop]
  apply Nat.lt_of_lt_of_le hlt
  exact Nat.le_add_right ..

theorem getElem_extract_loop_lt (as bs : Array α) (size start : Nat) (hlt : i < bs.size)
    (h := getElem_extract_loop_lt_aux as bs size start hlt) :
    (extract.loop as size start bs)[i] = bs[i] := by
  apply Eq.trans _ (getElem_append_left (bs:=extract.loop as size start #[]) hlt)
  · rw [size_append]; exact Nat.lt_of_lt_of_le hlt (Nat.le_add_right ..)
  · congr; rw [extract_loop_eq_aux]

theorem getElem_extract_loop_ge_aux (as bs : Array α) (size start : Nat) (hge : i ≥ bs.size)
    (h : i < (extract.loop as size start bs).size) : start + i - bs.size < as.size := by
  have h : i < bs.size + (as.size - start) := by
      apply Nat.lt_of_lt_of_le h
      rw [size_extract_loop]
      apply Nat.add_le_add_left
      exact Nat.min_le_right ..
  rw [Nat.add_sub_assoc hge]
  apply Nat.add_lt_of_lt_sub'
  exact Nat.sub_lt_left_of_lt_add hge h

theorem getElem_extract_loop_ge (as bs : Array α) (size start : Nat) (hge : i ≥ bs.size)
    (h : i < (extract.loop as size start bs).size)
    (h' := getElem_extract_loop_ge_aux as bs size start hge h) :
    (extract.loop as size start bs)[i] = as[start + i - bs.size] := by
  induction size using Nat.recAux generalizing start bs with
  | zero =>
    rw [size_extract_loop, Nat.zero_min, Nat.add_zero] at h
    omega
  | succ size ih =>
    have : start < as.size := by
      apply Nat.lt_of_le_of_lt (Nat.le_add_right start (i - bs.size))
      rwa [← Nat.add_sub_assoc hge]
    have : i < (extract.loop as size (start+1) (bs.push as[start])).size := by
      rwa [← extract_loop_succ]
    have heq : (extract.loop as (size+1) start bs)[i] =
        (extract.loop as size (start+1) (bs.push as[start]))[i] := by
      congr 1; rw [extract_loop_succ]
    rw [heq]
    if hi : bs.size = i then
      cases hi
      have h₁ : bs.size < (bs.push as[start]).size := by rw [size_push]; exact Nat.lt_succ_self ..
      have h₂ : bs.size < (extract.loop as size (start+1) (bs.push as[start])).size := by
        rw [size_extract_loop]; apply Nat.lt_of_lt_of_le h₁; exact Nat.le_add_right ..
      have h : (extract.loop as size (start + 1) (push bs as[start]))[bs.size] = as[start] := by
        rw [getElem_extract_loop_lt as (bs.push as[start]) size (start+1) h₁ h₂, getElem_push_eq]
      rw [h]; congr; rw [Nat.add_sub_cancel]
    else
      have hge : bs.size + 1 ≤ i := Nat.lt_of_le_of_ne hge hi
      rw [ih (bs.push as[start]) (start+1) ((size_push ..).symm ▸ hge)]
      congr 1; rw [size_push, Nat.add_right_comm, Nat.add_sub_add_right]

theorem getElem_extract_aux {as : Array α} {start stop : Nat} (h : i < (as.extract start stop).size) :
    start + i < as.size := by
  rw [size_extract] at h; apply Nat.add_lt_of_lt_sub'; apply Nat.lt_of_lt_of_le h
  apply Nat.sub_le_sub_right; apply Nat.min_le_right

@[simp] theorem getElem_extract {as : Array α} {start stop : Nat}
    (h : i < (as.extract start stop).size) :
    (as.extract start stop)[i] = as[start + i]'(getElem_extract_aux h) :=
  show (extract.loop as (min stop as.size - start) start #[])[i]
    = as[start + i]'(getElem_extract_aux h) by rw [getElem_extract_loop_ge]; rfl; exact Nat.zero_le _

theorem getElem?_extract {as : Array α} {start stop : Nat} :
    (as.extract start stop)[i]? = if i < min stop as.size - start then as[start + i]? else none := by
  simp only [getElem?_def, size_extract, getElem_extract]
  split
  · split
    · rfl
    · omega
  · rfl

@[simp] theorem toList_extract (as : Array α) (start stop : Nat) :
    (as.extract start stop).toList = (as.toList.drop start).take (stop - start) := by
  apply List.ext_getElem
  · simp only [length_toList, size_extract, List.length_take, List.length_drop]
    omega
  · intros n h₁ h₂
    simp

@[simp] theorem extract_all (as : Array α) : as.extract 0 as.size = as := by
  apply ext
  · rw [size_extract, Nat.min_self, Nat.sub_zero]
  · intros; rw [getElem_extract]; congr; rw [Nat.zero_add]

theorem extract_empty_of_stop_le_start (as : Array α) {start stop : Nat} (h : stop ≤ start) :
    as.extract start stop = #[] := by
  simp [extract]; rw [←Nat.sub_min_sub_right, Nat.sub_eq_zero_of_le h, Nat.zero_min,
    extract_loop_zero]

theorem extract_empty_of_size_le_start (as : Array α) {start stop : Nat} (h : as.size ≤ start) :
    as.extract start stop = #[] := by
  simp [extract]; rw [←Nat.sub_min_sub_right, Nat.sub_eq_zero_of_le h, Nat.min_zero,
    extract_loop_zero]

@[simp] theorem extract_empty (start stop : Nat) : (#[] : Array α).extract start stop = #[] :=
  extract_empty_of_size_le_start _ (Nat.zero_le _)

/-! ### any -/

theorem anyM_loop_cons [Monad m] (p : α → m Bool) (a : α) (as : List α) (stop start : Nat) (h : stop + 1 ≤ (a :: as).length) :
    anyM.loop p ⟨a :: as⟩ (stop + 1) h (start + 1) = anyM.loop p ⟨as⟩ stop (by simpa using h) start := by
  rw [anyM.loop]
  conv => rhs; rw [anyM.loop]
  split <;> rename_i h'
  · simp only [Nat.add_lt_add_iff_right] at h'
    rw [dif_pos h']
    rw [anyM_loop_cons]
    simp
  · rw [dif_neg]
    omega

@[simp] theorem anyM_toList [Monad m] (p : α → m Bool) (as : Array α) :
    as.toList.anyM p = as.anyM p :=
  match as with
  | ⟨[]⟩  => rfl
  | ⟨a :: as⟩ => by
    simp only [List.anyM, anyM, size_toArray, List.length_cons, Nat.le_refl, ↓reduceDIte]
    rw [anyM.loop, dif_pos (by omega)]
    congr 1
    funext b
    split
    · simp
    · simp only [Bool.false_eq_true, ↓reduceIte]
      rw [anyM_loop_cons]
      simpa [anyM] using anyM_toList p ⟨as⟩

-- Auxiliary for `any_iff_exists`.
theorem anyM_loop_iff_exists {p : α → Bool} {as : Array α} {start stop} (h : stop ≤ as.size) :
    anyM.loop (m := Id) p as stop h start = true ↔
      ∃ i : Fin as.size, start ≤ ↑i ∧ ↑i < stop ∧ p as[i] = true := by
  unfold anyM.loop
  split <;> rename_i h₁
  · dsimp
    split <;> rename_i h₂
    · simp only [true_iff]
      refine ⟨⟨start, by omega⟩, by dsimp; omega, by dsimp; omega, h₂⟩
    · rw [anyM_loop_iff_exists]
      constructor
      · rintro ⟨i, ge, lt, h⟩
        have : start ≠ i := by rintro rfl; omega
        exact ⟨i, by omega, lt, h⟩
      · rintro ⟨i, ge, lt, h⟩
        have : start ≠ i := by rintro rfl; erw [h] at h₂; simp_all
        exact ⟨i, by omega, lt, h⟩
  · simp
    omega
termination_by stop - start

-- This could also be proved from `SatisfiesM_anyM_iff_exists` in `Batteries.Data.Array.Init.Monadic`
theorem any_iff_exists {p : α → Bool} {as : Array α} {start stop} :
    as.any p start stop ↔ ∃ i : Fin as.size, start ≤ i.1 ∧ i.1 < stop ∧ p as[i] := by
  dsimp [any, anyM, Id.run]
  split
  · rw [anyM_loop_iff_exists]; rfl
  · rw [anyM_loop_iff_exists]
    constructor
    · rintro ⟨i, ge, _, h⟩
      exact ⟨i, by omega, by omega, h⟩
    · rintro ⟨i, ge, _, h⟩
      exact ⟨i, by omega, by omega, h⟩

theorem any_eq_true {p : α → Bool} {as : Array α} :
    as.any p ↔ ∃ i : Fin as.size, p as[i] := by simp [any_iff_exists, Fin.isLt]

theorem any_toList {p : α → Bool} (as : Array α) : as.toList.any p = as.any p := by
  rw [Bool.eq_iff_iff, any_eq_true, List.any_eq_true]; simp only [List.mem_iff_get]
  exact ⟨fun ⟨_, ⟨i, rfl⟩, h⟩ => ⟨i, h⟩, fun ⟨i, h⟩ => ⟨_, ⟨i, rfl⟩, h⟩⟩

/-! ### all -/

theorem allM_eq_not_anyM_not [Monad m] [LawfulMonad m] (p : α → m Bool) (as : Array α) :
    allM p as = (! ·) <$> anyM ((! ·) <$> p ·) as := by
  dsimp [allM, anyM]
  simp

@[simp] theorem allM_toList [Monad m] [LawfulMonad m] (p : α → m Bool) (as : Array α) :
    as.toList.allM p = as.allM p := by
  rw [allM_eq_not_anyM_not]
  rw [← anyM_toList]
  rw [List.allM_eq_not_anyM_not]

theorem all_eq_not_any_not (p : α → Bool) (as : Array α) (start stop) :
    as.all p start stop = !(as.any (!p ·) start stop) := by
  dsimp [all, allM]
  rfl

theorem all_iff_forall {p : α → Bool} {as : Array α} {start stop} :
    as.all p start stop ↔ ∀ i : Fin as.size, start ≤ i.1 ∧ i.1 < stop → p as[i] := by
  rw [all_eq_not_any_not]
  suffices ¬(as.any (!p ·) start stop = true) ↔
      ∀ i : Fin as.size, start ≤ i.1 ∧ i.1 < stop → p as[i] by
    simp_all
  rw [any_iff_exists]
  simp

theorem all_eq_true {p : α → Bool} {as : Array α} : as.all p ↔ ∀ i : Fin as.size, p as[i] := by
  simp [all_iff_forall, Fin.isLt]

theorem all_toList {p : α → Bool} (as : Array α) : as.toList.all p = as.all p := by
  rw [Bool.eq_iff_iff, all_eq_true, List.all_eq_true]; simp only [List.mem_iff_getElem]
  constructor
  · intro w i
    exact w as[i] ⟨i, i.2, getElem_toList i.2⟩
  · rintro w x ⟨r, h, rfl⟩
    rw [getElem_toList]
    exact w ⟨r, h⟩

theorem all_eq_true_iff_forall_mem {l : Array α} : l.all p ↔ ∀ x, x ∈ l → p x := by
  simp only [← all_toList, List.all_eq_true, mem_def]

/-! ### contains -/

theorem contains_def [DecidableEq α] {a : α} {as : Array α} : as.contains a ↔ a ∈ as := by
  rw [mem_def, contains, ← any_toList, List.any_eq_true]; simp [and_comm]

instance [DecidableEq α] (a : α) (as : Array α) : Decidable (a ∈ as) :=
  decidable_of_iff _ contains_def

/-! ### swap -/

@[simp] theorem getElem_swap_right (a : Array α) {i j : Nat} {hi hj} :
    (a.swap i j hi hj)[j]'(by simpa using hj) = a[i] := by
  simp [swap_def, getElem_set]

@[simp] theorem getElem_swap_left (a : Array α) {i j : Nat} {hi hj} :
    (a.swap i j hi hj)[i]'(by simpa using hi) = a[j] := by
  simp +contextual [swap_def, getElem_set]

@[simp] theorem getElem_swap_of_ne (a : Array α) {i j : Nat} {hi hj} (hp : p < a.size)
    (hi' : p ≠ i) (hj' : p ≠ j) : (a.swap i j hi hj)[p]'(a.size_swap .. |>.symm ▸ hp) = a[p] := by
  simp [swap_def, getElem_set, hi'.symm, hj'.symm]

theorem getElem_swap' (a : Array α) (i j : Nat) {hi hj} (k : Nat) (hk : k < a.size) :
    (a.swap i j hi hj)[k]'(by simp_all) = if k = i then a[j] else if k = j then a[i] else a[k] := by
  split
  · simp_all only [getElem_swap_left]
  · split <;> simp_all

theorem getElem_swap (a : Array α) (i j : Nat) {hi hj} (k : Nat) (hk : k < (a.swap i j).size) :
    (a.swap i j hi hj)[k] = if k = i then a[j] else if k = j then a[i] else a[k]'(by simp_all) := by
  apply getElem_swap'

@[simp] theorem swap_swap (a : Array α) {i j : Nat} (hi hj) :
    (a.swap i j hi hj).swap i j ((a.size_swap ..).symm ▸ hi) ((a.size_swap ..).symm ▸ hj) = a := by
  apply ext
  · simp only [size_swap]
  · intros
    simp only [getElem_swap]
    split
    · simp_all
    · split <;> simp_all

theorem swap_comm (a : Array α) {i j : Nat} {hi hj} : a.swap i j hi hj = a.swap j i hj hi := by
  apply ext
  · simp only [size_swap]
  · intros
    simp only [getElem_swap]
    split
    · split <;> simp_all
    · split <;> simp_all

/-! ### eraseIdx -/

theorem eraseIdx_eq_eraseIdxIfInBounds {a : Array α} {i : Nat} (h : i < a.size) :
    a.eraseIdx i h = a.eraseIdxIfInBounds i := by
  simp [eraseIdxIfInBounds, h]

/-! ### isPrefixOf -/

@[simp] theorem isPrefixOf_toList [BEq α] {as bs : Array α} :
    as.toList.isPrefixOf bs.toList = as.isPrefixOf bs := by
  cases as
  cases bs
  simp

/-! ### zipWith -/

@[simp] theorem toList_zipWith (f : α → β → γ) (as : Array α) (bs : Array β) :
    (Array.zipWith as bs f).toList = List.zipWith f as.toList bs.toList := by
  cases as
  cases bs
  simp

@[simp] theorem toList_zip (as : Array α) (bs : Array β) :
    (Array.zip as bs).toList = List.zip as.toList bs.toList := by
  simp [zip, toList_zipWith, List.zip]

@[simp] theorem toList_zipWithAll (f : Option α → Option β → γ) (as : Array α) (bs : Array β) :
    (Array.zipWithAll as bs f).toList = List.zipWithAll f as.toList bs.toList := by
  cases as
  cases bs
  simp

@[simp] theorem size_zipWith (as : Array α) (bs : Array β) (f : α → β → γ) :
    (as.zipWith bs f).size = min as.size bs.size := by
  rw [size_eq_length_toList, toList_zipWith, List.length_zipWith]

@[simp] theorem size_zip (as : Array α) (bs : Array β) :
    (as.zip bs).size = min as.size bs.size :=
  as.size_zipWith bs Prod.mk

@[simp] theorem getElem_zipWith (as : Array α) (bs : Array β) (f : α → β → γ) (i : Nat)
    (hi : i < (as.zipWith bs f).size) :
    (as.zipWith bs f)[i] = f (as[i]'(by simp at hi; omega)) (bs[i]'(by simp at hi; omega)) := by
  cases as
  cases bs
  simp

/-! ### findSomeM?, findM?, findSome?, find? -/

@[simp] theorem findSomeM?_toList [Monad m] [LawfulMonad m] (p : α → m (Option β)) (as : Array α) :
    as.toList.findSomeM? p = as.findSomeM? p := by
  cases as
  simp

@[simp] theorem findM?_toList [Monad m] [LawfulMonad m] (p : α → m Bool) (as : Array α) :
    as.toList.findM? p = as.findM? p := by
  cases as
  simp

@[simp] theorem findSome?_toList (p : α → Option β) (as : Array α) :
    as.toList.findSome? p = as.findSome? p := by
  cases as
  simp

@[simp] theorem find?_toList (p : α → Bool) (as : Array α) :
    as.toList.find? p = as.find? p := by
  cases as
  simp

end Array

open Array

namespace List

/-!
### More theorems about `List.toArray`, followed by an `Array` operation.

Our goal is to have `simp` "pull `List.toArray` outwards" as much as possible.
-/

@[simp] theorem toListRev_toArray (l : List α) : l.toArray.toListRev = l.reverse := by
  simp

@[simp] theorem take_toArray (l : List α) (n : Nat) : l.toArray.take n = (l.take n).toArray := by
  apply ext'
  simp

@[simp] theorem mapM_toArray [Monad m] [LawfulMonad m] (f : α → m β) (l : List α) :
    l.toArray.mapM f = List.toArray <$> l.mapM f := by
  simp only [← mapM'_eq_mapM, mapM_eq_foldlM]
  suffices ∀ init : Array β,
      foldlM (fun bs a => bs.push <$> f a) init l.toArray = (init ++ toArray ·) <$> mapM' f l by
    simpa using this #[]
  intro init
  induction l generalizing init with
  | nil => simp
  | cons a l ih =>
    simp only [foldlM_toArray] at ih
    rw [size_toArray, mapM'_cons, foldlM_toArray]
    simp [ih]

@[simp] theorem map_toArray (f : α → β) (l : List α) : l.toArray.map f = (l.map f).toArray := by
  apply ext'
  simp

@[simp] theorem uset_toArray (l : List α) (i : USize) (a : α) (h : i.toNat < l.toArray.size) :
    l.toArray.uset i a h = (l.set i.toNat a).toArray := by
  apply ext'
  simp

@[simp] theorem setIfInBounds_toArray (l : List α) (i : Nat) (a : α) :
    l.toArray.setIfInBounds i a  = (l.set i a).toArray := by
  apply ext'
  simp only [setIfInBounds]
  split
  · simp
  · simp_all [List.set_eq_of_length_le]

theorem anyM_toArray [Monad m] [LawfulMonad m] (p : α → m Bool) (l : List α) :
    l.toArray.anyM p = l.anyM p := by
  rw [← anyM_toList]

theorem any_toArray (p : α → Bool) (l : List α) : l.toArray.any p = l.any p := by
  rw [any_toList]

theorem allM_toArray [Monad m] [LawfulMonad m] (p : α → m Bool) (l : List α) :
    l.toArray.allM p = l.allM p := by
  rw [← allM_toList]

theorem all_toArray (p : α → Bool) (l : List α) : l.toArray.all p = l.all p := by
  rw [all_toList]

/-- Variant of `anyM_toArray` with a side condition on `stop`. -/
@[simp] theorem anyM_toArray' [Monad m] [LawfulMonad m] (p : α → m Bool) (l : List α)
    (h : stop = l.toArray.size) :
    l.toArray.anyM p 0 stop = l.anyM p := by
  subst h
  rw [← anyM_toList]

/-- Variant of `any_toArray` with a side condition on `stop`. -/
@[simp] theorem any_toArray' (p : α → Bool) (l : List α) (h : stop = l.toArray.size) :
    l.toArray.any p 0 stop = l.any p := by
  subst h
  rw [any_toList]

/-- Variant of `allM_toArray` with a side condition on `stop`. -/
@[simp] theorem allM_toArray' [Monad m] [LawfulMonad m] (p : α → m Bool) (l : List α)
    (h : stop = l.toArray.size) :
    l.toArray.allM p 0 stop = l.allM p := by
  subst h
  rw [← allM_toList]

/-- Variant of `all_toArray` with a side condition on `stop`. -/
@[simp] theorem all_toArray' (p : α → Bool) (l : List α) (h : stop = l.toArray.size) :
    l.toArray.all p 0 stop = l.all p := by
  subst h
  rw [all_toList]

@[simp] theorem swap_toArray (l : List α) (i j : Nat) {hi hj}:
    l.toArray.swap i j hi hj = ((l.set i l[j]).set j l[i]).toArray := by
  apply ext'
  simp

@[simp] theorem reverse_toArray (l : List α) : l.toArray.reverse = l.reverse.toArray := by
  apply ext'
  simp

@[simp] theorem modify_toArray (f : α → α) (l : List α) :
    l.toArray.modify i f = (l.modify f i).toArray := by
  apply ext'
  simp

@[simp] theorem filter_toArray' (p : α → Bool) (l : List α) (h : stop = l.toArray.size) :
    l.toArray.filter p 0 stop = (l.filter p).toArray := by
  subst h
  apply ext'
  rw [toList_filter]

@[simp] theorem filterMap_toArray' (f : α → Option β) (l : List α) (h : stop = l.toArray.size) :
    l.toArray.filterMap f 0 stop = (l.filterMap f).toArray := by
  subst h
  apply ext'
  rw [toList_filterMap]

theorem filter_toArray (p : α → Bool) (l : List α) :
    l.toArray.filter p = (l.filter p).toArray := by
  simp

theorem filterMap_toArray (f : α → Option β) (l : List α) :
    l.toArray.filterMap f = (l.filterMap f).toArray := by
  simp

@[simp] theorem flatten_toArray (l : List (List α)) : (l.toArray.map List.toArray).flatten = l.flatten.toArray := by
  apply ext'
  simp [Function.comp_def]

@[simp] theorem toArray_range (n : Nat) : (range n).toArray = Array.range n := by
  apply ext'
  simp

@[simp] theorem extract_toArray (l : List α) (start stop : Nat) :
    l.toArray.extract start stop = ((l.drop start).take (stop - start)).toArray := by
  apply ext'
  simp

@[simp] theorem toArray_ofFn (f : Fin n → α) : (ofFn f).toArray = Array.ofFn f := by
  ext <;> simp

@[simp] theorem eraseIdx_toArray (l : List α) (i : Nat) (h : i < l.toArray.size) :
    l.toArray.eraseIdx i h = (l.eraseIdx i).toArray := by
  rw [Array.eraseIdx]
  split <;> rename_i h'
  · rw [eraseIdx_toArray]
    simp only [swap_toArray, Fin.getElem_fin, toList_toArray, mk.injEq]
    rw [eraseIdx_set_gt (by simp), eraseIdx_set_eq]
    simp
  · simp at h h'
    have t : i = l.length - 1 := by omega
    simp [t]
termination_by l.length - i
decreasing_by
  rename_i h
  simp at h
  simp
  omega

@[simp] theorem eraseIdxIfInBounds_toArray (l : List α) (i : Nat) :
    l.toArray.eraseIdxIfInBounds i = (l.eraseIdx i).toArray := by
  rw [Array.eraseIdxIfInBounds]
  split
  · simp
  · simp_all [eraseIdx_eq_self.2]

end List

namespace Array

@[simp] theorem mapM_id {l : Array α} {f : α → Id β} : l.mapM f = l.map f := by
  induction l; simp_all

@[simp] theorem toList_ofFn (f : Fin n → α) : (Array.ofFn f).toList = List.ofFn f := by
  apply List.ext_getElem <;> simp

@[simp] theorem toList_takeWhile (p : α → Bool) (as : Array α) :
    (as.takeWhile p).toList = as.toList.takeWhile p := by
  induction as; simp

@[simp] theorem toList_eraseIdx (as : Array α) (i : Nat) (h : i < as.size) :
    (as.eraseIdx i h).toList = as.toList.eraseIdx i := by
  induction as
  simp

@[simp] theorem toList_eraseIdxIfInBounds (as : Array α) (i : Nat) :
    (as.eraseIdxIfInBounds i).toList = as.toList.eraseIdx i := by
  induction as
  simp

/-! ### map -/

@[simp] theorem map_map {f : α → β} {g : β → γ} {as : Array α} :
    (as.map f).map g = as.map (g ∘ f) := by
  cases as; simp

@[simp] theorem map_id_fun : map (id : α → α) = id := by
  funext l
  induction l <;> simp_all

/-- `map_id_fun'` differs from `map_id_fun` by representing the identity function as a lambda, rather than `id`. -/
@[simp] theorem map_id_fun' : map (fun (a : α) => a) = id := map_id_fun

-- This is not a `@[simp]` lemma because `map_id_fun` will apply.
theorem map_id (as : Array α) : map (id : α → α) as = as := by
  cases as <;> simp_all

/-- `map_id'` differs from `map_id` by representing the identity function as a lambda, rather than `id`. -/
-- This is not a `@[simp]` lemma because `map_id_fun'` will apply.
theorem map_id' (as : Array α) : map (fun (a : α) => a) as = as := map_id as

/-- Variant of `map_id`, with a side condition that the function is pointwise the identity. -/
theorem map_id'' {f : α → α} (h : ∀ x, f x = x) (as : Array α) : map f as = as := by
  simp [show f = id from funext h]

theorem array_array_induction (P : Array (Array α) → Prop) (h : ∀ (xss : List (List α)), P (xss.map List.toArray).toArray)
    (ass : Array (Array α)) : P ass := by
  specialize h (ass.toList.map toList)
  simpa [← toList_map, Function.comp_def, map_id] using h

theorem foldl_map (f : β₁ → β₂) (g : α → β₂ → α) (l : Array β₁) (init : α) :
    (l.map f).foldl g init = l.foldl (fun x y => g x (f y)) init := by
  cases l; simp [List.foldl_map]

theorem foldr_map (f : α₁ → α₂) (g : α₂ → β → β) (l : Array α₁) (init : β) :
    (l.map f).foldr g init = l.foldr (fun x y => g (f x) y) init := by
  cases l; simp [List.foldr_map]

theorem foldl_filterMap (f : α → Option β) (g : γ → β → γ) (l : Array α) (init : γ) :
    (l.filterMap f).foldl g init = l.foldl (fun x y => match f y with | some b => g x b | none => x) init := by
  cases l
  simp [List.foldl_filterMap]
  rfl

theorem foldr_filterMap (f : α → Option β) (g : β → γ → γ) (l : Array α) (init : γ) :
    (l.filterMap f).foldr g init = l.foldr (fun x y => match f x with | some b => g b y | none => y) init := by
  cases l
  simp [List.foldr_filterMap]
  rfl

theorem foldl_map' (g : α → β) (f : α → α → α) (f' : β → β → β) (a : α) (l : Array α)
    (h : ∀ x y, f' (g x) (g y) = g (f x y)) :
    (l.map g).foldl f' (g a) = g (l.foldl f a) := by
  cases l
  simp
  rw [List.foldl_map' _ _ _ _ _ h]

theorem foldr_map' (g : α → β) (f : α → α → α) (f' : β → β → β) (a : α) (l : List α)
    (h : ∀ x y, f' (g x) (g y) = g (f x y)) :
    (l.map g).foldr f' (g a) = g (l.foldr f a) := by
  cases l
  simp
  rw [List.foldr_map' _ _ _ _ _ h]

/-! ### flatten -/

@[simp] theorem flatten_empty : flatten (#[] : Array (Array α)) = #[] := rfl

@[simp] theorem flatten_toArray_map_toArray (xss : List (List α)) :
    (xss.map List.toArray).toArray.flatten = xss.flatten.toArray := by
  simp [flatten]
  suffices ∀ as, List.foldl (fun r a => r ++ a) as (List.map List.toArray xss) = as ++ xss.flatten.toArray by
    simpa using this #[]
  intro as
  induction xss generalizing as with
  | nil => simp
  | cons xs xss ih => simp [ih]

/-! ### reverse -/

@[simp] theorem mem_reverse {x : α} {as : Array α} : x ∈ as.reverse ↔ x ∈ as := by
  cases as
  simp

@[simp] theorem getElem_reverse (as : Array α) (i : Nat) (hi : i < as.reverse.size) :
    (as.reverse)[i] = as[as.size - 1 - i]'(by simp at hi; omega) := by
  cases as
  simp [Array.getElem_reverse]

/-! ### findSomeRevM?, findRevM?, findSomeRev?, findRev? -/

@[simp] theorem findSomeRevM?_eq_findSomeM?_reverse
    [Monad m] [LawfulMonad m] (f : α → m (Option β)) (as : Array α) :
    as.findSomeRevM? f = as.reverse.findSomeM? f := by
  cases as
  rw [List.findSomeRevM?_toArray]
  simp

@[simp] theorem findRevM?_eq_findM?_reverse
    [Monad m] [LawfulMonad m] (f : α → m Bool) (as : Array α) :
    as.findRevM? f = as.reverse.findM? f := by
  cases as
  rw [List.findRevM?_toArray]
  simp

@[simp] theorem findSomeRev?_eq_findSome?_reverse (f : α → Option β) (as : Array α) :
    as.findSomeRev? f = as.reverse.findSome? f := by
  cases as
  simp [findSomeRev?, Id.run]

@[simp] theorem findRev?_eq_find?_reverse (f : α → Bool) (as : Array α) :
    as.findRev? f = as.reverse.find? f := by
  cases as
  simp [findRev?, Id.run]

/-! ### unzip -/

@[simp] theorem fst_unzip (as : Array (α × β)) : (Array.unzip as).fst = as.map Prod.fst := by
  simp only [unzip]
  rcases as with ⟨as⟩
  simp only [List.foldl_toArray']
  rw [← List.foldl_hom (f := Prod.fst) (g₂ := fun bs x => bs.push x.1) (H := by simp), ← List.foldl_map]
  simp

@[simp] theorem snd_unzip (as : Array (α × β)) : (Array.unzip as).snd = as.map Prod.snd := by
  simp only [unzip]
  rcases as with ⟨as⟩
  simp only [List.foldl_toArray']
  rw [← List.foldl_hom (f := Prod.snd) (g₂ := fun bs x => bs.push x.2) (H := by simp), ← List.foldl_map]
  simp

end Array

namespace List

@[simp] theorem unzip_toArray (as : List (α × β)) :
    as.toArray.unzip = Prod.map List.toArray List.toArray as.unzip := by
  ext1 <;> simp

end List

namespace Array

@[simp] theorem toList_fst_unzip (as : Array (α × β)) :
    as.unzip.1.toList = as.toList.unzip.1 := by
  cases as
  simp

@[simp] theorem toList_snd_unzip (as : Array (α × β)) :
    as.unzip.2.toList = as.toList.unzip.2 := by
  cases as
  simp

@[simp] theorem flatMap_empty {β} (f : α → Array β) : (#[] : Array α).flatMap f = #[] := rfl

@[simp] theorem flatMap_toArray_cons {β} (f : α → Array β) (a : α) (as : List α) :
    (a :: as).toArray.flatMap f = f a ++ as.toArray.flatMap f := by
  simp [flatMap]
  suffices ∀ cs, List.foldl (fun bs a => bs ++ f a) (f a ++ cs) as =
      f a ++ List.foldl (fun bs a => bs ++ f a) cs as by
    erw [empty_append] -- Why doesn't this work via `simp`?
    simpa using this #[]
  intro cs
  induction as generalizing cs <;> simp_all

@[simp] theorem flatMap_toArray {β} (f : α → Array β) (as : List α) :
    as.toArray.flatMap f = (as.flatMap (fun a => (f a).toList)).toArray := by
  induction as with
  | nil => simp
  | cons a as ih =>
    apply ext'
    simp [ih]


end Array

/-! ### Deprecations -/

namespace List

@[deprecated back!_toArray (since := "2024-10-31")] abbrev back_toArray := @back!_toArray

@[deprecated setIfInBounds_toArray (since := "2024-11-24")] abbrev setD_toArray := @setIfInBounds_toArray

end List

namespace Array

@[deprecated foldl_toList_eq_flatMap (since := "2024-10-16")]
abbrev foldl_toList_eq_bind := @foldl_toList_eq_flatMap

@[deprecated foldl_toList_eq_flatMap (since := "2024-10-16")]
abbrev foldl_data_eq_bind := @foldl_toList_eq_flatMap

@[deprecated getElem_mem (since := "2024-10-17")]
abbrev getElem?_mem := @getElem_mem

@[deprecated getElem_fin_eq_getElem_toList (since := "2024-10-17")]
abbrev getElem_fin_eq_toList_get := @getElem_fin_eq_getElem_toList

@[deprecated "Use reverse direction of `getElem?_toList`" (since := "2024-10-17")]
abbrev getElem?_eq_toList_getElem? := @getElem?_toList

@[deprecated get?_eq_get?_toList (since := "2024-10-17")]
abbrev get?_eq_toList_get? := @get?_eq_get?_toList

@[deprecated getElem?_swap (since := "2024-10-17")] abbrev get?_swap := @getElem?_swap

@[deprecated getElem_push (since := "2024-10-21")] abbrev get_push := @getElem_push
@[deprecated getElem_push_lt (since := "2024-10-21")] abbrev get_push_lt := @getElem_push_lt
@[deprecated getElem_push_eq (since := "2024-10-21")] abbrev get_push_eq := @getElem_push_eq

@[deprecated back!_eq_back? (since := "2024-10-31")] abbrev back_eq_back? := @back!_eq_back?
@[deprecated back!_push (since := "2024-10-31")] abbrev back_push := @back!_push
@[deprecated eq_push_pop_back!_of_size_ne_zero (since := "2024-10-31")]
abbrev eq_push_pop_back_of_size_ne_zero := @eq_push_pop_back!_of_size_ne_zero

@[deprecated set!_is_setIfInBounds (since := "2024-11-24")] abbrev set_is_setIfInBounds := @set!_is_setIfInBounds
@[deprecated size_setIfInBounds (since := "2024-11-24")] abbrev size_setD := @size_setIfInBounds
@[deprecated getElem_setIfInBounds_eq (since := "2024-11-24")] abbrev getElem_setD_eq := @getElem_setIfInBounds_eq
@[deprecated getElem?_setIfInBounds_eq (since := "2024-11-24")] abbrev get?_setD_eq := @getElem?_setIfInBounds_eq
@[deprecated getD_get?_setIfInBounds (since := "2024-11-24")] abbrev getD_setD := @getD_get?_setIfInBounds
@[deprecated getElem_setIfInBounds (since := "2024-11-24")] abbrev getElem_setD := @getElem_setIfInBounds

@[deprecated List.getElem_toArray (since := "2024-11-29")]
theorem getElem_mk {xs : List α} {i : Nat} (h : i < xs.length) : (Array.mk xs)[i] = xs[i] := rfl

@[deprecated Array.getElem_toList (since := "2024-12-08")]
theorem getElem_eq_getElem_toList {a : Array α} (h : i < a.size) : a[i] = a.toList[i] := rfl

@[deprecated Array.getElem?_toList (since := "2024-12-08")]
theorem getElem?_eq_getElem?_toList (a : Array α) (i : Nat) : a[i]? = a.toList[i]? := by
  rw [getElem?_def]
  split <;> simp_all

@[deprecated LawfulGetElem.getElem?_def (since := "2024-12-08")]
theorem getElem?_eq {a : Array α} {i : Nat} :
    a[i]? = if h : i < a.size then some a[i] else none := by
  rw [getElem?_def]

end Array
