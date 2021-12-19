import Std

def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum (a : Array Nat) : Nat := a.foldl (· + ·) 0
def Array.min (a : Array Nat) : Nat := a.foldl _root_.min a[0]
def Array.max (a : Array Nat) : Nat := a.foldl _root_.max 0

partial def List.perms [DecidableEq α] : List α → List (List α)
  | [] => [[]]
  | as => as.bind (fun a => perms (as.filter (· ≠ a)) |>.bind (fun perm => [a::perm]))

partial def List.windowed : List α → List (α × α)
  | a::b::cs => (a, b) :: windowed (b::cs)
  | _ => []

instance : Hashable Char where
  hash c := hash c.val

structure Wrap (α : Type) where wrap :: val : α
export Wrap (wrap)

instance [Add α] : Add (Array α) where
  add as bs := as.zipWith bs Add.add

structure Loop where

@[inline]
partial def Loop.forIn {β : Type u} {m : Type u → Type v} [Monad m] (loop : Loop) (init : β) (f : Unit → β → m (ForInStep β)) : m β :=
  let rec @[specialize] loop (b : β) : m β := do
    match ← f () b with
      | ForInStep.done b  => pure b
      | ForInStep.yield b => loop b
  loop init

instance : ForIn m Loop Unit where
  forIn := Loop.forIn

syntax "repeat " doSeq : doElem

macro_rules
  | `(doElem| repeat $seq) => `(doElem| for _ in Loop.mk do $seq)

syntax "while " termBeforeDo " do " doSeq : doElem

macro_rules
  | `(doElem| while $cond do $seq) =>
    `(doElem| repeat if $cond then $seq else break)
