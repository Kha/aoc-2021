import Std

def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum (a : Array Nat) : Nat := a.foldl (· + ·) 0
def Array.min (a : Array Nat) : Nat := a.foldl _root_.min a[0]
def Array.max (a : Array Nat) : Nat := a.foldl _root_.max 0

partial def List.perms [DecidableEq α] : List α → List (List α)
  | [] => [[]]
  | as => as.bind (fun a => perms (as.filter (· ≠ a)) |>.bind (fun perm => [a::perm]))

partial def main : IO Unit := do
  let lines ← IO.FS.lines "12.input"
  let mut g := Std.HashMap.empty
  for line in lines do
    let [start, stop] ← line.splitOn "-" | dbg_trace "parse fail"
    g := g.insert start (stop :: g.findD start [])
    g := g.insert stop (start :: g.findD stop [])
  let rec go (path : List String) (twice : Bool) : Nat :=
    if path.head! == "end" then 1 else 0 + (
    g.find! path.head! |>.filter (fun n => n[0].isUpper ∨ !path.contains n ∨ !twice ∧ !["start", "end"].contains n ∧ (path.filter (· == n)).length == 1)
      |>.map (fun n => go (n::path) (twice ∨ !n[0].isUpper ∧ path.contains n))
      |>.toArray.sum)
  dbg_trace go ["start"] false

#eval main
