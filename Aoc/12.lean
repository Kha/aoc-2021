import Aoc.Util

partial def main : IO Unit := do
  let lines ← IO.FS.lines "Aoc/12.input"
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
