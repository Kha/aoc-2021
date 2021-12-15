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
  let lines ← IO.FS.lines "13.input"
  let empty := lines.findIdx? (· = "") |>.get!
  let mut dots := lines[0:empty] |>.toArray.map (·.splitOn ",") |>.map (fun | ([x, y]) => (x.toNat!, y.toNat!) | _ => unreachable!)
  let instrs := lines[empty + 1:]
  for instr in instrs do
    let val := instr.splitOn "=" |>.get! 1 |>.toNat!
    dots := if instr.contains 'x' then
      dots.map (fun (x, y) => if x > val then (val - (x - val), y) else (x, y))
    else
      dots.map (fun (x, y) => if y > val then (x, val - (y - val)) else (x, y))
    dots := dots.foldl (·.insert) Std.HashSet.empty |>.toArray
    dbg_trace dots.size
  --dbg_trace dots
  let xmax := dots.map (·.1) |>.max
  let ymax := dots.map (·.2) |>.max
  let mut a := Array.mkArray (ymax + 1) (Array.mkArray (xmax + 1) '.')
  for (x, y) in dots do
    a := a.set! y (a[y].set! x '#')
  for line in a do
    IO.println <| String.mk line.toList

#eval main
