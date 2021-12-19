import Aoc.Util

partial def main : IO Unit := do
  let lines ← IO.FS.lines "Aoc/13.input"
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
