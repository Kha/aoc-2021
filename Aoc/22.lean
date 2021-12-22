import Aoc.Util
open Std

partial def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let rules := lines.map fun l =>
    let ws := l.splitOn.toArray
    (ws[0] == "on", ws[1].splitOn "," |>.toArray |>.map fun part =>
      let ints := part.splitOn "=" |>.get! 1 |>.splitOn ".." |>.toArray |>.map String.toInt!
      #[ints[0], ints[1] + 1])
  let coords := List.range 3 |>.map (fun axis => rules.foldMap (·.2[axis]) |>.qsort (· < ·) |>.toList.eraseDups.toArray) |>.toArray
  let lookups := coords.map fun l => l.mapIdx ((·, ·)) |>.foldl (fun m (i, a) => m.insert a i.val) Std.HashMap.empty
  let mut a := Array.mkArray coords[0].size (Array.mkArray coords[1].size (Array.mkArray coords[2].size false))
  for (b, r) in rules do
    --if r[0][0] < -50 || r[0][0] > 50 then
    --  continue
    for x in [lookups[0].find! r[0][0]: lookups[0].find! r[0][1]] do
      for y in [lookups[1].find! r[1][0]: lookups[1].find! r[1][1]] do
        for z in [lookups[2].find! r[2][0]: lookups[2].find! r[2][1]] do
          a := { wrap a with val[x][y][z] := b }.val
  dbg_trace a.mapIdx (fun x a => a.mapIdx (fun y a => a.mapIdx (fun z b =>
    if b then (#[x.val, y.val, z.val]).mapIdx (fun axis v => coords[axis.val][v + 1] - coords[axis.val][v]) |>.prod
    else 0) |>.sum) |>.sum) |>.sum

--#eval main ["Aoc/22.input"]
