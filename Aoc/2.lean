import Aoc.Util

partial def main : IO Unit := do
  let lines ← IO.FS.lines "Aoc/2.input"
  let mut x := 0
  let mut d := 0
  let mut a := 0
  for l in lines do
    match l.splitOn with
    | ["forward", n] => x := x + n.toNat!; d := d + a * n.toNat!
    | ["down", n] => a := a + n.toNat!
    | ["up", n] => a := a - n.toNat!
    | _ => unreachable!
  dbg_trace x * d

#eval main
