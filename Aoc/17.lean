import Aoc.Util

partial def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let l := lines[0].splitOn ": " |>.get! 1 |>.splitOn ", "
  let x := l.get! 0 |>.splitOn "=" |>.get! 1 |>.splitOn ".."
  let xmin := x.get! 0 |>.toInt!
  let xmax := x.get! 1 |>.toInt!
  let y := l.get! 1 |>.splitOn "=" |>.get! 1 |>.splitOn ".."
  let ymin := y.get! 0 |>.toInt!
  let ymax := y.get! 1 |>.toInt!
  let yinit := max (if ymin < 0 then -ymin - 1 else 0) (if ymax > 0 then ymax + 1 else 0)
  let high := yinit * (yinit + 1) / 2
  let mut count := 0
  for dx0 in [1:xmax.toNat + 1] do
    for dy0 in [0:high.toNat + (-ymin).toNat] do
      let dy0 := Int.ofNat dy0 + ymin
      let mut dx := Int.ofNat dx0
      let mut dy := dy0
      let mut x := 0
      let mut y := 0
      while x ≤ xmax ∧ y ≥ ymin do
        if xmin ≤ x ∧ y ≤ ymax then
          count := count + 1
          dbg_trace "{dx0}, {dy0}"
          break
        x := x + dx
        y := y + dy
        dx := max 0 (dx - 1)
        dy := dy - 1


  dbg_trace count


#eval timeit "" <| main ["Aoc/17.ex"]
