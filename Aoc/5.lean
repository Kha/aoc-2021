import Aoc.Util

#eval show IO _ from do
  let lines ← IO.FS.lines "Aoc/5.input"
  let lines := lines.map (·.splitOn " -> " |>.toArray.map (·.splitOn "," |>.toArray.map String.toNat!))
  let maxX := 1 + (lines.map (fun l => max l[0][0] l[1][0]) |>.getMax? (· < ·) |>.get!)
  let maxY := 1 + (lines.map (fun l => max l[0][1] l[1][1]) |>.getMax? (· < ·) |>.get!)
  let mut a := Array.mkArray (maxX * maxY) 0
  for l in lines do
    let dx := Int.ofNat l[0][0] - l[1][0]
    let dy := Int.ofNat l[0][1] - l[1][1]
    for i in [0: max (abs dx) (abs dy) + 1] do
      let i := Int.ofNat i
      let idx := l[1][0] + i * sgn dx + (l[1][1] + i * sgn dy) * maxX
      a := a.set! idx.toNat (a[idx.toNat] + 1)
  --for y in [0:maxY] do
  --  for x in [0:maxX] do
  --    IO.print a[x + y * maxX]
  --  IO.println ""
  dbg_trace a.filter (· > 1) |>.size
