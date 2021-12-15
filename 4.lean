def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum (a : Array Nat) : Nat := a.foldl (· + ·) 0

#eval show IO _ from do
  let lines ← IO.FS.lines "4.input"
  let drawn := lines[0].splitOn "," |>.map String.toNat! |>.toArray
  let boards := lines[1:] |>.toArray.filter (· ≠ "") |>.map (·.trim.splitOn " " |>.filter (· ≠ "") |>.map String.toNat! |>.toArray)
  let mut max := 0
  let mut win := 0
  let draw := (drawn.indexOf? · |>.map (·.val) |>.getD drawn.size)
  for b in [0:boards.size / 5] do
    let mut min := drawn.size
    let mut minwin := 0
    for y in [0:5] do
      let idxs := boards[b * 5 + y].map draw
      let idx := idxs.getMax? (· < ·) |>.get!
      if idx < min then
        min := idx
        minwin := List.range 5 |>.toArray.map (fun y => boards[b * 5 + y].filter (draw · > idx) |>.sum) |>.sum
        minwin := minwin * drawn[idx]
    for x in [0:5] do
      let idxs := List.range 5 |>.toArray.map (draw boards[b * 5 + ·][x])
      let idx := idxs.getMax? (· < ·) |>.get!
      if idx < min then
        min := idx
        minwin := List.range 5 |>.toArray.map (fun y => boards[b * 5 + y].filter (draw · > idx) |>.sum) |>.sum
        minwin := minwin * drawn[idx]
    if min > max then
      max := min
      win := minwin
  dbg_trace win
