import Aoc.Util

partial def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let mut players := lines.map fun l => l.splitOn " " |>.getLast! |>.toNat!
  --let mut points := #[0, 0]
  --let mut die := 1
  --while points.all (· < 1000) do
  --  let wrap n := (n - 1) % 10 + 1
  --  for p in [0:2] do
  --    players := players.set! p (wrap (players[p] + die + (die + 1) + (die + 2)))
  --    points := points.set! p (points[p] + players[p])
  --    die := die + 3
  --    if points[p] >= 1000 then break
  --dbg_trace points
  --dbg_trace die
  --dbg_trace (if points[0] >= 1000 then points[1] else points[0]) * (die - 1)
  let mut a := Array.mkArray 10 (Array.mkArray 31 (Array.mkArray 10 (Array.mkArray 31 (Array.mkArray 2 0))))
  a := { wrap a with val[players[0] - 1][0][players[1] - 1][0][0] := 1 }.val
  for ik in [0:31] do
    for jk in [0:31] do
      for i in [0:10] do
        for j in [0:10] do
            for d1 in [1:4] do
              for d2 in [1:4] do
                for d3 in [1:4] do
                  let d := d1 + d2 + d3
                  let back n := (n + 10 - d) % 10
                  if ik >= i + 1 && ik - (i + 1) < 21 && jk < 21 then
                    a := { wrap a with val[i][ik][j][jk][1] :=
                      a[i][ik][j][jk][1] + a[back i][ik - (i + 1)][j][jk][0] }.val
                  if jk >= j + 1 && jk - (j + 1) < 21 && jk < 21 then
                    a := { wrap a with val[i][ik][j][jk][0] :=
                      a[i][ik][j][jk][0] + a[i][ik][back j][jk - (j + 1)][1] }.val

  dbg_trace a[7][8][0][1][0]
  let mut wins1 := 0
  let mut wins2 := 0
  for i in [0:10] do
    for ik in [0:31] do
      for j in [0:10] do
        for jk in [0:31] do
          if ik >= 21 then
            wins1 := wins1 + a[i][ik][j][jk][1]
          else if jk >= 21 then
            wins2 := wins2 + a[i][ik][j][jk][0]
  dbg_trace (wins1, wins2)
  --      a := { wrap a with val[i][21][j][jk] := 1 }.val



--#eval main ["Aoc/21.ex"]
