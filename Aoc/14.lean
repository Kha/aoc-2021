import Aoc.Util

partial def main : IO Unit := do
  let lines ← IO.FS.lines "Aoc/14.ex"
  let l := lines[0].data
  let mut reassoc := l.foldl (fun m c => if m.contains c then m else m.insert c m.size) Std.HashMap.empty
  if !reassoc.contains 'H' then
    reassoc := reassoc.insert 'H' reassoc.size
  let l := l.map reassoc.find!
  let m := reassoc.size
  let rules := lines[2:].toArray.map (fun l => l.splitOn " -> " |>.toArray) |>.foldl (fun m r => m.insert (reassoc.find! r[0][0], reassoc.find! r[0][1]) (reassoc.find! r[1][0])) Std.HashMap.empty
  let s := 41
  let mut dp := Array.mkArray s (Array.mkArray m (Array.mkArray m (Array.mkArray m 0)))
  for x in [0:m] do
    for y in [0:m] do
      dp := { wrap dp with val[0][x][y][x] := 1 }.val
      --dp := { wrap dp with val[0][x][y][y] := dp[0][x][y][y] + 1 }.val
  for s in [1:s] do
    for x in [0:m] do
      for y in [0:m] do
        let mid := rules.find! (x, y)
        dp := { wrap dp with val[s][x][y] := dp[s - 1][x][mid] + dp[s - 1][mid][y] }.val
        --for z in [0:m] do
        --  dp := { wrap dp with val[s][x][y][z] := dp[s][x][mid][z] + dp[s][mid][y][z] }.val

  dbg_trace reassoc.toArray
  let counts := l.windowed.map (fun (a, b) => dp[40][a][b])
  let counts := counts.tail!.foldl (· + ·) counts.head!
  let counts := counts.set! l.getLast! (counts[l.getLast!] + 1)
  dbg_trace counts
  dbg_trace counts.max - counts.min

  --for _ in [0:10] do
  --  l := l.head! :: (l.windowed.map (fun (a, b) => (rules.find! (String.mk [a, b])).data ++ [b]) |>.bind id)
  --dbg_trace l.length
  --let counts := l.foldl (fun m c => m.insert c.val ((m.find? c.val |>.getD 0) + 1)) Std.HashMap.empty |>.toArray
  --let counts := counts.map (·.2)

#eval main
