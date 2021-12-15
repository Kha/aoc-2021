import Std

def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum (a : Array Nat) : Nat := a.foldl (· + ·) 0
def Array.min (a : Array Nat) : Nat := a.foldl _root_.min a[0]
def Array.max (a : Array Nat) : Nat := a.foldl _root_.max 0

partial def List.perms [DecidableEq α] : List α → List (List α)
  | [] => [[]]
  | as => as.bind (fun a => perms (as.filter (· ≠ a)) |>.bind (fun perm => [a::perm]))

partial def List.windowed : List α → List (α × α)
  | a::b::cs => (a, b) :: windowed (b::cs)
  | _ => []

instance : Hashable Char where
  hash c := hash c.val

structure Wrap (α : Type) where wrap :: val : α
export Wrap (wrap)

instance [Add α] : Add (Array α) where
  add as bs := as.zipWith bs Add.add

partial def main : IO Unit := do
  let lines ← IO.FS.lines "14.ex"
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
