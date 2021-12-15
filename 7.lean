def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum (a : Array Nat) : Nat := a.foldl (· + ·) 0

#eval show IO _ from do
  let a ← IO.FS.readFile "7.input"
  let a := a.trim.splitOn "," |>.map String.toNat! |>.toArray
  IO.println a
  let mi := a.getMax? (· > ·) |>.get!
  let ma := a.getMax? (· < ·) |>.get!
  let mins := List.range (ma + 1 - mi) |>.map (fun x => a.map (fun src => let d := abs (Int.ofNat src - Int.ofNat x); d * (d + 1) / 2) |>.sum)
  dbg_trace mins
  IO.println <| mins.toArray.getMax? (· > ·)
  --let a := a.qsort (· < ·)
  --let med := a[a.size / 2]
  --let sum := a.foldl (fun acc x => acc + abs (Int.ofNat x - med)) 0
  --IO.println sum
