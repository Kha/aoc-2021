def m := 256

def f := Id.run do
  let mut a := Array.mkArray (m + 7) 1
  for n in [7:m + 7] do
    a := a.set! n <| a[n - 7] + (if n ≥ 9 then a[n - 9] else 1)
  a

#eval show IO _ from do
  let a ← IO.FS.readFile "input"
  let a := a.trim.splitOn "," |>.map String.toNat!
  IO.println a
  let fishies := a.map (fun i => f[m + (6 - i)])
  let sum := fishies.foldl (· + ·) 0
  IO.println sum
