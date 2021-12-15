def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum (a : Array Nat) : Nat := a.foldl (· + ·) 0
def Array.min (a : Array Nat) : Nat := a.foldl _root_.min a[0]
def Array.max (a : Array Nat) : Nat := a.foldl _root_.max 0

partial def main : IO Unit := do
  let lines ← IO.FS.lines "2.input"
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
