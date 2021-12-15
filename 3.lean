def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum (a : Array Nat) : Nat := a.foldl (· + ·) 0

partial def main : IO Unit := do
  let lines ← IO.FS.lines "3.input"
  let common := fun (col : Array Nat) => if (col.filter (· == 0) |>.size) > col.size / 2 then 0 else 1
  let lines := lines.map (fun line => line.data.toArray.map (·.toNat - '0'.toNat))
  dbg_trace lines

  let getCol lines := fun i => lines.map (·[i])
  --let cols := List.range lines[0].size |>.map (getCol lines)
  --let gamma := cols.map (common)
  --let gamma := gamma.foldl (· * 2 + ·) 0
  --let delta := cols.map (1 - common ·)
  --let delta := delta.foldl (· * 2 + ·) 0
  let rec down rows i f :=
    if rows.size == 1 then
      rows[0]
    else
      let b := f (common (getCol rows i))
      down (rows.filter (fun row => row[i] == b)) (i + 1) f
  let ox := down lines 0 id
  let ox := ox.foldl (· * 2 + ·) 0
  let co := down lines 0 (1 - ·)
  let co := co.foldl (· * 2 + ·) 0
  dbg_trace co * ox

#eval main
