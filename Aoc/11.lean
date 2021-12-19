import Aoc.Util

partial def main : IO Unit := do
  let lines ← IO.FS.lines "Aoc/11.input"
  let mut a := lines.map fun line => #[100] ++ line.data.toArray.map (fun c => c.toNat - '0'.toNat) ++ #[100]
  a := #[Array.mkArray a[0].size 100] ++ a ++ #[Array.mkArray a[0].size 100]
  let mut sum := 0
  for step in [0:1000] do
    for y in [1:a.size-1] do
      for x in [1:a[y].size-1] do
        a := a.set! y (a[y].set! x (a[y][x] + 1))
    for y in [1:a.size-1] do
      for x in [1:a[y].size-1] do
        let rec fill y x : StateM _ _ := do
          let a ← get
          if a[y][x] == 100 then return 0
          if a[y][x] == 200 then return 0
          let a := a.set! y (a[y].set! x (a[y][x] + 1))
          --if step == 1 then
          --  dbg_trace "{x - 1}, {y - 1}: {a[y][x]}"
          if a[y][x] > 9 then
            let a := a.set! y (a[y].set! x 200)
            set a
            let git (dy dx : Int) :=
                let y' := Int.ofNat y + dy
                let x' := Int.ofNat x + dx
                fill y'.toNat x'.toNat
            let sum' ← (List.range 3 |>.bind (fun y => List.range 3 |>.map (fun x => if x == 1 ∧ y == 1 then pure 0 else git (Int.ofNat y - 1) (Int.ofNat x - 1)))).mapM id
            return sum'.toArray.sum + 1
          else
            set a
            pure 0
        if a[y][x] > 9 ∧ a[y][x] < 100 then
          a := a.set! y (a[y].set! x (a[y][x] - 1))
          let (sum', a') := fill y x a
          a := a'
          sum := sum + sum'
    let mut all := true
    for y in [1:a.size-1] do
      for x in [1:a[y].size-1] do
        if a[y][x] == 200 then
          a := a.set! y (a[y].set! x 0)
        else
          all := false
    if all then dbg_trace step

#eval main
