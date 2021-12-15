def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum (a : Array Nat) : Nat := a.foldl (· + ·) 0
def Array.min (a : Array Nat) : Nat := a.foldl _root_.min a[0]
def Array.max (a : Array Nat) : Nat := a.foldl _root_.max 0

partial def List.perms [DecidableEq α] : List α → List (List α)
  | [] => [[]]
  | as => as.bind (fun a => perms (as.filter (· ≠ a)) |>.bind (fun perm => [a::perm]))

partial def main : IO Unit := do
  let lines ← IO.FS.lines "9.input"
  let a := lines.map fun line => #[10] ++ line.data.toArray.map (fun c => c.toNat - '0'.toNat) ++ #[10]
  let a := #[Array.mkArray a[0].size 10] ++ a ++ #[Array.mkArray a[0].size 10]
  let mut sum := 0
  let mut basins := #[]
  for y in [0:a.size] do
    for x in [0:a[y].size] do
  --let x := 1 let y := 0
      let git (dy dx : Int) :=
        let y' := Int.ofNat y + dy + 1
        let x' := Int.ofNat x + dx + 1
        a[y'.toNat][x'.toNat]
      if a[y + 1][x + 1] < #[git (-1) 0, git 0 1, git 1 0, git 0 (-1)].min then
        sum := sum + a[y + 1][x + 1] + 1
        let rec fill y x dest : StateM _ _ := do
          let a ← get
          let git (dy dx : Int) :=
              let y' := Int.ofNat y + dy + 1
              let x' := Int.ofNat x + dx + 1
              a[y'.toNat][x'.toNat]
          let dest' := a[y + 1][x + 1]
          if dest' < 9 ∧ a[y + 1][x + 1] ≤ #[git (-1) 0, git 0 1, git 1 0, git 0 (-1)].min ∧ #[git (-1) 0, git 0 1, git 1 0, git 0 (-1)].min > dest then
            set <| a.set! (y + 1) (a[y + 1].set! (x + 1) 10)
            return 1 + #[← fill (y-1) x dest', ← fill y (x+1) dest', ← fill (y+1) x dest', ← fill y (x-1) dest'].sum
          else
            pure 0
        let (basin, a) := fill y x 0 a
        --dbg_trace (String.intercalate "\n" <| a.map toString |>.toList)
        --dbg_trace "\n"
        basins := basins.push basin

  dbg_trace a
  dbg_trace sum
  basins := basins.qsort (· < ·)
  dbg_trace basins.toList.drop (basins.size - 3) |>.foldl (· * ·) 1

#eval main
