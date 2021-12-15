def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum (a : Array Nat) : Nat := a.foldl (· + ·) 0
def Array.min (a : Array Nat) : Nat := a.foldl _root_.min a[0]
def Array.max (a : Array Nat) : Nat := a.foldl _root_.max 0

partial def main : IO Unit := do
  let lines ← IO.FS.lines "10.input"
  let score (s : String) : Option Nat := do
    let mut stack := #[]
    for c in s.data do
      match c with
      | ')' => if stack.back? == some '(' then stack := stack.pop else return none
      | ']' => if stack.back? == some '[' then stack := stack.pop else return none
      | '}' => if stack.back? == some '{' then stack := stack.pop else return none
      | '>' => if stack.back? == some '<' then stack := stack.pop else return none
      | _   => stack := stack.push c
    stack.map (fun
      | '(' => 1
      | '[' => 2
      | '{' => 3
      | '<' => 4
      | _ => 0) |>.foldr (· + · * 5) 0 |> some

  let incompl := lines.filterMap score
  dbg_trace incompl
  let incompl := incompl.qsort (· < ·)
  IO.println incompl[incompl.size / 2]

#eval main
