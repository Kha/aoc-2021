import Aoc.Util

partial def main : IO Unit := do
  let lines ← IO.FS.lines "Aoc/10.input"
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
