import Aoc.Util

inductive Num
  | atom (n : Nat)
  | pair (n m : Num)
  deriving Inhabited, Repr

partial def parse : StateM String.Iterator Num := do
  if !(← get).hasNext then
    return unreachable!
  match (← get).curr with
  | '[' =>
    modify (·.next)
    let n ← parse
    modify (·.next)
    let m ← parse
    modify (·.next)
    return Num.pair n m
  | c =>
    modify (·.next)
    return Num.atom (c.toNat - '0'.toNat)

partial def explode? (n : Num) : Option Num :=
  go n 0 |>.map (·.1)
where
  go n d := match n with
    | Num.atom _ => none
    | Num.pair n m =>
      if d ≥ 4 then
        match n, m with
        | Num.atom n, Num.atom m => some (Num.atom 0, n, m)
        | _, _ => none
      else
        if let some (n, l, r) := go n (d + 1) then
          some (Num.pair n (addL m r), l, 0)
        else if let some (m, l, r) := go m (d + 1) then
          some (Num.pair (addR n l) m, 0, r)
        else none
  addL
  | n, 0 => n
  | Num.atom n, m => Num.atom (n + m)
  | Num.pair n m, r => Num.pair (addL n r) m
  addR
  | n, 0 => n
  | Num.atom n, m => Num.atom (n + m)
  | Num.pair n m, r => Num.pair n (addR m r)

partial def split? : Num → Option Num
  | Num.atom n => if n ≥ 10 then Num.pair (Num.atom (n / 2)) (Num.atom (n - n / 2)) else none
  | Num.pair n m =>
    if let some n := split? n then
      Num.pair n m
    else if let some m := split? m then
      Num.pair n m
    else
      none

partial def reduce (n : Num) : Num :=
  if let some n' := explode? n then
    reduce n'
  else if let some n' := split? n then
    reduce n'
  else n

def add (n m : Num) : Num :=
  reduce (Num.pair n m)

def popPop : Num → Nat
  | Num.atom n => n
  | Num.pair n m => 3 * popPop n + 2 * popPop m

partial def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let mut nums := lines.map (parse ·.mkIterator |>.1)
  let res := nums[1:].foldl add nums[0]
  let mut res2 := 0
  for i in [:nums.size] do
    for j in [:nums.size] do
      if i ≠ j then
        let pop := popPop (add nums[i] nums[j])
        if pop > res2 then
          res2 := pop
  dbg_trace repr res
  dbg_trace popPop res
  dbg_trace res2

#eval timeit "" <| main ["Aoc/18.input"]
