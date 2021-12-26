import Aoc.Util
open Std

inductive BinOp
  | add | mul | div | mod | eql
  deriving Inhabited, BEq, Repr

def BinOp.eval (i j : Int) : BinOp → Int
  | BinOp.add => i + j
  | BinOp.mul => i * j
  | BinOp.div => i / j
  | BinOp.mod => i % j
  | BinOp.eql => btn (i == j)

inductive Val
  | input (idx : Nat)
  | lit (i : Int)
  | bin (a : Val) (op : BinOp) (b : Val)
  deriving Inhabited, BEq, Repr

instance : Coe Int Val where
  coe := Val.lit

instance : Add Val where
  add a b := Val.bin a BinOp.add b

instance : Mul Val where
  mul a b := Val.bin a BinOp.mul b

instance : Div Val where
  div a b := Val.bin a BinOp.div b

instance : Mod Val where
  mod a b := Val.bin a BinOp.mod b

notation:10 a "===" b => Val.bin a BinOp.eql b
notation:10 a "!==" b => (a === b) === 0

variable (inputs : Array Int) in
def Val.eval : Val → Int
  | Val.input idx => inputs[idx]
  | Val.lit i => i
  | Val.bin a op b => op.eval a.eval b.eval

unsafe def Val.toFormat : Val → Format := (go · |>.run' (ShareCommon.mkObjectMap ()))
where go : Val → StateM ShareCommon.ObjectMap Std.Format := fun v => match v with
  | Val.input idx => f!"inputs[{idx}]"
  | Val.lit i     => f!"{i}"
  | Val.bin a op b => do
    if let some idx := (← get).find? (unsafeCast v) then
      --return f!"x{(unsafeCast idx : Nat)}"
      return f!"x{(unsafeCast idx : Nat)}"
    let idx := (← get).size
    modify fun st => st.insert (unsafeCast v) (unsafeCast idx)
    let op := match op with
      | BinOp.add => "+"
      | BinOp.mul => "*"
      | BinOp.div => "/"
      | BinOp.mod => "%"
      | BinOp.eql => "="
    return Std.Format.paren f!"{← go a}{Format.line}{op}{Format.line}{← go b}"

@[implementedBy Val.toFormat]
constant Val.format : Val → Format

instance : Std.ToFormat Val where
  format := Val.format

unsafe def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  --let lines := lines[0:90]
  let mut vars := Std.HashMap.empty
  for var in ["w", "x", "y", "z"] do
    vars := vars.insert var (Val.lit 0)
  let mut input := 0

  let rec simp v := match v with
  | Val.bin (Val.lit i) op (Val.lit j) => Val.lit (op.eval i j)
  | _ * 0 => 0
  | a * 1 => a
  | Val.lit i * b => simp <| b * i
  | (a * Val.lit i) * Val.lit j => simp <| a * Val.lit (i * j)
  | a + 0 => a
  | 0 + a => a
  | (a + Val.lit i) + Val.lit j => simp <| a + Val.lit (i + j)
  | a / 1 => a
  | (a * Val.lit m + b) / Val.lit m' => if m % m' == 0 then simp (simp (a * Val.lit (m / m')) + simp (b / m')) else v
  | (Val.lit m + b) / Val.lit m' => if m % m' == 0 then simp (simp (Val.lit (m / m')) + simp (b / m')) else v
  | Val.bin (a * Val.lit m + b) BinOp.mod (Val.lit m') => if m % m' == 0 then simp (b % Val.lit m') else v
  | Val.bin (Val.lit m + b) BinOp.mod (Val.lit m') => if m % m' == 0 then simp (b % Val.lit m') else v
  | (Val.input idx + Val.lit b) / Val.lit m => if m > 0 ∧ b > 0 ∧ 9 + b < m then 0 else v
  | Val.bin (Val.input idx + Val.lit b) BinOp.mod (Val.lit m) => if m > 0 ∧ b > 0 ∧ 9 + b < m then Val.input idx + Val.lit b else v
  | Val.lit i === Val.input _ => if i >= 1 ∧ i <= 9 then v else Val.lit 0
  | Val.input _ === Val.lit i => if i >= 1 ∧ i <= 9 then v else Val.lit 0
  | Val.input _ + Val.lit i === Val.input _ => if i >= 10 then Val.lit 0 else v
  | Val.input _ + Val.lit i === Val.lit j => if 9 + i < j || i + 1 > j then Val.lit 0 else v
  | Val.input idx + Val.lit i === Val.input jdx + Val.lit j => if idx == jdx && i == j then Val.lit 1 else v
  -- dubious
  --|  _ + Val.lit i === Val.input _ => if i >= 10 then Val.lit 0 else v
  | v => v

  -- iteratively reverse-engineered from output, construct min/max from it manually
  let rec inp
  | 3 => simp <| inp 2 + 3
  | 5 => simp <| inp 4 + Val.lit (-6)
  | 6 => simp <| inp 1 + 8
  | 10 => simp <| inp 9 + Val.lit (-5)
  | 11 => simp <| inp 8 + 1
  | 12 => simp <| inp 7 + 5
  | 13 => simp <| inp 0 + Val.lit (-4)
  | i => Val.input i

  for l in lines do
    let words := l.splitOn.toArray
    vars := vars.insert words[1] <| match words[0] with
      | "inp" =>
        inp input
      | op =>
        let lhs := vars.find! words[1]
        let op := match op with
          | "add" => BinOp.add
          | "mul" => BinOp.mul
          | "div" => BinOp.div
          | "mod" => BinOp.mod
          | "eql" => BinOp.eql
          | _     => unreachable!
        let rhs := if words[2][0].isAlpha then vars.find! words[2] else Val.lit words[2].toInt!
        simp (Val.bin lhs op rhs)
    if words[0] == "inp" then input := input + 1
  let target := vars.find! "z"
  dbg_trace format target

#eval main ["Aoc/24.input"]
