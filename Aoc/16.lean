import Aoc.Util
open Std

def toBits (s : String) := s.toList.toArray.foldMap fun hex =>
  let n := if '0' <= hex && hex <= '9' then hex.toNat - '0'.toNat else 10 + hex.toNat - 'A'.toNat
  #[n / 8, n / 4 % 2, n / 2 % 2, n % 2]

def toNat (bits : Subarray Nat) := bits.foldl (· * 2 + ·) 0

partial def packet : StateM (Array Nat) Nat := do
  let take (n : Nat) := do
    modifyGet (fun bits => (bits[:n], bits[n:]))
  let ver ← take 3
  let type ← take 3
  match toNat type with
  | 4 =>
    let mut n := #[]
    while true do
      let bits ← take 5
      n := n ++ bits[1:5].toArray
      if bits[0] != 1 then
        break
    --toNat ver
    toNat n.toSubarray
  | op =>
    let op := match op with
      | 0 => (· + ·)
      | 1 => (· * ·)
      | 2 => min
      | 3 => max
      | 5 => fun x y => btn (x > y)
      | 6 => fun x y => btn (x < y)
      | 7 => fun x y => btn (x = y)
      | _ => unreachable!

    match (← take 1)[0] with
    | 0 =>
      let l := toNat (← take 15)
      let stop := (← get).size - l
      let mut ps := #[]
      while (← get).size > stop do
        let p ← packet
        ps := ps.push p
      return ps[1:].foldl op ps[0]
    | _ =>
      let m := toNat (← take 11)
      let ps ← List.range m |>.toArray.mapM fun _ => packet
      return ps[1:].foldl op ps[0]

#eval packet (toBits "C200B40A82")
#eval packet (toBits "04005AC33890")
#eval packet (toBits "EE00D40C823060")
#eval packet (toBits "8A004A801A8002F478")
#eval packet (toBits "620080001611562C8802118E34")

partial def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  IO.println <| packet (toBits lines[0])

#eval main ["Aoc/16.input"]
