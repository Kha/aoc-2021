import Aoc.Util

def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let meass := lines.map String.toNat!
  -- part 2
  let meass := meass.windowed 3 |>.map (·.toArray.sum)
  dbg_trace meass
  let n := meass.windowed 2 |>.filter (fun w => w[0] < w[1]) |>.size
  IO.println n

#eval main ["Aoc/1.ex"]
