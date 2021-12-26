import Aoc.Util

def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let mut a := lines.map (·.toList.toArray)
  let h := a.size
  let w := a[0].size
  for step in [1:1000] do
    let mut changed := false
    let mut a' := a
    for y in [0:h] do
      for x in [0:w] do
        if a[y][x] == '>' && a[y][(x + 1) % w] == '.' then
          a' := { wrap a' with val[y][x] := '.' }.val
          a' := { wrap a' with val[y][(x + 1) % w] := '>' }.val
          changed := true
    a := a'
    for y in [0:h] do
      for x in [0:w] do
        if a[y][x] == 'v' && a[(y + 1) % h][x] == '.' then
          a' := { wrap a' with val[y][x] := '.' }.val
          a' := { wrap a' with val[(y + 1) % h][x] := 'v' }.val
          changed := true
    a := a'
    --IO.println <| "\n".intercalate <| a.map (String.mk ·.toList) |>.toList
    --IO.println ""

    if !changed then
      IO.println step
      return

--#eval main ["Aoc/25.ex"]
