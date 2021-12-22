import Aoc.Util

def toBit c := if c == '#' then 1 else 0
partial def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let rule := lines[0]
  let mut a := lines[2:] |>.toArray |>.map (·.data.toArray)
  for step in [0:50] do
    a := a.map fun line => #['.'] ++ line ++ #['.']
    a := #[Array.mkArray a[0].size '.'] ++ a ++ #[Array.mkArray a[0].size '.']
    a := a.map fun line => #['.'] ++ line ++ #['.']
    a := #[Array.mkArray a[0].size '.'] ++ a ++ #[Array.mkArray a[0].size '.']
    --dbg_trace ("\n".intercalate <| a.toList.map (String.mk ·.toList))
    let mut a' := a
    for x in [1:a[0].size - 1] do
      for y in [1:a.size - 1] do
        let mut idx := 0
        let minv b c := if rule[0] == '#' && b then if c == '#' then '.' else '#' else c
        for dy in [0:3] do
          for dx in [0:3] do
            idx := idx * 2 + (toBit <| minv (step % 2 == 1) a[y + dy - 1][x + dx - 1])
        a' := a'.set! y (a'[y].set! x <| minv (step % 2 == 0) rule[idx])
    a := a'
    --dbg_trace ("\n".intercalate <| a.toList.map (String.mk ·.toList))
  dbg_trace a.foldMap (fun line => line.map toBit) |>.sum

--#eval main ["Aoc/20.ex"]
