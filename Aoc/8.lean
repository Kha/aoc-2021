import Aoc.Util

partial def main : IO Unit := do
  let lines ← IO.FS.lines "Aoc/8.input"
  let mut count := 0
  let mut sum := 0
  for line in lines do
    let ([tens, out]) ← line.splitOn " | " | pure ()
    let out := out.splitOn
    count := count + (out.filter (fun o => [2, 4, 3, 7].contains o.bsize) |>.length)
    for perm in List.range 7 |>.perms do
      let trans d := d.map (fun c => Char.ofNat (perm.get! (c.toNat - 'a'.toNat) + 'a'.toNat)) |>.data.toArray.qsort (· < ·) |>.toList |> String.mk
    --let perm := [2, 5, 6, 0, 1, 3, 4]
      let pats := #["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]
      if tens.splitOn.all fun d =>
        let d := trans d
        pats.contains d then
        sum := sum + (out.map (fun d => let d := trans d; pats.findIdx? (· = d) |>.get!) |>.foldl (· * 10 + ·) 0)
        break
  IO.println count
  IO.println sum

#eval main
