import Aoc.Util
open Std

partial def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let mut lines := lines.toList
  let mut scans := #[]
  while !lines.isEmpty do
    scans := lines.tail!.takeWhile (!·.isEmpty)
      |>.map (fun s => s.splitOn "," |>.toArray |>.map String.toInt!)
      |>.toArray
      |> scans.push
    lines := lines.dropWhile (!·.isEmpty) |>.tail!
  let mut aligned := Array.mkArray scans.size false
  aligned := aligned.set! 0 true
  let mut scanners := Array.mkArray scans.size #[(0 : Int), 0, 0]
  while !aligned.all id do
    for i in [0:scans.size] do
      if !aligned[i] then
        continue
      for j in [0:scans.size] do
        if i == j || aligned[j] then
          continue
        for pi in scans[i] do
          for pj in scans[j] do
            for x in cardinals do
              if aligned[j] then break
              for y in cardinals do
                let z := cross x y
                let transl := fun p =>
                  let p := (p - pj)
                  (p[0] : Int) * x + p[1] * y + p[2] * z + pi
                if sqrLen z == 1 then
                  let trans := scans[j].map transl
                  if (scans[i].filter (trans.contains ·) |>.size) >= 12 then
                    aligned := aligned.set! j true
                    scans := scans.set! j trans
                    scanners := scanners.set! j  (transl scanners[j])
                    dbg_trace "{i} <-> {j}: {scanners[j]}"
                    break
  dbg_trace scans.toList.bind (·.toList) |>.toArray |> groupBy (fun p => (p[0], p[1], p[2])) |>.size
  dbg_trace List.pairs scanners.toList scanners.toList |>.map (fun (s, s') => manhattan (s - s')) |>.toArray.max

  --dbg_trace repr scans

--#eval timeit "" <| main ["Aoc/19.ex"]
