import Aoc.Util

partial def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let a := lines.map (fun l => l.data.map (fun c => c.toNat - '0'.toNat) |>.toArray)
  let a := List.range (a.size * 5) |>.map (fun y => List.range (a.size * 5) |>.map (fun x => (a[y % a.size][x % a[0].size] + y / a.size + x / a[0].size - 1) % 9 + 1) |>.toArray) |>.toArray
  let mut q := Std.RBTree.fromList [(0, 0, 0)] (fun x y => compareOfLessAndEq x y)
  let mut mins := a.map (·.map (fun _ => 10000000))
  let mut vis := a.map (·.map (fun _ => false))
  mins := mins.set! 0 (mins[0].set! 0 0)
  while !q.isEmpty do
    let (w, x, y) := q.min.get!
    q := q.erase (w, x, y)
    if !vis[y][x] then
      vis := vis.set! y (vis[y].set! x true)
      let push x y q mins := if w + a[y][x] < mins[y][x] then (q.insert (w + a[y][x], x, y), mins.set! y (mins[y].set! x (w + a[y][x]))) else (q, mins)
      if x > 0 then
        (q, mins) := push (x - 1) y q mins
      if x < a[0].size - 1 then
        (q, mins) := push (x + 1) y q mins
      if y > 0 then
        (q, mins) := push x (y - 1) q mins
      if y < a.size - 1 then
        (q, mins) := push x (y + 1) q mins

  dbg_trace mins[a.size - 1][a[0].size - 1]

#eval timeit "" <| main ["Aoc/15.input"]
