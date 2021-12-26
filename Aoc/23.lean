import Aoc.Util

set_option trace.compiler.ir.result true
set_option trace.Elab.definition.body true
partial def main (args : List String) : IO Unit := do
  let lines ← IO.FS.lines ⟨args.head!⟩
  let lines := lines[:3] ++ #["  #D#C#B#A#  ", "  #D#B#A#C#  "] ++ lines[3:]
  let hallways := 7
  let rooms := 4
  let depth := lines.size - 3
  let init := String.mk (List.replicate hallways '.') ++ String.mk (List.range rooms |>.bind fun i =>
    List.range depth |>.map fun d => lines[2 + d][3 + 2 * i])
  let mut q := Std.RBTree.fromList [(0, init)] (fun x y => compareOfLessAndEq x y)
  let mut mins := Std.HashMap.empty
  let mut vis := Std.HashSet.empty
  let mut prevs := Std.HashMap.empty
  mins := mins.insert init 0
  let pp (s : String) := Id.run do
    let mut l := lines
    l := l.set! 1 (l[1].set 1 s[0])
    for i in [0:hallways - 2] do
      l := l.set! 1 (l[1].set (2 + 2 * i) s[i + 1])
    l := l.set! 1 (l[1].set (2 + 2 * (hallways - 3) + 1) s[hallways - 1])
    for j in [0:rooms] do
      for d in [0:depth] do
        l := l.set! (2 + d) (l[2 + d].set (3 + 2 * j) s[hallways + depth * j + d])
    "\n".intercalate l.toList

  while !q.isEmpty do
    let (w, s) := q.min.get!
    q := q.erase (w, s)
    if !vis.contains s then
      vis := vis.insert s
      if w > 0 && s == String.mk (init.toList.toArray.qsort (· < ·) |>.toList) then
        IO.println "SOL"
        let mut s' := s
        while s' != init do
          IO.println s!"{mins.find! s'}:\n{pp s'}"
          s' := prevs.find! s'
        return
      --IO.println s!"{w}: {s}"
      let room j k := hallways + depth * j + k
      let first j := room j (List.range depth |>.find? (s[room j ·] != '.') |>.getD (depth - 1))
      let firstfree j := if s[room j (depth - 1)] == '.' then room j (depth - 1) else first j - 1
      let swap i j := s.set i s[j] |>.set j s[i]
      let cost
        | 'A' => 1
        | 'B' => 10
        | 'C' => 100
        | 'D' => 1000
        | _   => unreachable!
      for i in [0:hallways] do
        for j in [0:rooms] do
          if (s[i] == '.' ∧ s[first j] != '.') ∨ (s[i] != '.' && s[room j 0] == '.' && s[i].toNat - 'A'.toNat == j && [0:depth].all (fun d => s[room j d] == s[i] || s[room j d] == '.')) then
            let hallstart := if i <= j + 1 then i else j + 2
            let hallstop := if i >= j + 2 then i else j + 1
            let free := [hallstart + (if i == hallstart then 1 else 0):hallstop + (if i == hallstop then 0 else 1)].all (s[·] == '.')
            if free then
              let t := if s[i] == '.' then s[first j] else s[i]
              let k := if s[i] == '.' then first j else firstfree j
              let w' := w + cost t * (2 * (hallstop - hallstart) - (if hallstart == 0 then 1 else 0) -
                (if hallstop == hallways - 1 then 1 else 0) + 2 + (k - room j 0))
              let s' := swap i k
              --if s == ".....BBAADC.C.D" && i == 4 && j == 1 then dbg_trace "{w'}: {s'}"
              if w' < mins.findD s' 1000000 then
                q := q.insert (w', s')
                mins := mins.insert s' w'
                prevs := prevs.insert s' s

--#eval timeit "" <| main ["Aoc/23.ex"]
