open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

module M = Belt.Map.Int
module S = Belt.Set.Int
type disk = M.t<int>
type free = S.t

// todo: put in StdLib
// ref: https://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-IntMap-Lazy.html#v:union
// O(n+m). The (left-biased) union of two maps. It prefers the first map when duplicate keys are encountered
let union: (M.t<'a>, M.t<'a>) => M.t<'a> = (a, b) => {
  M.merge(a, b, (k, v1, v2) => {
    switch (v1, v2) {
    | (Some(v1), Some(_)) => Some(v1) // left-biased
    | (Some(v1), None) => Some(v1)
    | (None, Some(v2)) => Some(v2)
    | (None, None) => None
    }
  })
}

let expandMapItem: ((bool, int, int, disk, free), int) => (bool, int, int, disk, free) = (
  acc,
  size,
) => {
  let (isFile, pos, fileID, disk, free) = acc

  isFile
    ? {
        let fileExtent = Array.zip(
          Array.fromInitializer(~length=size, i => pos + i),
          Array.make(~length=size, fileID),
        )
        let file = M.fromArray(fileExtent)
        let disk' = union(disk, file)
        (false, pos + size, fileID + 1, disk', free)
      }
    : {
        let gap = Array.fromInitializer(~length=size, i => pos + i)->S.fromArray
        let free' = S.union(free, gap)
        (true, pos + size, fileID, disk, free')
      }
}

let expand: array<int> => (disk, free) = diskMap => {
  let (_, _, _, disk, free) = diskMap->Array.reduce((true, 0, 0, M.empty, S.empty), expandMapItem)
  (disk, free)
}

let showDisk: disk => string = disk => {
  let pMax = M.maxKey(disk)->Option.getUnsafe
  let str = ref("")
  for i in 0 to pMax {
    let char = switch M.get(disk, i) {
    | Some(v) => v->Int.toString
    | None => "."
    }
    str.contents = str.contents ++ char
  }
  str.contents
}

let showFree: free => string = free => {
  let pMax = S.maximum(free)->Option.getUnsafe
  let str = ref("")
  for i in 0 to pMax {
    let char = S.has(free, i) ? "+" : "."
    str.contents = str.contents ++ char
  }
  str.contents
}

let showDiskFree: (disk, free) => string = (disk, free) => {
  showDisk(disk) ++ "\n" ++ showFree(free)
}

//let expand: array<int> => (disk, free) = (disk, free) => {
//  let diskMap = (disk, free)
//}

let parse = data =>
  data
  ->String.trim
  ->String.split("")
  ->Array.map(s => s->Int.fromString->Option.getUnsafe)

let solvePart1 = data => {
  let diskMap = data->parse
  let (disk, free) = diskMap->expand
  showDiskFree(disk, free)->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
