open Stdlib
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
  M.merge(a, b, (_k, v1, v2) => {
    switch (v1, v2) {
    | (Some(v1), Some(_)) => Some(v1) // left-biased
    | (Some(v1), None) => Some(v1)
    | (None, Some(v2)) => Some(v2)
    | (None, None) => None
    }
  })
}

// todo: put in StdLib
let mapDeleteFindMax: M.t<'a> => ((int, 'a), M.t<'a>) = m => {
  let (k, v) = M.maximum(m)->Option.getUnsafe
  ((k, v), m->M.remove(k))
}

// todo: put in StdLib
let setDeleteFindMin: S.t => (int, S.t) = s => {
  let k = S.minimum(s)->Option.getUnsafe
  (k, s->S.remove(k))
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

let isPackedBlock: (disk, free) => bool = (disk, free) => {
  let dMax = M.maxKey(disk)->Option.getUnsafe
  let fMin = S.minimum(free)->Option.getUnsafe
  dMax < fMin
}

let packBlocksStep: (disk, free) => (disk, free) = (disk, free) => {
  isPackedBlock(disk, free)
    ? (disk, free)
    : {
        // continue here
        let ((from, fID), disk') = mapDeleteFindMax(disk)
        let (to, free') = setDeleteFindMin(free)

        (disk'->M.set(to, fID), free'->S.add(from))
      }
}

let packBlocks: (disk, free) => (disk, free) = (disk, free) => {
  let rec iterate = (disk, free) => {
    isPackedBlock(disk, free)
      ? (disk, free)
      : {
          let (disk', free') = packBlocksStep(disk, free)
          iterate(disk', free')
        }
  }
  iterate(disk, free)
}

let parse = data =>
  data
  ->String.trim
  ->String.split("")
  ->Array.map(s => s->Int.fromString->Option.getUnsafe)

let solvePart1 = data => {
  let diskMap = data->parse
  let (disk, free) = diskMap->expand
  let (disk', _free') = packBlocks(disk, free)
  //  showDiskFree(disk', free')->log

  // todo: the sum is too big for int, use sumBigIntArray
  //  disk'->M.toArray->log
  disk'
  ->M.toArray
  ->Array.map(((k, v)) => k->BigInt.fromInt * v->BigInt.fromInt)
  ->Array.reduce(_, 0n, BigInt.add)
}

let solvePart2 = data => {
  data->ignore
  2
}
