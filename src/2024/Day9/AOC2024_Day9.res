open Stdlib
let log = Console.log
let log2 = Console.log2

module M = Relude.Map.WithOrd(Relude.Int.Ord)
module S = Relude.Set.WithOrd(Relude.Int.Ord)
type disk = M.t<int>
type free = S.t

type region = Free(int) | Used(int, int)
type rDisk = array<region>

// todo: put in StdLib
// ref: https://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-IntMap-Lazy.html#v:union
// O(n+m). The (left-biased) union of two maps. It prefers the first map when duplicate keys are encountered
let union: (M.t<'a>, M.t<'a>) => M.t<'a> = (a, b) => {
  M.merge((_k, v1, v2) => {
    switch (v1, v2) {
    | (Some(v1), Some(_)) => Some(v1) // left-biased
    | (Some(v1), None) => Some(v1)
    | (None, Some(v2)) => Some(v2)
    | (None, None) => None
    }
  }, a, b)
}

// todo: put in StdLib
let mapDeleteFindMax: M.t<'a> => ((int, 'a), M.t<'a>) = m => {
  let (k, v) = M.max(m)->Option.getUnsafe
  ((k, v), m->M.remove(k, _))
}

// todo: put in StdLib
let setDeleteFindMin: S.t => (int, S.t) = s => {
  let k = S.minimum(s)->Option.getUnsafe
  (k, s->S.remove(k, _))
}

let fileID: region => int = r => {
  switch r {
  | Used(_, fID) => fID
  | _ => -1
  }
}

let freeSize: region => int = r => {
  switch r {
  | Free(size) => size
  | _ => 0
  }
}

let expandRegion: ((bool, int, int, rDisk), int) => (bool, int, int, rDisk) = (acc, size) => {
  let (_isFile, _pos, _fID, _disk) = acc
  switch (acc, size) {
  | ((true, pos, fID, disk), size) => (false, pos + size, fID + 1, [Used(size, fID), ...disk])
  | ((false, pos, fID, disk), 0) => (true, pos, fID, disk)
  | ((false, pos, fID, disk), size) => (true, pos + size, fID, [Free(size), ...disk])
  }
}

let expandRDisk: array<int> => rDisk = diskMap => {
  let diskMap = Array.toReversed(diskMap)
  let (_isFile, _pos, _fID, disk) = diskMap->Array.reduce((true, 0, 0, []), expandRegion)
  disk
}

let toBlock: ((int, disk, free), region) => (int, disk, free) = (acc, r) => {
  let (_pos, _disk, _free) = acc
  switch (acc, r) {
  | ((pos, disk, free), Free(size)) => {
      let gap = List.fromInitializer(~length=size, i => pos + i)
      let free' = S.union(free, gap->List.toArray->S.fromArray)
      (pos + size, disk, free')
    }
  | ((pos, disk, free), Used(size, fileID)) => {
      let fileExtent = Array.zip(
        Array.fromInitializer(~length=size, i => pos + i),
        Array.make(~length=size, fileID),
      )

      let file = M.fromArray(fileExtent)
      let disk' = union(disk, file)
      (pos + size, disk', free)
    }
  }
}

let toBlocks: rDisk => (disk, free) = rdisk => {
  let (_, disk, free) = rdisk->Array.reduce((0, M.make(), S.empty), toBlock)
  (disk, free)
}

let findFree: (int, rDisk) => Option.t<(rDisk, region, rDisk)> = (size, disk) => {
  let (prefix, suffix) = disk->Array.break(r => freeSize(r) >= size)

  switch (size, disk) {
  | (0, _suffix) => None
  | _ => (prefix, Array.headUnsafe(suffix), Array.tail(suffix))->Some
  }
}

exception Impossible(string)
let rec tidyRegion: (rDisk, region) => rDisk = (rdisk, r) => {
  switch (r, rdisk) {
  | (Free(0), rdisk) => rdisk
  | (Free(size) as region, rdisk) => {
      let head = rdisk[0]

      switch head {
      | Some(Free(size1)) => {
          let rdisk' = Array.tail(rdisk)

          tidyRegion(rdisk', Free(size + size1))
        }
      | Some(Used(_, _))
      | None =>
        [region, ...rdisk]
      }
    }
  | (region, rdisk) => [region, ...rdisk]
  }
}

let tidy: rDisk => rDisk = disk => Array.reduceRight(disk, [], tidyRegion)

let packFile: (int, rDisk) => rDisk = (fid, disk) => {
  let (prefixMid, suffix0) = disk->Array.span(r => fileID(r) != fid)
  let Used(fSize, _) = suffix0[0]->Option.getUnsafe
  let suffix = Array.tail(suffix0)

  let gap = findFree(fSize, prefixMid)

  switch gap {
  | None => disk
  | Some(prefix, Free(gapSize), mid) =>
    [...prefix, Used(fSize, fid), Free(gapSize - fSize), ...mid, Free(fSize), ...suffix]
  }
}

let rec packBelow: (int, rDisk) => rDisk = (fid, disk) => {
  switch (fid, disk) {
  | (_, []) => []
  | (0, disk) => disk
  | (fid, disk) => {
      let disk' = packFile(fid, disk)->tidy
      packBelow(fid - 1, disk')
    }
  }
}

let packFiles: rDisk => rDisk = disk => {
  let maxID = disk->Array.map(fileID)->Utils.maxIntInArray
  packBelow(maxID, disk)
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
  let (_isFile, _pos, _fileID, disk, free) =
    diskMap->Array.reduce((true, 0, 0, M.make(), S.empty), expandMapItem)
  (disk, free)
}

let showDisk: disk => string = disk => {
  let pMax = M.maxKey(disk)->Option.getUnsafe
  let str = ref("")
  for i in 0 to pMax {
    let char = switch M.get(i, disk) {
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
    let char = S.contains(i, free) ? "+" : "."
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

        (disk'->M.set(to, fID, _), free'->S.add(from, _))
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
  let diskMap = data->parse
  diskMap->log2("diskMap", _)
  let rDisk = diskMap->expandRDisk
  rDisk->log2("rDisk", _)
  let rDisk' = packFiles(rDisk)
  rDisk'->log2("rDisk'", _)

  let (disk, free) = rDisk'->toBlocks

  // todo: the sum is too big for int, use sumBigIntArray
  //  disk'->M.toArray->log
  disk
  ->M.toArray
  ->Array.map(((k, v)) => k->BigInt.fromInt * v->BigInt.fromInt)
  ->Array.reduce(_, 0n, BigInt.add)
}
