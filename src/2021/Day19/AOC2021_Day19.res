open Belt
open Utils
let log = Js.Console.log
open! FP_Utils

module Scanner = {
  // Coord Type [should refactor out]
  //
  type coord = Coord(Linear.V3.t<int>)

  let coordToString = (Coord(x, y, z)) => j`($x, $y, $z)`

  type transform = coord => coord

  let transformToString = trans => {
    Coord(1, 2, 3)->trans->coordToString
  }

  let nullTrans = identity

  let rotX: transform = (Coord(x, y, z)) => Coord(x, -z, y)
  let rotY: transform = (Coord(x, y, z)) => Coord(z, y, -x)
  let rotZ: transform = (Coord(x, y, z)) => Coord(-y, x, z)

  let translate = (Coord(x1, y1, z1), Coord(x2, y2, z2)) => Coord(x1 + x2, y1 + y2, z1 + z2)

  let rotations: array<transform> = {
    let ras = [
      nullTrans,
      rotY,
      compose(rotY, rotY),
      composeN([rotY, rotY, rotY]),
      rotZ,
      composeN([rotZ, rotZ, rotZ]),
    ]
    let rbs = [nullTrans, rotX, compose(rotX, rotX), composeN([rotX, rotX, rotX])]

    combinationArray2(ras, rbs, (a, b) => compose(a, b))
  }

  rotations->Array.length->log
  rotations->Printable.Array.toString(transformToString)->log

  module I = {
    type t = int
    let compare = Pervasives.compare
  }
  module Bag = Bag.Bag // namespaced in Bag package
  module B = Bag.Make(I) // MultiSet Bag of int
  let bagFromArray = Array.reduce(_, B.empty, (acc, x) => acc->B.add(x, _))
  let bagToString = b => {
    let str = ref("")
    b->B.iter((x, m) => {
      str := str.contents ++ j`@ $x:$m,`
    }, _)
    "{" ++ str.contents ++ "}"
  }
  /**
    type of Scanner
 */
  type t = {
    scannerName: int,
    beacons: array<coord>,
    transformation: transform,
    signature: B.t,
  }

  let toString = t =>
    t->Printable.Array.toString(({scannerName, beacons, transformation, signature}) => {
      j`scannerName: $scannerName, ` ++
      "beacons: " ++
      beacons->Printable.Array.toString((Coord(x, y, z)) => j`($x, $y, $z)`) ++
      "\n" ++
      `signature: ${signature->bagToString}` ++ "\n"
    })

  let sign = (bcns: array<coord>): B.t => {
    let pythag = (Coord(x, y, z)) => {
      x * x + y * y + z * z
    }

    combinationIfArray2(bcns, bcns, (a, b) => {
      a < b ? Some(pythag(a) - pythag(b)) : None
    })->bagFromArray
  }

  let make = ((name, beacons)) => {
    scannerName: name,
    beacons: beacons,
    transformation: nullTrans,
    signature: sign(beacons),
  }
}

let parse = data => {
  module Str = Js.String2
  let parseOne = data => {
    let lines = data->splitNewline->Array.map(Str.trim)
    let name =
      lines
      ->Array.getExn(0)
      ->Str.replace("--- scanner ", "")
      ->Str.replace(" ---", "")
      ->Int.fromString
      ->Option.getExn
    let coords =
      lines
      ->Array.sliceToEnd(1)
      ->Array.map(line => {
        let c = line->Str.split(",")->Array.map(x => x->Int.fromString->Option.getExn)
        Scanner.Coord(c->Array.getExn(0), c->Array.getExn(1), c->Array.getExn(2))
      })

    (name, coords)
  }
  data->splitDoubleNewline->Array.map(parseOne)
}

let solvePart1 = data => {
  let scanners = data->parse->Array.map(Scanner.make)
  scanners->Scanner.toString->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
