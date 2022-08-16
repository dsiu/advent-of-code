open Belt
open Utils
let log = Js.Console.log
open! FP_Utils

module Scanner = {
  type coord = Coord(int, int, int)
  type transform = coord => coord
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
}

let parse = data => {
  module Str = Js.String2
  let parseOne = data => {
    let lines = data->splitNewline->Array.map(Str.trim)
    let name = lines->Array.getExn(0)->Str.replace("--- scanner ", "")->Str.replace(" ---", "")
    let coords =
      lines
      ->Array.sliceToEnd(1)
      ->Array.map(line => {
        let c = line->Str.split(",")->Array.map(Int.fromString)
        (c->Array.getExn(0), c->Array.getExn(1), c->Array.getExn(2))
      })

    (name, coords)
  }
  data->splitDoubleNewline->Array.map(parseOne)
}

let solvePart1 = data => {
  let scanners = data->parse
  scanners
  ->Printable.Array.toString(((name, coords)) => {
    `name: ${name}, ` ++
    "coords: " ++
    coords->Printable.Array.toString(((x, y, z)) => j`($x, $y, $z)`) ++ "\n"
  })
  ->log
}

let solvePart2 = data => {
  data->ignore
  2
}
