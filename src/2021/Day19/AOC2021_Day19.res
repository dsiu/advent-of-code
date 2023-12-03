open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2

let {compose, composeN} = module(Stdlib_Function)

module Scanner = {
  // Coord Type [should refactor out]
  //
  open Linear
  type coord = Coord(V3.t<float>)

  let coordToString: coord => string = (Coord((x, y, z))) =>
    `(${x->Float.toString}, ${y->Float.toString}, ${z->Float.toString})`

  type transform = coord => coord

  let transformToString = trans => {
    Coord(0., 0., 0.)->trans->coordToString
  }

  let nullTrans = identity

  let rotX: transform = (Coord(x, y, z)) => Coord(x, -.z, y)
  let rotY: transform = (Coord(x, y, z)) => Coord(z, y, -.x)
  let rotZ: transform = (Coord(x, y, z)) => Coord(-.y, x, z)

  let translate = (Coord(x1, y1, z1), Coord(x2, y2, z2)) => Coord(x1 +. x2, y1 +. y2, z1 +. z2)

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

    Stdlib.Array.combination2(ras, rbs, (. a, b) => compose(a, b))
  }

  //  rotations->Array.length->log
  //  rotations->Printable.Array.toString(transformToString)->log

  module I = {
    type t = int
    let compare = Pervasives.compare
  }
  module F = {
    type t = float
    let compare = (a, b) => Float.toInt(a -. b)
  }
  module Bag = Bag.Bag // namespaced in Bag package
  module B = Bag.Make(F) // MultiSet Bag of int

  let bagFromArray = Array.reduce(_, B.empty, (acc, x) => acc->B.add(x, _))

  let bagToString = b => {
    let str = ref("")
    b->B.iter((x, m) => {
      str := str.contents ++ `@ ${x->Float.toString}:${m->Int.toString},`
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

  let eq = (a, b) => {a.scannerName == b.scannerName}

  let toString = t =>
    t->Printable.Array.toString(({scannerName, beacons, transformation: _, signature}) => {
      `scannerName: ${scannerName->Int.toString}, ` ++
      "beacons: " ++
      beacons->Printable.Array.toString((Coord(x, y, z)) =>
        `(${x->Float.toString}, ${y->Float.toString}, ${z->Float.toString})`
      ) ++
      ", " ++
      `signature: ${signature->bagToString}` ++ "\n"
    })

  // should refactor
  let minus = (Coord(a), Coord(b)) => {
    let (x1, y1, z1) = a
    let (x2, y2, z2) = b
    Coord(x1 -. x2, y1 -. y2, z1 -. z2)
  }

  let sign = (bcns: array<coord>): B.t => {
    let pythag = (Coord(x, y, z)) => {
      x *. x +. y *. y +. z *. z
    }

    Stdlib.Array.combinationIf2(bcns, bcns, (. Coord(a), Coord(b)) => {
      V3.cmp(b, a) > 0 ? Some(pythag(minus(Coord(a), Coord(b)))) : None
    })->bagFromArray
  }

  let vagueMatch = (scanner1, scanner2) => {
    let s1 = scanner1.signature
    let s2 = scanner2.signature
    let s = B.inter(s1, s2)->B.elements->List.size
    s >= 12 * 11 / 2
  }

  module V3Comparator = Belt.Id.MakeComparable({
    type t = coord
    let cmp = (Coord(a), Coord(b)) => V3.cmp(a, b)
  })
  // let v3_set = Belt.Set.make(~id=module(V3Comparator))
  let v3SetFromArray = a => {
    Belt.Set.fromArray(a, ~id=module(V3Comparator))
  }

  let v3SetMake = Belt.Set.make(~id=module(V3Comparator))

  // should refactor
  let interact = (a, b) => {
    let sa = v3SetFromArray(a)
    let sb = v3SetFromArray(b)
    Belt.Set.intersect(sa, sb)
  }

  let matchingTransformAll = (scanner1, scanner2): array<transform> => {
    let beacons1 = scanner1.beacons
    let beacons2 = scanner2.beacons

    Stdlib.Array.combinationIf3(beacons1, beacons2, rotations, (. b1, b2, rot) => {
      let t = minus(b1, rot(b2)) // apply rot to b2
      let translation = translate(t)

      let transB2 = beacons2->Array.mapU((. b) => {b->rot->translation})

      let len = interact(beacons1, transB2)->Belt.Set.size
      len >= 12 ? Some(compose(rot, translation)) : None
    })
  }

  let matchingTransform = (scanner1, scanner2): option<transform> => {
    matchingTransformAll(scanner1, scanner2)->Stdlib.Array.arrayToOption
  }

  type reconstruction = Reconstruction({found: list<t>, working: list<t>, waiting: list<t>})

  let mkReconstruction = scanners => {
    switch scanners {
    | list{s, ...ss} =>
      Reconstruction({
        found: list{},
        working: list{s},
        waiting: ss,
      })
    | list{} => raise(Invalid_argument("empty scanners"))
    }
  }

  let transformScanner = (. (s, trans)) => {
    ...s,
    beacons: s.beacons->Array.mapU((. b) => {
      Option.getExn(trans)(b)
    }),
    transformation: trans->Option.getExn,
  }

  let reconstructStep = (Reconstruction({found, working, waiting})) => {
    switch working {
    | list{current, ...workers} => {
        let passMatches = waiting->List.keep(x => vagueMatch(current, x))
        let matches =
          List.zip(
            passMatches,
            List.mapU(passMatches, (. x) => matchingTransform(current, x)),
          )->List.keep(x => x->snd->Option.isSome)

        let waiting' = waiting->List.keep(s => {
          matches->List.map(fst)->List.has(s, eq) ? false : true
        })

        let newWorker = matches->List.mapU(transformScanner)

        Reconstruction({
          found: list{current, ...found},
          working: List.concat(workers, newWorker),
          waiting: waiting',
        })
      }

    | list{} => raise(Invalid_argument("empty working scanner"))
    }
  }

  let rec reconstruct = (Reconstruction({found: _, working, waiting: _}) as r) => {
    switch working {
    | list{} => r
    | _ => reconstruct(reconstructStep(r))
    }
  }

  let make = ((name, beacons)) => {
    scannerName: name,
    beacons,
    transformation: nullTrans,
    signature: sign(beacons),
  }
}

let parse = data => {
  module Str = Js.String2
  let floatFromStr = Js.Float.fromString

  let parseOne = data => {
    let lines = data->splitNewline->Array.map(Str.trim)
    let name =
      lines
      ->Array.getExn(0)
      ->Str.replace("--- scanner ", "")
      ->Str.replace(" ---", "")
      ->intFromStringExn
    let coords =
      lines
      ->Array.sliceToEnd(1)
      ->Array.map(line => {
        let c = line->Str.split(",")->Array.map(floatFromStr)
        Scanner.Coord(c->Array.getExn(0), c->Array.getExn(1), c->Array.getExn(2))
      })

    (name, coords)
  }
  data->splitDoubleNewline->Array.map(parseOne)
}

open Scanner

let reconstructScanners = scanners => {
  let Reconstruction(r) = Scanner.reconstruct(mkReconstruction(scanners->List.fromArray))
  r.found
}

let part1 = scanners => {
  let bSets = scanners->List.mapU((. s) => {
    v3SetFromArray(s.beacons)
  })

  let result = bSets->List.reduceU(v3SetMake, (. a, b) => {
    Belt.Set.union(a, b)
  })

  //  result->Belt.Set.toArray->log
  result->Belt.Set.size
}

let part2 = scanners => {
  let extractOrigin = (. sc) => sc.transformation(Coord(0.0, 0.0, 0.0))
  let origins = scanners->List.mapU(extractOrigin)

  let manhatton = (Coord(a)) => {
    let (x1, y1, z1) = a
    abs_float(x1) +. abs_float(y1) +. abs_float(z1)
  }

  Stdlib.List.combination2(origins, origins, (. a, b) => {
    minus(a, b)->manhatton
  })
  ->List.sort((a, b) => Float.toInt(b -. a))
  ->Stdlib.List.listToOption
  ->Option.getExn
}

let solvePart1 = data => {
  let scanners = data->parse->Array.map(Scanner.make)
  //  scanners->Scanner.toString->log

  let newScanners = reconstructScanners(scanners)
  part1(newScanners)
}

let solvePart2 = data => {
  let scanners = data->parse->Array.map(Scanner.make)
  let newScanners = reconstructScanners(scanners)
  part2(newScanners)
}
