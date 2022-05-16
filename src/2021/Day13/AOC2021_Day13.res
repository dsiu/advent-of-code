open Belt
open Utils
let log = Js.Console.log

module Paper = {
  type mark = [#"#" | #"."]

  let addMark = (m1, m2) => {
    switch (m1, m2) {
    | (#"#", _) => #"#"
    | (_, #"#") => #"#"
    | (_, _) => #"."
    }
  }

  let countMark = p =>
    p->Array2D.reduce(0, (a, mark) => {
      switch mark {
      | #"#" => a + 1
      | _ => a
      }
    })

  type paper = Array2D.t<mark>
  type coord = (int, int)
  type fold_direction = [#X | #Y]
  type fold = (fold_direction, int)

  let makeFoldDirection = s => {
    switch s {
    | "x" => #X
    | "y" => #Y
    | _ => raise(Not_found)
    }
  }

  type t = {
    paper: paper,
    folds: array<fold>,
  }

  let getPaper = t => t.paper
  let getFolds = t => t.folds

  let findSize = t => {
    t
    ->Array.reduce((0, 0), ((xa, ya), (x, y)) => {
      (x > xa ? x : xa, y > ya ? y : ya)
    })
    ->(((x, y)) => {(x + 1, y + 1)})
  }

  let makePaper = (coords: array<coord>) => {
    let sizes = findSize(coords)
    let p = Array2D.make(sizes, #".")
    Array.forEach(coords, c => {
      p->Array2D.set(c, #"#") ? () : raise(Not_found)
    })
    p
  }

  let makeFolds = folds => {
    folds->Array.map(((s, i)) => {
      (s->makeFoldDirection, i)
    })
  }

  let make = (coords: array<coord>, folds: array<(string, int)>) => {
    {paper: coords->makePaper, folds: makeFolds(folds)}
  }

  let transformCoord = ((x, y): coord, (dir, n): fold) => {
    switch dir {
    | #X => x > n ? (2 * n - x, y) : (x, y)
    | #Y => y > n ? (x, 2 * n - y) : (x, y)
    }
  }

  let getTransformedSize = (t, (dir, n): fold) => {
    let (sizeX, sizeY) = (t->Array2D.lengthX, t->Array2D.lengthY)
    switch dir {
    | #X => (n, sizeY)
    | #Y => (sizeX, n)
    }
  }

  let transform = (t: paper, f: fold) => {
    let new_size = getTransformedSize(t, f)
    let t' = Array2D.make(new_size, #".")

    t->Array2D.reduceWithIndex(t', (a, mark, coord) => {
      let coord' = transformCoord(coord, f)
      let new_mark = t->Array2D.get(coord')->Option.getExn
      a->Array2D.set(coord', addMark(mark, new_mark))->ignore
      a
    })
  }

  let transformFirst = t => {
    transform(t->getPaper, getFolds(t)[0]->Option.getExn)
  }

  let transformAll = t => {
    let folds = getFolds(t)
    folds->Array.reduce(t->getPaper, (a, f) => {
      transform(a, f)
    })
  }

  let toString = t => {
    t->Array.map(x => x->Js.Array2.joinWith(""))->Js.Array2.joinWith("\n")
  }
}

let parse = data => {
  let parsed = data->splitDoubleNewline
  let coords = parsed[0]->Option.getExn
  let folds = parsed[1]->Option.getExn

  open Js.String2
  open Option
  (
    coords
    ->splitNewline
    ->Array.map(x => {
      let s = x->trim->split(",")
      (s[0]->flatMap(Int.fromString)->getExn, s[1]->flatMap(Int.fromString)->getExn)
    }),
    folds
    ->splitNewline
    ->Array.map(x => {
      let i = length("fold along ")
      let sub = x->trim->slice(~from=i, ~to_=x->length)
      let s = sub->split("=")
      (s[0]->getExn, s[1]->flatMap(Int.fromString)->getExn)
    }),
  )
}

let solvePart1 = data => {
  let (coords, folds) = data->parse
  //  coords->Js.log2("coords", _)
  //  folds->Js.log2("folds", _)
  let p = Paper.make(coords, folds)

  //  Js.log("p")
  //  p->Paper.getPaper->Paper.toString->Js.log
  //  p->Paper.getFolds->Js.log

  let p1 = p->Paper.transformFirst
  //  Js.log("p1")
  //  p1->Paper.toString->Js.log
  p1->Paper.countMark
}

let solvePart2 = data => {
  let (coords, folds) = data->parse
  //  coords->Js.log2("coords", _)
  //  folds->Js.log2("folds", _)
  let p = Paper.make(coords, folds)

  //  Js.log("p")
  //  p->Paper.getPaper->Paper.toString->Js.log
  //  p->Paper.getFolds->Js.log

  let p1 = p->Paper.transformAll
  //  Js.log("p1")
  //  p1->Paper.toString->Js.log
  p1->Paper.countMark
}
