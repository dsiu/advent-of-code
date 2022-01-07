open Belt
open Utils
let log = Js.Console.log

module Point = {
  type t = {x: int, y: int}

  let fromArray = xs => {
    x: xs[0]->Option.getExn,
    y: xs[1]->Option.getExn,
  }

  let make = (x, y) => {x: x, y: y}
}

module Line = {
  type line = {start: Point.t, end: Point.t}

  type t = Horizontal(line) | Vertical(line) | Diagonal(line) | Other(line)

  let getLine = t => {
    switch t {
    | Horizontal(line)
    | Vertical(line)
    | Diagonal(line)
    | Other(line) => line
    }
  }

  let toString = t => {
    let line = t->getLine
    let (start, end) = (line.start, line.end)

    let itos = Int.toString

    switch t {
    | Horizontal(_) => "Horizontal"
    | Vertical(_) => "Vertical"
    | Diagonal(_) => "Diagonal"
    | Other(_) => "Other"
    } ++
    ` : (${start.x->itos},${start.y->itos}), (${end.x->itos},${end.y->itos})`
  }

  let fromArray = xs => {
    let start = xs[0]->Option.getExn->Point.fromArray
    let end = xs[1]->Option.getExn->Point.fromArray
    start.x == end.x
      ? Vertical({start: start, end: end})
      : start.y == end.y
      ? Horizontal({start: start, end: end})
      : Js.Math.abs_int(start.x - end.x) == Js.Math.abs_int(start.y - end.y)
      ? Diagonal({start: start, end: end})
      : Other({start: start, end: end})
  }

  let toPoints2 = t => {
    switch t {
    | Vertical(l) => {
        let (start, end) = (l.start, l.end)

        let a = minIntInArray([start.y, end.y])
        let b = maxIntInArray([start.y, end.y])
        let points = ref([])
        Range.forEach(a, b, i => {
          points := Array.concat(points.contents, [Point.make(start.x, i)])
        })
        points.contents
      }
    | Horizontal(l) => {
        let (start, end) = (l.start, l.end)

        let a = minIntInArray([start.x, end.x])
        let b = maxIntInArray([start.x, end.x])
        let points = ref([])
        Range.forEach(a, b, i => {
          points := Array.concat(points.contents, [Point.make(i, start.y)])
        })
        points.contents
      }
    | Diagonal(l) => []
    | _ => raise(Not_found)
    }
  }

  let cmp = (a, b) => {
    a === b ? 0 : a > b ? 1 : -1
  }

  let makeRange = (start, end) => {
    switch cmp(end, start) {
    | 1 => Belt.Array.range(start, end)
    | -1 => Belt.Array.range(end, start)->Array.reverse
    | _ => []
    }
  }

  let makePoints = (start: Point.t, end: Point.t) => {
    //    Js.log2("start=", start)
    //    Js.log2("end=", end)
    let diff_x = Js.Math.abs_int(end.x - start.x)
    let diff_y = Js.Math.abs_int(end.y - start.y)
    let xs = start.x != end.x ? makeRange(start.x, end.x) : Array.make(diff_y + 1, start.x)
    let ys = start.y != end.y ? makeRange(start.y, end.y) : Array.make(diff_x + 1, start.y)

    //    Js.log2("xs", xs)
    //    Js.log2("ys", ys)
    Array.zipBy(xs, ys, (x, y) => Point.make(x, y))
  }

  let toPoints = t => {
    switch t {
    | Vertical(l)
    | Horizontal(l)
    | Diagonal(l) => {
        let (start, end) = (l.start, l.end)
        makePoints(start, end)
      }
    | _ => raise(Not_found)
    }
  }

  //  let onlyVertOrHoriz = t => {
  //    let (start, end) = (t.start, t.end)
  //    start.x == end.x || start.y == end.y
  //  }

  let onlyVertOrHoriz = t => {
    switch t {
    | Horizontal(_)
    | Vertical(_) => true
    | _ => false
    }
  }

  let onlyVertOrHorizOrDiagonal = t => {
    switch t {
    | Horizontal(_)
    | Vertical(_)
    | Diagonal(_) => true
    | _ => false
    }
  }
}

module Lines = {
  type t = array<Line.t>
  let make = Array.map(_, Line.fromArray)
  let size = t => {
    t->Array.reduce(({x: 0, y: 0}: Point.t), (a, l: Line.t) => {
      let line = l->Line.getLine
      let (start, end) = (line.start, line.end)
      {
        x: maxIntInArray([a.x, start.x, end.x]),
        y: maxIntInArray([a.y, start.y, end.y]),
      }
    })
  }
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(l => {
    l
    ->Js.String2.trim
    ->Js.String2.split(" -> ")
    ->Array.map(x => {
      x
      ->Js.String2.split(",")
      ->Array.map(x => {
        x->Int.fromString->Option.getExn
      })
    })
  })

module Vents = {
  type t = Array2D.t<int>

  let make = Array2D.make

  let update = (t, (x, y), fn) => {
    t->Array2D.set((x, y), t->Array2D.getExn((x, y))->fn)
  }

  // count if fn returns true
  let count = (t, fn) => {
    t->Array.reduce(0, (a, y) => {
      a +
      y->Array.reduce(0, (a, x) => {
        x->fn ? a + 1 : a
      })
    })
  }
}

let solve = (data, filter) => {
  let lines = data->parse->Lines.make
  //  lines->Array.forEach(x => x->Line.toString->log)
  let size = lines->Lines.size
  let points = lines->Array.keepMap(x => {
    x->filter ? x->Line.toPoints->Some : None
  })

  //  points->log
  let add1 = Utils.add(_, 1)
  let vents = Vents.make((size.x + 1, size.y + 1), 0)

  points
  ->Utils.flatten
  ->Array.forEach(p => {
    vents->Vents.update((p.x, p.y), add1)->ignore
  })

  //  vents->Array.map(Js.log)->ignore
  vents->Vents.count(x => x >= 2)
}
let solvePart1 = solve(_, Line.onlyVertOrHoriz)

let solvePart2 = solve(_, Line.onlyVertOrHorizOrDiagonal)
