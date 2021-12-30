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
  type t = {start: Point.t, end: Point.t}

  let fromArray = xs => {
    start: xs[0]->Option.getExn->Point.fromArray,
    end: xs[1]->Option.getExn->Point.fromArray,
  }

  let toPoints = t => {
    let (start, end) = (t.start, t.end)
    start.x == end.x
      ? {
          let a = minIntInArray([start.y, end.y])
          let b = maxIntInArray([start.y, end.y])
          let points = ref([])
          Range.forEach(a, b, i => {
            points := Array.concat(points.contents, [Point.make(start.x, i)])
          })
          points.contents
        }
      : start.y == end.y
      ? {
        let a = minIntInArray([start.x, end.x])
        let b = maxIntInArray([start.x, end.x])
        let points = ref([])
        Range.forEach(a, b, i => {
          points := Array.concat(points.contents, [Point.make(i, start.y)])
        })
        points.contents
      }
      : raise(Not_found)
  }

  let onlyVertOrHoriz = t => {
    let (start, end) = (t.start, t.end)
    start.x == end.x || start.y == end.y
  }
}

module Lines = {
  type t = array<Line.t>
  let make = Array.map(_, Line.fromArray)
  let size = t => {
    t->Array.reduce(({x: 0, y: 0}: Point.t), (a, l: Line.t) => {
      let (start, end) = (l.start, l.end)
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

let solvePart1 = data => {
  let lines = data->parse->Lines.make

  let size = lines->Lines.size

  let vents = Array2D.make((size.x + 1, size.y + 1), 0)

  let points = lines->Array.keepMap(x => {
    x->Line.onlyVertOrHoriz
      ? {
          x->Line.toPoints->Some
        }
      : None
  })
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
