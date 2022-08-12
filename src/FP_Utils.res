//
// List
//
module List = Belt.List

let flatMapList: (List.t<'a>, 'a => List.t<'b>) => List.t<'b> = (xs, f) => {
  List.reduce(List.map(xs, f), list{}, List.concat)
}

//
// Array
//
module Array = Belt.Array

let flatMapArray: (array<'a>, 'a => array<'b>) => array<'b> = (xs, f) => {
  Array.reduce(Array.map(xs, f), [], Array.concat)
}

let foldlArray = (xs, f) => {
  let init = xs->Array.getExn(0)
  let rest = xs->Array.sliceToEnd(1)
  rest->Array.reduce(init, f)
}

let foldrArray = (xs, f) => {
  let end = xs->Array.length - 1
  let init = xs->Array.getExn(end)
  let rest = xs->Array.slice(~offset=0, ~len=end)
  rest->Array.reduceReverse(init, f)
}

let identity = (a: 'a) => a
let eq = (x, y) => x === y

// compose(f, g, x) = g(f(x))
type composeU<'a, 'b, 'c> = ((. 'a) => 'b, (. 'b) => 'c, 'a) => 'c
let composeU: composeU<'a, 'b, 'c> = (f, g, x) => g(. f(. x))

type compose<'a, 'b, 'c> = ('a => 'b, 'b => 'c, 'a) => 'c
let compose: compose<'a, 'b, 'c> = (f, g, x) => g(f(x))

let compose3 = (f, g, h, x) => h(g(f(x)))
let compose4 = (f, g, h, i, x) => i(h(g(f(x))))

let composeN = fs =>
  fs->Array.sliceToEnd(1)->Array.reduce(fs->Array.getExn(0), (a, f) => compose(a, f))

// Options
// refactor this
let optionOr = (a, b) => {
  switch a {
  | Some(_) => a
  | None => b
  }
}
