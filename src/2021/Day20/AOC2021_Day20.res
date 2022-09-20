//
// https://work.njae.me.uk/2021/12/23/advent-of-code-2021-day-20/
//
open Belt
open Utils
let log = Js.Console.log

type pixel = Pixel(Linear.V2.t<int>)

module V2Comparator = Belt.Id.MakeComparable({
  type t = pixel
  let cmp = (Pixel(a), Pixel(b)) => Linear.V2.cmp(a, b)
})

let v2SetMake = Belt.Set.make(~id=module(V2Comparator))
type pixelSet = Belt.Set.t<V2Comparator.t, V2Comparator.identity>
type image = Image({grid: pixelSet, distantPixel: bool, explicitPixel: (pixel, pixel)})

//let findContents : (pixelSet, bool, (pixel, pixel), pixel) => bool= (grid, distant, region, here) => {
//
//}

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
