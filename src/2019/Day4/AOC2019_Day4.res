open Belt
open Utils
let log = Js.Console.log

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

//
// should use list comprehension but ReScript doesn't support it yet
//
let candidates = {
  let ret = ref([])
  Array.range(1, 6)->Array.forEach(d1 => {
    Array.range(d1, 9)->Array.forEach(d2 => {
      Array.range(d2, 9)->Array.forEach(
        d3 => {
          Array.range(d3, 9)->Array.forEach(
            d4 => {
              Array.range(d4, 9)->Array.forEach(
                d5 => {
                  Array.range(d5, 9)->Array.forEach(
                    d6 => {
                      let fofi = Float.fromInt
                      ret :=
                        Array.concat(
                          ret.contents,
                          [(d1->fofi, d2->fofi, d3->fofi, d4->fofi, d5->fofi, d6->fofi)],
                        )
                    },
                  )
                },
              )
            },
          )
        },
      )
    })
  })
  ret.contents
}

let numify = ((d1, d2, d3, d4, d5, d6)) => {
  let exp = (b, e) => Js.Math.pow_float(~base=b, ~exp=e)
  d1 *. exp(10.0, 5.0) +.
  d2 *. exp(10.0, 4.0) +.
  d3 *. exp(10.0, 3.0) +.
  d4 *. exp(10.0, 2.0) +.
  d5 *. exp(10.0, 1.0) +.
  d6
}

let adjacentSame = ((d1, d2, d3, d4, d5, d6)) => {
  d1 == d2 || d2 == d3 || d3 == d4 || d4 == d5 || d5 == d6
}

let isolatedAdjacentSame = ((d1, d2, d3, d4, d5, d6)) => {
  (d1 == d2 && d2 != d3) ||
  d1 != d2 && d2 == d3 && d3 != d4 ||
  d2 != d3 && d3 == d4 && d4 != d5 ||
  d3 != d4 && d4 == d5 && d5 != d6 ||
  (d4 != d5 && d5 == d6)
}

let inRange = (lowerLimit, upperLimit, digits) => {
  let n = numify(digits)
  n >= lowerLimit && n <= upperLimit
}

let part1 = (lowerLimit, upperLimit) => {
  open Array
  candidates->keep(adjacentSame)->keep(inRange(lowerLimit, upperLimit, _))->length
}

let part2 = (lowerLimit, upperLimit) => {
  open Array
  candidates->keep(isolatedAdjacentSame)->keep(inRange(lowerLimit, upperLimit, _))->length
}

let solvePart1 = data => {
  data->ignore
  part1(231832.0, 767346.0)
}

let solvePart2 = data => {
  data->ignore
  part2(231832.0, 767346.0)
}
