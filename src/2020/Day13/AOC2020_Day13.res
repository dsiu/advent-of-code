open Belt
open Utils
let log = Js.Console.log

let findEarliestBus = ((time_stamp, bus_numbers)) => {
  bus_numbers
  ->Array.map(x => (x, x - mod(time_stamp, x)))
  ->Array.reduce((0, 10000), ((ats, amin), (ts, min)) => {
    min < amin ? (ts, min) : (ats, amin)
  })
}

let parse = data => {
  let lines =
    data
    ->splitNewline
    ->Array.map(x => {
      x->Js.String2.trim
    })

  let timeStamp = lines->Array.get(0)->Option.getExn->Int.fromString->Option.getExn

  let isNotX = c => c !== "x"
  let bus_numbers =
    lines
    ->Array.get(1)
    ->Option.getExn
    ->Js.String2.split(",")
    ->Array.keep(isNotX)
    ->Array.map(x => {x->Int.fromString->Option.getExn})
  (timeStamp, bus_numbers)
}

let solvePart2 = xs => {
  //  open ReScriptJs.Js

  let rec helper = (time, xs) => {
    xs->Array.reduce(true, (acc, (bus, delta)) => {
      acc && mod(time, bus + delta) === 0 ? true : false
    })
      ? time
      : helper(time + 1, xs)
  }

  //  helper(BigInt.fromInt(1068773), xs)
  helper(1, xs)
}

let parse2 = data => {
  let lines =
    data
    ->splitNewline
    ->Array.map(x => {
      x->Js.String2.trim
    })
  let bus_list =
    lines
    ->Array.get(1)
    ->Option.getExn
    ->Js.String2.split(",")
    ->Array.reduceWithIndex([], (acc, x, i) => {
      x !== "x" ? acc->Array.concat([(x->Int.fromString->Option.getExn, i)]) : acc
    })
  bus_list->log
  bus_list
}

let solvePart1 = data => {
  let (bus, min) = data->parse->findEarliestBus
  bus * min
}

let solvePart2 = data => {
  "----"->log
  data->parse2->solvePart2->log
  "----"->log

  2
}
