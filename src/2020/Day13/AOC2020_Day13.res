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

let solvePart2BruteForce = xs => {
  open ReScriptJs.Js

  let big_zero = BigInt.fromInt(0)
  let big_one = BigInt.fromInt(1)

  let rec helper = (time, xs) => {
    xs->Belt.Array.reduceU(true, (. acc, (bus, delta)) => {
      acc && BigInt.mod(BigInt.add(time, delta), bus) === big_zero ? true : false
    })
      ? time
      : helper(BigInt.add(time, big_one), xs)
  }

  //  helper(BigInt.fromInt(1068773), xs)
  helper("100000000000000"->BigInt.fromString, xs)
  //  helper("1000000000"->BigInt.fromString, xs)
}

//
// use Chinese Remainder Theorum
// https://0xdf.gitlab.io/adventofcode2020/13
//
// for each bus b, let i be its index in the list
// need to solve (t+i) mod b = 0
//
// rearrange, thus:
// (t+i) === 0 mod b
// t === -i mod b
// t === b-i mod b
//
let part2 = xs => {
  let rem = ref([])
  let num = ref([])
  xs->Array.forEach(((bus, delta)) => {
    rem := rem.contents->Array.concat([ReScriptJs.Js.BigInt.sub(bus, delta)])
    num := num.contents->Array.concat([bus])
  })

  ChineseRemainder.crtBigInt(rem.contents, num.contents)
}

let parse2 = data => {
  let lines =
    data
    ->splitNewline
    ->Array.map(x => {
      x->Js.String2.trim
    })

  //  let start = lines->Array.get(0)->Option.getExn->Int.fromString->Option.getExn
  let bus_list =
    lines
    ->Array.get(1)
    ->Option.getExn
    ->Js.String2.split(",")
    ->Array.reduceWithIndex([], (acc, x, i) => {
      x !== "x"
        ? acc->Array.concat([(x->ReScriptJs.Js.BigInt.fromString, i->ReScriptJs.Js.BigInt.fromInt)])
        : acc
    })
  bus_list
}

let solvePart1 = data => {
  let (bus, min) = data->parse->findEarliestBus
  bus * min
}

let solvePart2 = data => {
  data->parse2->part2
}
