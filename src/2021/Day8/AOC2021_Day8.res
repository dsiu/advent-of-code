open Belt
open Utils
let log = Js.Console.log

module Digit = {
  type t = array<string> // char

  let make = x => x->Utils.splitChars
  let size = Array.length
  let map = Array.map

  let makeSet = Set.String.fromArray
  let getExn = Array.getExn

  let eq = (a, b) => Set.String.eq(a->makeSet, b->makeSet)
  let has = (t, x) => t->makeSet->Set.String.has(x)
  let intersect = (a, b) => Set.String.intersect(a->makeSet, b->makeSet)->Set.String.toArray

  let mustHave = (t, ~must_have) => t->intersect(must_have)->eq(must_have)

  let diff = (a, b) => Set.String.diff(a->makeSet, b->makeSet)->Set.String.toArray
  let union = (a, b) => Set.String.union(a->makeSet, b->makeSet)->Set.String.toArray
  let keepSize = (t, size) => t->Array.keep(x => x->Array.size === size)
}

module Segments = {
  type t = Map.Int.t<Digit.t>

  let defaults = [
    (0, "abcefg"->Digit.make), // 0
    (1, "cf"->Digit.make), // 1
    (2, "acdeg"->Digit.make), // 2
    (3, "acdfg"->Digit.make), // 3
    (4, "bcdf"->Digit.make), // 4
    (5, "abdfg"->Digit.make), // 5
    (6, "abdefg"->Digit.make), // 6
    (7, "acf"->Digit.make), // 7
    (8, "abcdefg"->Digit.make), // 8
    (9, "abcdfg"->Digit.make), // 9
  ]

  let default_0to9 = Map.Int.fromArray(defaults)

  let getSegments = (t, n: int) => t->Map.Int.get(n)->Option.getExn
  let getDefaultSegments = getSegments(default_0to9, _)

  let getSegmentSize = (t, n: int) => t->getSegments(n)->Digit.size
  let getDefaultSegmentSize = getSegmentSize(default_0to9, _)

  let translate = (t, d: Digit.t) => {
    t->Map.Int.keep((_, v) => Digit.eq(v, d))->Map.Int.keysToArray->Array.getExn(0)
  }
}

module Entry = {
  type t = {
    inputs: array<Digit.t>,
    outputs: array<Digit.t>,
    segment_map: Segments.t,
  }

  // this is totally NOT type safe
  module Algo = {
    let find_1 = xs => xs->Digit.keepSize(2)->Digit.getExn(0)
    let find_4 = xs => xs->Digit.keepSize(4)->Digit.getExn(0)
    let find_7 = xs => xs->Digit.keepSize(3)->Digit.getExn(0)
    let find_8 = xs => xs->Digit.keepSize(7)->Digit.getExn(0)

    let find_0_6_9 = (xs, one, four, seven) => {
      let has_6_segs = xs->Digit.keepSize(6) // can be 0, 6, or 9

      let union_4_7 = Digit.union(four, seven)
      let nine = has_6_segs->Array.keep(Digit.mustHave(~must_have=union_4_7))->Digit.getExn(0)

      let six_or_zero = has_6_segs->Array.keep(x => !Digit.mustHave(x, ~must_have=union_4_7))

      let diff_4_1 = Digit.diff(four, one)
      let six = six_or_zero->Array.keep(Digit.mustHave(~must_have=diff_4_1))->Digit.getExn(0)
      let zero =
        six_or_zero->Array.keep(x => !Digit.mustHave(x, ~must_have=diff_4_1))->Digit.getExn(0)

      (zero, six, nine)
    }

    let find_2_3_5 = (xs, eight, six, nine) => {
      let has_5_segs = xs->Digit.keepSize(5) // can be 2, 3, 5

      let diff_8_9 = Digit.diff(eight, nine)
      let two = has_5_segs->Array.keep(Digit.mustHave(~must_have=diff_8_9))->Digit.getExn(0)

      let three_or_five = has_5_segs->Array.keep(x => !Digit.mustHave(x, ~must_have=diff_8_9))

      let diff_8_6 = Digit.diff(eight, six)
      let three = three_or_five->Array.keep(Digit.mustHave(~must_have=diff_8_6))->Digit.getExn(0)
      let five =
        three_or_five->Array.keep(x => !Digit.mustHave(x, ~must_have=diff_8_6))->Digit.getExn(0)

      (two, three, five)
    }
  }

  let makeSegments = (xs: array<Digit.t>) => {
    let one = xs->Algo.find_1
    let four = xs->Algo.find_4
    let seven = xs->Algo.find_7
    let eight = xs->Algo.find_8
    let (zero, six, nine) = xs->Algo.find_0_6_9(one, four, seven)
    let (two, three, five) = xs->Algo.find_2_3_5(eight, six, nine)

    [
      (0, zero),
      (1, one),
      (2, two),
      (3, three),
      (4, four),
      (5, five),
      (6, six),
      (7, seven),
      (8, eight),
      (9, nine),
    ]->Map.Int.fromArray
  }

  let translate = (t, d) => {
    t.segment_map->Segments.translate(d)
  }

  let translateOutputs = t => {
    t.outputs->Array.map(translate(t, _))
  }

  let make = (inputStr, outputStr): t => {
    let inputs = inputStr->Js.String2.split(" ")->Array.map(Digit.make)
    let outputs = outputStr->Js.String2.split(" ")->Array.map(Digit.make)

    {inputs: inputs, outputs: outputs, segment_map: inputs->makeSegments}
  }

  let countMatchLen = (digits, matches) => {
    let match_len_set =
      matches->Array.map(x => {x->Segments.getDefaultSegmentSize})->Set.Int.fromArray
    digits->Array.reduce(0, (a, x) => {
      match_len_set->Set.Int.has(x->Digit.size) ? a + 1 : a
    })
  }

  let countOutputMatchLen = (t, matches) => countMatchLen(t.outputs, matches)
  let countInputMatchLen = (t, matches) => countMatchLen(t.inputs, matches)
}

let parse = data => {
  data
  ->splitNewline
  ->Array.map(x => {
    let y = x->Js.String2.trim->Js.String2.split(" | ")
    Entry.make(y[0]->Option.getUnsafe, y[1]->Option.getUnsafe)
  })
}

let solvePart1 = data => {
  let parsed = data->parse

  //  parsed->Array.forEach(Js.log)
  parsed->Array.reduce(0, (a, x) => a + x->Entry.countOutputMatchLen([1, 4, 7, 8]))
}

let solvePart2 = data => {
  let parsed = data->parse
  //  parsed->Array.forEach(Js.log)
  let outputs =
    parsed->Array.map(e =>
      e->Entry.translateOutputs->Js.Array2.joinWith("")->Int.fromString->Option.getExn
    )
  outputs->Array.reduce(0, Utils.sum)
}
