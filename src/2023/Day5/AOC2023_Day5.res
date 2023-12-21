@@uncurried

open RescriptCore
open Utils
let log = Console.log
let log2 = Console.log2

module Rule = {
  type t = {
    srcInterval: Interval.t,
    // destInterval: Interval.t,
    dest: BigInt.t,
    offset: BigInt.t,
  }

  let toString: t => string = t => {
    `Rule(Src:${t.srcInterval->Interval.toString}, Dest:${t.dest->BigInt.toString}, Offset:${t.offset->BigInt.toString})`
  }

  // run the rule with single input
  let run: (t, BigInt.t) => option<BigInt.t> = (t, srcNum) => {
    //    log2("srcNum", srcNum)
    //    t->toString->log
    t.srcInterval->Interval.contains(srcNum)
      ? {
          //   log2("(srcNum + t.offset)", srcNum + t.offset)
          Some(BigInt.add(srcNum, t.offset))
        }
      : {
          // log("Not in interval")
          None
        }
  }

  // run the rule with array of intervals
  let runWithInterval: (t, Interval.t) => (option<Interval.t>, option<Interval.t>) = (t, src) => {
    switch src->Interval.intersect(t.srcInterval) {
    | Some(i) => {
        log2("  intersect Some: ", i->Interval.add(t.offset))
        log2("  src: ", src)
        log2("  i: ", i)
        (Interval.remove(src, i), Some(i->Interval.add(t.offset)))
      }
    | None => {
        log("  intersect None: ")
        (src->Some, None)
      }
    }
  }
}

module AlmanacMap = {
  type t = {
    srcCategory: string,
    destCategory: string,
    rules: array<Rule.t>,
  }

  let toString: t => string = t => {
    `AlmanacMap(${t.srcCategory}, ${t.destCategory}, [${t.rules
      ->Array.map(Rule.toString)
      ->Array.joinWith(", ")}])`
  }

  let runRules: (t, BigInt.t) => BigInt.t = (t, srcNum) => {
    t.rules
    ->Array.findMap(r => r->Rule.run(srcNum))
    ->Option.getOr(srcNum)
  }

  let runRulesWithInterval: (t, Interval.t) => (option<Interval.t>, array<Interval.t>) = (
    t,
    src,
  ) => {
    t->toString->log
    t.rules->Array.reduce((Some(src), []), ((s, d), r) => {
      switch s {
      | Some(s) => {
          let (newSrc, dests) = Rule.runWithInterval(r, s)

          let newDests = switch dests {
          | Some(mapped) => Array.concat([mapped], d)
          | None => d
          }

          (newSrc, newDests)
        }
      | None => (None, d)
      }
    })
    //->Interval.sortAndMergeOverlaps
  }

  let runRulesWithMultiIntervals: (t, array<Interval.t>) => array<Interval.t> = (t, xs) => {
    xs->Array.flatMap(x => {
      let (src, dests) = runRulesWithInterval(t, x)
      let newSrc = switch src {
      | Some(s) => [s]
      | None => []
      }
      Array.concat(newSrc, dests)
    })
  }
}

module Almanac = {
  type t = {
    seeds: array<Interval.t>,
    maps: array<AlmanacMap.t>,
  }

  let toString: t => string = t => {
    `Almanac (Seeds: [${t.seeds->Array.map(Interval.toString)->Array.joinWith(", ")}],\n[${t.maps
      ->Array.map(AlmanacMap.toString)
      ->Array.joinWith("\n")})]`
  }

  let getMap: (t, string) => AlmanacMap.t = (t, src) => {
    t.maps->Array.find(m => m.srcCategory == src)->Option.getExn
  }
}

let parse: string => Almanac.t = data => {
  let lines = data->splitDoubleNewline->Array.map(l => l->splitNewline->Array.map(String.trim))
  let seedLine = lines[0]->Option.flatMap(Array.get(_, 0))->Option.getExn
  let mapLines = lines->(Array.sliceToEnd(_, ~start=1))

  let parseSeed: string => array<BigInt.t> = line => {
    line->String.split(": ")->Array.get(1)->Option.getExn->splitSpace->Array.map(BigInt.fromString)
  }

  let parseMap: array<string> => AlmanacMap.t = lines => {
    let categoryLine = lines[0]->Option.getExn
    let srcDestLines = lines->Array.sliceToEnd(~start=1)

    let [srcCategory, destCategory] =
      categoryLine
      ->splitSpace
      ->Array.get(0)
      ->Option.flatMap(s => s->String.split(_, "-to-")->Some)
      ->Option.getExn

    let parseIntervalLine: string => Rule.t = l => {
      let [destStart, srcStart, len] = l->splitSpace->Array.map(BigInt.fromString)
      let one = BigInt.fromInt(1)
      open! BigInt
      {
        srcInterval: Interval.make(srcStart, srcStart + len - one),
        //        destInterval: {lower: destStart, upper: destStart + len - one},
        dest: destStart,
        offset: destStart - srcStart,
      }
    }

    let rules = srcDestLines->Array.map(parseIntervalLine)
    {srcCategory, destCategory, rules}
  }

  // make seeds interval with 1 element
  let makeSeedsInterval: array<BigInt.t> => array<Interval.t> = seeds => {
    seeds->Array.map(s => Interval.makeWithLength(s, ~length=BigInt.fromInt(1)))
  }

  let seeds = parseSeed(seedLine)->makeSeedsInterval

  let maps = mapLines->Array.map(parseMap)
  //  {seeds, maps}
  {seeds, maps}
}

let part1 = ({seeds, _} as almanac: Almanac.t) => {
  let startCat = "seed"
  let endCat = "location"

  let rec loop = (endCat, curCat, curNum) => {
    let map = almanac->Almanac.getMap(curCat)
    let {destCategory} = map
    let nextNum = AlmanacMap.runRules(map, curNum)
    destCategory == endCat ? nextNum : loop(endCat, destCategory, nextNum)
  }

  let locations = seeds->Array.map(((lower, _)) => {
    loop(endCat, startCat, lower)
  })

  locations->log
  locations->Utils.minBigIntInArray
}

let part1_Interval = ({seeds, _} as almanac: Almanac.t) => {
  let startCat = "seed"
  let endCat = "location"

  let rec loop: (string, string, array<Interval.t>) => array<Interval.t> = (
    endCat,
    curCat,
    cur,
  ) => {
    let map = almanac->Almanac.getMap(curCat)
    let {destCategory} = map
    let next = AlmanacMap.runRulesWithMultiIntervals(map, cur)
    destCategory == endCat ? next : loop(endCat, destCategory, next)
  }

  let locations = seeds->Array.flatMap(((lower, _)) => {
    loop(endCat, startCat, [Interval.makeWithLength(lower, ~length=BigInt.fromInt(1))])
  })

  locations->(log2("locations =", _))
  locations->Array.map(((lower, _)) => lower)->Utils.minBigIntInArray
}

let solvePart1 = data => {
  let almanac = data->parse
  almanac->Almanac.toString->log
  let m = almanac->Almanac.getMap("seed")

  //  log("runRules")
  //  AlmanacMap.runRules(m, BigInt.fromInt(79))->BigInt.toString->log
  //  AlmanacMap.runRules(m, BigInt.fromInt(14))->BigInt.toString->log
  //  AlmanacMap.runRules(m, BigInt.fromInt(55))->BigInt.toString->log
  //  AlmanacMap.runRules(m, BigInt.fromInt(13))->BigInt.toString->log

  log("runRulesWithInterval")
  AlmanacMap.runRulesWithMultiIntervals(
    m,
    [Interval.makeWithLength(BigInt.fromInt(79), ~length=BigInt.fromInt(1))],
  )->(log2("--> 79", _))
  AlmanacMap.runRulesWithMultiIntervals(
    m,
    [Interval.makeWithLength(BigInt.fromInt(14), ~length=BigInt.fromInt(1))],
  )->(log2("--> 14", _))
  AlmanacMap.runRulesWithMultiIntervals(
    m,
    [Interval.makeWithLength(BigInt.fromInt(55), ~length=BigInt.fromInt(1))],
  )->(log2("--> 55", _))
  AlmanacMap.runRulesWithMultiIntervals(
    m,
    [Interval.makeWithLength(BigInt.fromInt(13), ~length=BigInt.fromInt(1))],
  )->(log2("--> 13", _))
  //
  let p1i = part1_Interval(almanac)
  log("part1_Intervals")
  p1i->log
  //    part1(almanac)
  BigInt.fromInt(1)
}

let solvePart2 = data => {
  data->ignore
  2
}
