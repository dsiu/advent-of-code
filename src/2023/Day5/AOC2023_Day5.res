@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

module Rule = {
  type t = {
    srcInterval: Interval.t,
    dest: BigInt.t,
    offset: BigInt.t,
  }

  let toString: t => string = t => {
    `Rule(Src:${t.srcInterval->Interval.toString}, Dest:${t.dest->BigInt.toString}, Offset:${t.offset->BigInt.toString})`
  }

  /**
   * This function runs a rule with a single input.
   * It checks if the input number is contained within the source interval of the rule.
   * If it is, it adds the offset of the rule to the input number and returns the result wrapped in a `Some`.
   * If the input number is not contained within the source interval, it returns `None`.
   *
   * @param {t} t - The rule to run. It is an object containing a source interval, a destination, and an offset.
   * @param {BigInt.t} srcNum - The input number to run the rule with.
   * @returns {option<BigInt.t>} - The result of running the rule with the input number. It is `Some` of the result if the input number is contained within the source interval of the rule, otherwise it is `None`.
  */
  let run: (t, BigInt.t) => option<BigInt.t> = (t, srcNum) => {
    t.srcInterval->Interval.contains(srcNum) ? Some(BigInt.add(srcNum, t.offset)) : None
  }

  /**
   * This function runs a rule with an interval as input.
   * It first calculates the intersection of the input interval and the source interval of the rule.
   * If there is an intersection, it removes the intersection from the input interval and adds the offset of the rule to the intersection.
   * If there is no intersection, it simply returns the input interval.
   * The function returns a tuple of two options. The first option is the new source interval after removing the intersection, and the second option is the new destination interval after adding the offset to the intersection.
   *
   * @param {t} t - The rule to run. It is an object containing a source interval, a destination, and an offset.
   * @param {Interval.t} src - The input interval to run the rule with.
   * @returns {(option<Interval.t>, option<Interval.t>)} - The result of running the rule with the input interval. The first option is the new source interval after removing the intersection, and the second option is the new destination interval after adding the offset to the intersection.
   */
  let runWithInterval: (t, Interval.t) => (option<Interval.t>, option<Interval.t>) = (t, src) => {
    let intersection = src->Interval.intersect(t.srcInterval)
    let newSrc =
      intersection
      ->Option.map(i => Interval.remove(src, i))
      ->Option.getOr(Some(src))
    let newDest = intersection->Option.map(i => Interval.add(i, t.offset))
    (newSrc, newDest)
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

  /**
   * This function runs a set of rules with a single input.
   * It iterates over the array of rules and for each rule, it runs the `Rule.run` function with the input number.
   * If a rule returns a `Some` value, it stops iterating and returns that value.
   * If no rule returns a `Some` value, it returns the original input number.
   *
   * @param {t} t - The AlmanacMap to run the rules from. It is an object containing a source category, a destination category, and an array of rules.
   * @param {BigInt.t} srcNum - The input number to run the rules with.
   * @returns {BigInt.t} - The result of running the rules with the input number. It is the first `Some` value returned by a rule, or the original input number if no rule returns a `Some` value.
   */
  let runRules: (t, BigInt.t) => BigInt.t = (t, srcNum) => {
    t.rules
    ->Array.findMap(Rule.run(_, srcNum))
    ->Option.getOr(srcNum)
  }

  /**
   * This function runs a set of rules with an interval as input.
   * It iterates over the array of rules and for each rule, it runs the `Rule.runWithInterval` function with the input interval.
   * If a rule returns a new source interval, it updates the input interval for the next rule and adds the destination interval to the destination array.
   * If a rule does not return a new source interval, it keeps the input interval for the next rule and does not add anything to the destination array.
   * The function returns a tuple of two values. The first value is the final source interval after running all the rules, and the second value is the array of all destination intervals returned by the rules.
   *
   * @param {t} t - The AlmanacMap to run the rules from. It is an object containing a source category, a destination category, and an array of rules.
   * @param {Interval.t} src - The input interval to run the rules with.
   * @returns {(option<Interval.t>, array<Interval.t>)} - The result of running the rules with the input interval. The first value is the final source interval after running all the rules, and the second value is the array of all destination intervals returned by the rules.
   */
  let runRulesWithInterval: (t, Interval.t) => (option<Interval.t>, array<Interval.t>) = (
    t,
    src,
  ) => {
    t.rules->Array.reduce((Some(src), []), ((s, d), r) => {
      switch s {
      // run rule only when there is any src left to process
      | Some(s) => {
          let (newSrc, dest) = Rule.runWithInterval(r, s)

          // if some dest is found, add it to the dest array
          let newDest = switch dest {
          | Some(mapped) => Array.concat([mapped], d)
          | None => d
          }

          (newSrc, newDest)
        }
      | None => (None, d)
      }
    })
  }

  /**
   * This function runs a set of rules with multiple intervals as input.
   * It iterates over the array of input intervals and for each interval, it runs the `runRulesWithInterval` function.
   * If a new source interval is returned, it is added to the new source array.
   * The function then concatenates the new source array with the destination array and returns the result.
   *
   * @param {t} t - The AlmanacMap to run the rules from. It is an object containing a source category, a destination category, and an array of rules.
   * @param {array<Interval.t>} xs - The array of input intervals to run the rules with.
   * @returns {array<Interval.t>} - The result of running the rules with the input intervals. It is an array of all source and destination intervals returned by the rules.
   */
  let runRulesWithMultiIntervals: (t, array<Interval.t>) => array<Interval.t> = (t, xs) => {
    xs->Array.flatMap(x => {
      let (src, dest) = runRulesWithInterval(t, x)
      let newSrc = src->Option.flatMap(s => {Some([s])})->Option.getOr([])
      Array.concat(newSrc, dest)
    })
  }
}

module Almanac = {
  type t = {
    seeds: array<BigInt.t>,
    maps: array<AlmanacMap.t>,
  }

  let toString: t => string = t => {
    `Almanac (Seeds: [${t.seeds->Array.map(BigInt.toString)->Array.joinWith(", ")}],\n[${t.maps
      ->Array.map(AlmanacMap.toString)
      ->Array.joinWith("\n")})]`
  }

  let getMap: (t, string) => AlmanacMap.t = (t, src) => {
    t.maps->Array.find(m => m.srcCategory == src)->Option.getExn
  }
}

/**
 * This function parses a string into an Almanac object.
 * It first splits the input data into lines and trims each line.
 * Then it parses the seed line and the map lines separately.
 * The seed line is split by ": " and the second part is split by spaces and each part is converted to a BigInt.
 * Each map line is split by spaces and the first part is split by "-to-" to get the source and destination categories.
 * The rest of the map line is parsed into a Rule object.
 * Finally, it returns an Almanac object containing the parsed seeds and maps.
 *
 * @param {string} data - The input data to parse. It is a string containing the seed line and the map lines.
 * @returns {Almanac.t} - The parsed Almanac object. It contains an array of seeds and an array of maps.
 *
 * @example
 * let data = "seed: 1 2 3\n\nseed-to-location\n1 1 1\n2 2 2\n\nlocation-to-destination\n3 3 3\n4 4 4"
 * let almanac = parse(data)
 * // almanac is now an Almanac object with seeds [1, 2, 3] and two maps from "seed" to "location" and from "location" to "destination"
 */
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
        dest: destStart,
        offset: destStart - srcStart,
      }
    }

    let rules = srcDestLines->Array.map(parseIntervalLine)
    {srcCategory, destCategory, rules}
  }

  let seeds = parseSeed(seedLine)
  let maps = mapLines->Array.map(parseMap)
  {seeds, maps}
}

let part1_simple = ({seeds, _} as almanac: Almanac.t) => {
  let startCat = "seed"
  let endCat = "location"

  let rec loop = (endCat, curCat, curNum) => {
    let map = almanac->Almanac.getMap(curCat)
    let {destCategory} = map
    let nextNum = AlmanacMap.runRules(map, curNum)
    destCategory == endCat ? nextNum : loop(endCat, destCategory, nextNum)
  }

  let locations = seeds->Array.map(s => {
    loop(endCat, startCat, s)
  })

  locations->log
  locations->Utils.minBigIntInArray
}

/**
 * This function finds the location from the seeds in the almanac.
 * It first transforms the seeds using the provided seedTransform function.
 * Then it iterates over the transformed seeds and for each seed, it runs a loop function.
 * The loop function recursively runs the rules with multiple intervals from the current category to the next category until it reaches the end category.
 * Finally, it returns the minimum lower bound of all the locations found.
 *
 * @param {Almanac.t} almanac - The almanac to find the location from. It is an object containing an array of seeds and an array of maps.
 * @param {array<BigInt.t> => array<Interval.t>} seedTransform - The function to transform the seeds. It takes an array of seeds and returns an array of intervals.
 * @returns {BigInt.t} - The minimum lower bound of all the locations found.
 *
 * @example
 * let almanac = parse("seed: 1 2 3\n\nseed-to-location\n1 1 1\n2 2 2\n\nlocation-to-destination\n3 3 3\n4 4 4")
 * let location = findLocation(almanac, makeSeedsInterval)
 * // location is now the minimum lower bound of all the locations found from the seeds in the almanac
 */
let findLocation = ({seeds, _} as almanac: Almanac.t, seedTransform) => {
  let startCat = "seed"
  let endCat = "location"

  let rec loop = (endCat, curCat, cur) => {
    let map = almanac->Almanac.getMap(curCat)
    let {destCategory} = map
    let next = AlmanacMap.runRulesWithMultiIntervals(map, cur)
    destCategory == endCat ? next : loop(endCat, destCategory, next)
  }

  let newSeeds = seeds->seedTransform
  let locations = newSeeds->Array.flatMap(s => loop(endCat, startCat, [s]))
  locations->Array.map(((lower, _)) => lower)->Utils.minBigIntInArray
}

/**
 * This function transforms an array of seeds into an array of intervals.
 * Each seed is transformed into an interval of length 1 starting at the seed.
 *
 * @param {array<BigInt.t>} seeds - The array of seeds to transform. Each seed is a BigInt.
 * @returns {array<Interval.t>} - The array of intervals. Each interval is of length 1 and starts at the corresponding seed.
 *
 * @example
 * let seeds = [BigInt.fromInt(1), BigInt.fromInt(2), BigInt.fromInt(3)]
 * let intervals = makeSeedsInterval(seeds)
 * // intervals is now an array of intervals [Interval(1, 1), Interval(2, 2), Interval(3, 3)]
 */
let makeSeedsInterval: array<BigInt.t> => array<Interval.t> = seeds => {
  seeds->Array.map(Interval.makeWithLength(_, ~length=BigInt.fromInt(1)))
}

/**
 * This function transforms an array of seeds into an array of intervals.
 * It iterates over the array of seeds and for each pair of seeds, it creates an interval.
 * The interval starts at the first seed and its length is the second seed.
 * If the index of the seed is odd, it skips the seed.
 * If there is no next seed for the last seed, it also skips the last seed.
 *
 * @param {array<BigInt.t>} seeds - The array of seeds to transform. Each seed is a BigInt.
 * @returns {array<Interval.t>} - The array of intervals. Each interval starts at the first seed of a pair and its length is the second seed of the pair.
 *
 * @example
 * let seeds = [BigInt.fromInt(1), BigInt.fromInt(2), BigInt.fromInt(3), BigInt.fromInt(4)]
 * let intervals = makeSeedsPair(seeds)
 * // intervals is now an array of intervals [Interval(1, 2), Interval(3, 4)]
 */
let makeSeedsPair: array<BigInt.t> => array<Interval.t> = seeds => {
  seeds
  ->Array.mapWithIndex((a, i) => {
    mod(i, 2) == 0
      ? {
          let b = seeds->Array.get(i + 1)->Option.getExn
          Some(Interval.makeWithLength(a, ~length=b))
        }
      : None
  })
  ->Array.filter(Option.isSome)
  ->Array.map(Option.getExn)
}

let part1: Almanac.t => BigInt.t = findLocation(_, makeSeedsInterval)
let part2: Almanac.t => BigInt.t = findLocation(_, makeSeedsPair)

let solvePart1 = data => {
  let almanac = data->parse
  part1(almanac)
}

let solvePart2 = data => {
  let almanac = data->parse
  part2(almanac)
}
