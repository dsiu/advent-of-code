let data = Day2_Data.data

let string_to_charStr = Js.String.split("")

type charStrArray = array<string>
type charStrFreqMap = Belt.Map.String.t<int>

let reducer = (r: charStrFreqMap, x) => {
  let count = Belt.Map.String.get(r, x)
  switch count {
  | None => Belt.Map.String.set(r, x, 1)
  | Some(y) => Belt.Map.String.set(r, x, y + 1)
  }
}

let char_freq = (cs: charStrArray): charStrFreqMap => {
  Belt.Array.reduce(cs, Belt.Map.String.empty, reducer)
}

@@warning("-27")
let countMatchFreq = (freq: int, m: charStrFreqMap): int => {
  Belt.Map.String.reduce(m, 0, (acc, key, v) => {
    v == freq ? acc + 1 : acc
  })
}

let n_char_matched_freq = (freq: int, s: string): int => {
  s |> string_to_charStr |> char_freq |> countMatchFreq(freq)
}

let twoTimesCounter = n_char_matched_freq(2)
let threeTimesCounter = n_char_matched_freq(3)

let nonZero = x => {
  switch x {
  | 0 => 0
  | _ => 1
  }
}

// let twoTimesUniqCounter = twoTimesCounter |> nonZero
// let threeTimesUniqCounter = threeTimesCounter |> nonZero

type resultRec = {
  twoTimes: int,
  threeTimes: int,
}

let runDay2Part1 = lines => {
  let result = Belt.Array.reduce(lines, {twoTimes: 0, threeTimes: 0}, (acc, l) => {
    twoTimes: acc.twoTimes + (l |> twoTimesCounter |> nonZero),
    threeTimes: acc.threeTimes + (l |> threeTimesCounter |> nonZero),
  })
  // result
  result.twoTimes * result.threeTimes
}

let test_string = "aabbbccccccddddd"
//"------ Day2 Part1 Starts" |> Js.Console.log
//data |> Js.String.split("\n") |> runDay2Part1 |> Js.Console.log
//[test_string, test_string] |> runDay2Part1 |> Js.Console.log

//
// Part2
//
type diffType =
  | Match(string)
  | NotMatch(string, string)

type diffs = array<diffType>

let diffOfTwoCharStr = (s1, s2): diffs => {
  let s1CharStr = s1 |> string_to_charStr
  let s2CharStr = s2 |> string_to_charStr

  Belt.Array.mapWithIndex(s1CharStr, (i, x) => {
    let y = Belt.Array.get(s2CharStr, i)
    switch y {
    | Some(y) => x == y ? Match(x) : NotMatch(x, y)
    | None => NotMatch(x, x)
    }
  })
}

let countTrue = (xs: diffs): int => {
  Belt.Array.keep(xs, x =>
    switch x {
    | Match(_) => true
    | _ => false
    }
  ) |> Belt.Array.length
}

let countFalse = (xs: diffs): int => {
  Belt.Array.keep(xs, x =>
    switch x {
    | NotMatch(_, _) => true
    | _ => false
    }
  ) |> Belt.Array.length
}

let isDiffBy = (n, xs: diffs) => {
  countFalse(xs) == n
}

let isDiffBy1 = isDiffBy(1)
let isDiffBy5 = isDiffBy(5)

type matchRecord = {
  src: string,
  matched: array<string>,
}

let findMatch = (lines, predicate, x) => {
  Belt.Array.reduce(lines, ({src: x, matched: []}: matchRecord), (a: matchRecord, y) => {
    ...a,
    matched: switch diffOfTwoCharStr(a.src, y) |> predicate {
    | true => Belt.Array.concat(a.matched, [y])
    | _ => a.matched
    },
  })
}

let findAllMatch = (predicate, lines) => {
  Belt.Array.reduce(lines, [], (a, x) => {
    let res = findMatch(lines, predicate, x)
    switch Array.length(res.matched) {
    | 0 => a
    | _ => Belt.Array.concat(a, [res])
    }
  })
}

let runDay2Part2 = lines => {
  findAllMatch(isDiffBy1, lines)->Belt.Array.map(x => {
    diffOfTwoCharStr(x.src, x.matched[0])->Belt.Array.reduce("", (a, x) =>
      switch x {
      | Match(x) => a ++ x
      | NotMatch(_, _) => a
      }
    )
  })
}

//"------ Day2 Part2 Starts" |> Js.Console.log
//data |> Js.String.split("\n") |> runDay2Part2 |> Js.Console.log
