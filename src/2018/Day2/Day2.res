let data = Day2_Data.data

let string_to_charStr = Js.String.split("")

type charStrArray = array<string>
type charStrFreqMap = Belt.Map.String.t<int>

let reducer = (r:charStrFreqMap, x) => {
  let count = Belt.Map.String.get(r, x)
  switch count {
    | None => Belt.Map.String.set(r, x, 1)
    | Some(y) => Belt.Map.String.set(r, x, y + 1)
  }
}

let char_freq = (cs:charStrArray) : charStrFreqMap => {
  Belt.Array.reduce(cs, Belt.Map.String.empty, reducer)
}

let countMatchFreq = (freq: int, m:charStrFreqMap) : int => {
  Belt.Map.String.reduce(m, 0, (acc, key, v) => {
    v == freq ? acc + 1 : acc
  })
}

let n_char_matched_freq = (freq:int, s:string)  : int => {
  s |> string_to_charStr
    |> char_freq
    |> countMatchFreq(freq)
}


let twoTimesCounter = n_char_matched_freq(2)
let threeTimesCounter = n_char_matched_freq(3)

let nonZero = x => {
  switch x {
    | 0 => 0
    | _ => 1
 }
}

//let twoTimesUniqCounter = twoTimesCounter |> nonZero
//let threeTimesUniqCounter = threeTimesCounter |> nonZero

type resultRec = {
 twoTimes: int
 threeTimes: int
}

let runDay2Part1 = lines => {
  let result = Belt.Array.reduce(lines, { twoTimes:0, threeTimes:0 }, (acc, l) => {
        twoTimes: acc.twoTimes + (l |> twoTimesCounter |> nonZero),
        threeTimes: acc.threeTimes +  (l |> threeTimesCounter |> nonZero)
  })
  // result
  result.twoTimes * result.threeTimes
}

let test_string = "aabbbccccccddddd"
test_string |> string_to_charStr |> char_freq |> Utils.map_string_dump
test_string |> n_char_matched_freq(3) |> Js.Console.log
test_string |> threeTimesCounter |> Js.Console.log

data |> Js.String.split("\n")|> runDay2Part1 |> Js.Console.log
[test_string, test_string] |> runDay2Part1 |> Js.Console.log
