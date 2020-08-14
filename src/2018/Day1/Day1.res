open Belt

let sum = (x, y) => x + y
let part1 = Array.reduce(Day1_Data.data, 0, sum)
Js.Console.log(part1)

type record = {
  sum: int,
  countMap: Map.Int.t<int>,
  found: option<int>
}


let init = {
  sum: 0,
  countMap: Map.Int.empty,
  found: None
}

let reducer = (r:record, x) => {
  let countMap = r.countMap;
  let new_sum = r.sum + x;
  let countVal = switch Map.Int.get(countMap, new_sum) {
                             | None => 1
                             | Some(y) => y + 1
                         };
  let foundVal = switch (r.found, countVal) {
      | (None, 2) => Some(new_sum)
      | (None, _) => None
      | (Some(y), _) => Some(y)
    };

  {
    sum: new_sum,
    countMap: Map.Int.set(countMap, new_sum, countVal),
    found: foundVal
  };
}


let rec solve_part2 = (xs, init, reducer) => {
  let result = Array.reduce(xs, init, reducer);
  switch result.found {
    | None => solve_part2(xs, result, reducer)
    | _ => result
  }
}

let part2_result = solve_part2(Day1_Data.data, init, reducer)

Js.Console.log(switch part2_result.found {
| Some(y) => string_of_int(y)
| None => "diu"
})
