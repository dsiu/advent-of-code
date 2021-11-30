/**
  powerset

  refs:
    https://blog.shaynefletcher.org/2014/06/powerset.html
    https://stackoverflow.com/questions/40141955/computing-a-set-of-all-subsets-power-set
    https://www.freecodecamp.org/news/my-favorite-examples-of-functional-programming-in-kotlin-e69217b39112/

*/

open Belt

let rec powerset_list = set => {
  switch set {
  | list{} => list{list{}}
  | list{x, ...xs} => {
      let rest = xs->powerset_list
      let rest_map = Belt.List.map(rest, it => list{x, ...it})
      Belt.List.concat(rest, rest_map)
    }
  }
}

let powerset_array_with_list = xs => {
  xs->Belt.List.fromArray->powerset_list->Belt.List.map(Belt.List.toArray)->Belt.List.toArray
}

let rec powerset_array = set => {
  switch set->Array.size {
  | 0 => [[]]
  | _ => {
      let x = set->Array.getExn(0)
      let xs = set->Array.sliceToEnd(1)
      let rest = xs->powerset_array
      let rest_map = rest->Array.map(it => Array.concat([x], it))
      Array.concat(rest, rest_map)
    }
  }
}
