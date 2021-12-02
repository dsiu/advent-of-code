/**
  powerset

  refs:
    https://blog.shaynefletcher.org/2014/06/powerset.html
    https://stackoverflow.com/questions/40141955/computing-a-set-of-all-subsets-power-set
    https://www.freecodecamp.org/news/my-favorite-examples-of-functional-programming-in-kotlin-e69217b39112/

*/

module List = Belt.List

let flatMapList: (List.t<'a>, 'a => List.t<'b>) => List.t<'b> = (xs, f) => {
  List.reduce(List.map(xs, f), list{}, List.concat)
}

// powerset with map (easier to understand)
// ref: https://gist.github.com/JadenGeller/6174b3461a34465791c5
let rec powerset_list_map = set => {
  switch set {
  | list{} => list{list{}}
  | list{x, ...xs} => {
      let tail_powersets = xs->powerset_list_map
      let with_x = Belt.List.map(tail_powersets, it => list{x, ...it})
      Belt.List.concat(tail_powersets, with_x)
    }
  }
}

let rec powerset_list_flatMap = set => {
  switch set {
  | list{} => list{list{}}
  | list{x, ...xs} => {
      let tail_powersets = xs->powerset_list_flatMap
      tail_powersets->flatMapList(it => list{List.concat(list{x}, it)})
    }
  }
}

let powerset_list = powerset_list_flatMap

let powerset_array_with_list = xs => {
  xs->Belt.List.fromArray->powerset_list_map->Belt.List.map(Belt.List.toArray)->Belt.List.toArray
}

module Array = Belt.Array

let flatMapArray: (array<'a>, 'a => array<'b>) => array<'b> = (xs, f) => {
  Array.reduce(Array.map(xs, f), [], Array.concat)
}

// powerset with map (easier to understand)
let rec powerset_array_map = set => {
  switch set->Array.size {
  | 0 => [[]]
  | _ => {
      let x = set->Array.getExn(0)
      let xs = set->Array.sliceToEnd(1)
      let tail_powersets = xs->powerset_array_map
      let with_x = tail_powersets->Array.map(it => Array.concat([x], it))
      Array.concat(tail_powersets, with_x)
    }
  }
}

// powerset with flatmap
// ref: https://gist.github.com/JadenGeller/6174b3461a34465791c5
let rec powerset_array_flatMap = set => {
  switch set->Array.size {
  | 0 => [[]]
  | _ => {
      let x = set->Array.getExn(0)
      let xs = set->Array.sliceToEnd(1)
      let tail_powersets = xs->powerset_array_flatMap
      tail_powersets->flatMapArray(it => [Array.concat([x], it)])
    }
  }
}

let powerset_array = powerset_array_flatMap
