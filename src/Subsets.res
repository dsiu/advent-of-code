// power set
//let rec subsets = l => {
//  switch l {
//  | list{} => list{}
//  | list{x, ...xs} =>
//    switch x {
//    | list{} => list{}
//    | _ => subsets(xs)->List.map(ss => {list{ss, list{x, ...ss}}->List.flatten})
//    }
//  }
//}
//
//let a = list{list{1, 2, 3}}
//a->subsets->Belt.List.toArray->Js.log

open Belt

let rec powerset = set => {
  switch set {
  | list{} => list{list{}}
  | list{x, ...xs} => {
      let rest = xs->powerset
      let rest_map = Belt.List.map(rest, it => list{x, ...it})
      Belt.List.concat(rest, rest_map)
    }
  }
}

let powerset_array_with_list = xs => {
  xs->Belt.List.fromArray->powerset->Belt.List.map(Belt.List.toArray)->Belt.List.toArray
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

//let a = [1, 2, 3]
//a->powerset_array->Js.log

let b = ["a", "b", "c"]
b->powerset_array->Js.log
