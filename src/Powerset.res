/**
  powerset

  refs:
    https://blog.shaynefletcher.org/2014/06/powerset.html
    https://stackoverflow.com/questions/40141955/computing-a-set-of-all-subsets-power-set
    https://www.freecodecamp.org/news/my-favorite-examples-of-functional-programming-in-kotlin-e69217b39112/
    https://medium.com/flawless-app-stories/foldable-map-and-flatmap-416dbd5ace36
    https://gist.github.com/JadenGeller/6174b3461a34465791c5

*/
module List = Stdlib.List

//
// powerset with map (easier to understand)
//
let rec powersetListMap_ = set => {
  switch set {
  | list{} => list{list{}}
  | list{x, ...xs} => {
      let tail_powersets = xs->powersetListMap_
      let with_x = List.map(tail_powersets, it => list{x, ...it})
      List.concat(tail_powersets, with_x)
    }
  }
}

let {flatMapList} = module(List)

let rec powersetListFlatMap_ = set => {
  switch set {
  | list{} => list{list{}}
  | list{x, ...xs} => {
      let tail_powersets = xs->powersetListFlatMap_
      tail_powersets->flatMapList(it => list{it, List.concat(list{x}, it)})
    }
  }
}

let powersetList = powersetListFlatMap_

let powersetArrayWithList_ = xs => {
  xs->List.fromArray->powersetListMap_->List.map(List.toArray)->List.toArray
}

//
// powerset with map (easier to understand)
//
module Array = Stdlib.Array

let rec powersetArrayMap_ = set => {
  switch set->Array.size {
  | 0 => [[]]
  | _ => {
      let x = set->Array.getExn(0)
      let xs = set->Array.sliceToEnd(1)
      let tail_powersets = xs->powersetArrayMap_
      let with_x = tail_powersets->Array.map(it => Array.concat([x], it))
      Array.concat(tail_powersets, with_x)
    }
  }
}

//
// powerset with flatmap
//
let {flatMapArray} = module(Array)

let rec powersetArrayFlatMap_ = set => {
  switch set->Array.size {
  | 0 => [[]]
  | _ => {
      let x = set->Array.getExn(0)
      let xs = set->Array.sliceToEnd(1)
      let tail_powersets = xs->powersetArrayFlatMap_
      tail_powersets->flatMapArray(it => [it, Array.concat([x], it)])
    }
  }
}

let powersetArray = powersetArrayFlatMap_

//let a = [1, 2, 3]
//"array ====="->Js.log
//a->powerset_array_map->Js.log
//a->powerset_array_flatMap->Js.log
//
//let b = list{1, 2, 3}
//"list ====="->Js.log
//b->powerset_list_map->List.map(x => x->List.toArray)->List.toArray->Js.log
//b->powerset_list_flatMap->List.map(x => x->List.toArray)->List.toArray->Js.log

//let a = [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]
//a->Js.log
//a->Array.map(Belt.SortArray.stableSortBy(_, (a, b) => b - a))->Belt.SortArray.stableSortBy((a,b) => {
//  let a.size
//})->Js.log
