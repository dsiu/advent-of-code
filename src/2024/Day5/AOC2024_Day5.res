open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type beforePages = Set.t<int>
type afterPages = Set.t<int>
type rules = Map.t<int, (beforePages, afterPages)>
type update = array<int>

let ruleMapUpdate = (m, key, f) => {
  let value = m->Map.get(key)
  switch value {
  | Some(v) => m->Map.set(key, f(v))
  | None => m->Map.set(key, f((Set.make(), Set.make())))
  }
}

let makeRules: array<(int, int)> => rules = rulesData => {
  rulesData->Array.reduce(Map.make(), (acc, (before, after)) => {
    acc->ruleMapUpdate(before, ((u, v)) => {
      v->Set.add(after)
      (u, v)
    })
    acc->ruleMapUpdate(after, ((u, v)) => {
      u->Set.add(before)
      (u, v)
    })
    acc
  })
}

let isOrderValid = (update: array<int>, rules) => {
  //  log("============= isOrderValid")
  //  update->(log2("update: ", _))
  let (result, _, _, _) = update->Array.reduceWithIndex(([], [], [], update), (acc, x, i) => {
    let (result, u, v, update) = acc
    //    acc->(log2("acc:", _))

    let cur = update->Array.getUnsafe(i)
    let v' = update->Array.sliceToEnd(~start=i + 1)

    let result' =
      rules
      ->Map.get(cur)
      ->Option.getUnsafe
      ->(
        ((before, after)) => {
          let left = u->Set.fromArray->Set.isSubsetOf(before)
          let right = v'->Set.fromArray->Set.isSubsetOf(after)
          //          cur->(log2("cur: ", _))
          //          u->(log2("checking u: ", _))
          //          v'->(log2("checking v: ", _))
          left && right
        }
      )

    u->Array.push(cur)
    ([...result, result'], u, v', update)
  })
  //  result->(log2("result: ", _))
  result->Array.every(x => x)
}

let parse = data => {
  let [rules, updates] = data->splitDoubleNewline->Array.map(splitNewline)
  (
    rules->Array.map(l =>
      l
      ->String.trim
      ->String.split("|")
      ->Array.map(x => x->Int.fromString->Option.getUnsafe)
      ->Tuple2.fromArray
      ->Option.getUnsafe
    ),
    updates->Array.map(l =>
      l->String.trim->String.split(",")->Array.map(x => x->Int.fromString->Option.getUnsafe)
    ),
  )
}

let solvePart1 = data => {
  let (rulesData, updates) = data->parse
  let rules = rulesData->makeRules
  //  rules->log
  updates
  ->Array.filter(x => x->isOrderValid(rules))
  ->Array.map(x => x->Array.getUnsafe(Array.length(x) / 2))
  ->sumIntArray
}

let solvePart2 = data => {
  data->ignore
  2
}
