open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type beforePages = Set.t<int>
type afterPages = Set.t<int>
type rules = Map.t<int, (beforePages, afterPages)>
type update = array<int>
type page = int

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
  update->Array.reduceWithIndex(true, (isValid, cur, i) => {
    !isValid
      ? false
      : {
          let before = update->Array.slice(~start=0, ~end=i)
          let after = update->Array.sliceToEnd(~start=i + 1)

          rules
          ->Map.get(cur)
          ->Option.getUnsafe
          ->(
            ((beforeSet, afterSet)) => {
              before->Set.fromArray->Set.isSubsetOf(beforeSet) &&
                after->Set.fromArray->Set.isSubsetOf(afterSet)
            }
          )
        }
  })
}

let printable: (rules, Set.t<page>, page) => bool = (rules, unprinted, page) => {
  let (_beforeSet, afterSet) = rules->Map.get(page)->Option.getUnsafe
  unprinted->Set.isSubsetOf(afterSet)
}

let printCandidate: (rules, Set.t<page>) => Set.t<page> = (rules, unprinted) => {
  unprinted
  ->Set.toArray
  ->Array.filter(x => {
    let unprinted' = unprinted->Set.toArray->Array.filter(i => !(i == x))->Set.fromArray
    printable(rules, unprinted', x)
  })
  ->Set.fromArray
}

let rec reorder: (rules, array<page>, Set.t<page>) => array<page> = (rules, printed, unprinted) => {
  switch unprinted->Set.size {
  | 0 => printed
  | _ => {
      let candidate = printCandidate(rules, unprinted)
      let next = candidate->Set.toArray->minIntInArray
      unprinted->Set.delete(next)->ignore
      let rest = unprinted
      let printed' = printed->Array.concat([next])
      reorder(rules, printed', rest)
    }
  }
}

let middlePage: array<page> => page = pages => {
  let len = pages->Array.length
  let middle = len / 2
  pages->Array.getUnsafe(middle)
}

@warning("-8")
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

  updates
  ->Array.filter(isOrderValid(_, rules))
  ->Array.map(middlePage)
  ->sumIntArray
}

let solvePart2 = data => {
  let (rulesData, updates) = data->parse
  let rules = rulesData->makeRules

  updates
  ->Array.filter(x => !isOrderValid(x, rules))
  ->Array.map(p => reorder(rules, [], Set.fromArray(p)))
  ->Array.map(middlePage)
  ->sumIntArray
}
