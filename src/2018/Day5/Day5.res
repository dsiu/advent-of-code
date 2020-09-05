let data = Day5_Data.data
let testData = Day5_Data_Test.data
open Belt
open Utils
let log = Js.Console.log

let charArray = data->Js.String2.split("")
let charList = charArray->List.fromArray

// charList -> List.forEach(Console.log)
// da bA cC aC BA cC ca DA
let groupByN = (l, n) => {
  let (r, cur) = l->List.reduce((list{}, list{}), (a, x) => {
    let (r, cur) = a
    cur->List.length == n - 1
      ? (List.add(r, cur->List.add(x)->List.reverse), list{})
      : (r, cur->List.add(x))
  })
  r->List.reverse
}

let groupBy2 = groupByN(_, 2)
let groupBy3 = groupByN(_, 3)

let charList2 = charList->groupBy2

let fuse = pair => {
  let a = pair->List.get(0)->Option.getExn
  let b = pair->List.get(1)->Option.getExn

  open Js
  a !== b &&
    ((a === b->String2.toLowerCase && b === a->String2.toUpperCase) ||
      (b === a->String2.toLowerCase && a === b->String2.toUpperCase))
    ? true
    : false
}

let rec defuse = l => {
  // r {b c ...rest}
  // l is the
  let (r, cur, defused) = l->List.reduce((list{}, "", false), (a, x) => {
    let (r, cur, defused) = a
//    log(`cur:${cur} x:${x} defused:${defused->string_of_bool}`)
    cur->Js.String2.length == 2 - 1 // 1
      ? {
          let pair = list{cur, x}
          let fused = fuse(pair)
          defused
            ? {
//                log(`  Defused: put cur:${cur} to r x:${x} to cur`)
                (r->List.add(cur), x, defused)
              }
            : fused // fused!!
            ? {
//              log(`  NOT defused and cur+x is fused!: clear cur and x`)
              (r, "", true)
            }
            : {
//                log(`  NOT defused and cur+x doesn't match!: put cur:${cur} to r  x:${x} to cur`)
                (r->List.add(cur), x, defused)
              }
        }
      : {
//          log(`  stash x:${x} to cur`)
          (r, x, defused)  // tail condition...didn't put last x into r
        }
  })
//  log("--> again")
  // check left over cur, tail condition
  let tailed = cur->Js.String2.length == 1 ? r->List.add(cur): r
  !defused ? tailed->List.reverse : tailed->List.reverse->defuse
}


let findPairIndex = l => {
  let (last, found, founded_idx) = l->List.reduceWithIndex(("", false, -1), (a, x, i) => {
  switch a {
    | (last, found, founded_idx) when found => a
    | (last, found, founded_idx) when (! found && last->Js.String2.length === 0) => {
      (x, false, -1)
    }
    | (last, found, founded_idx) when (! found && last->Js.String2.length > 0) => {
        fuse(list{last, x}) ?
          ("", true, i-1)
          : (x, false, -1)
    }
  }
  })
  found? Some(founded_idx) : None
}

//i=3, remove d, e
//[0,1,2,3,4,5,6]
//[a,b,c,d,e,f,g]
//h=[a,b,c] List.take(3)
//t=[f,g] List.drop(5)

let rec defuse_fast = l => {
    switch findPairIndex(l) {
    | Some(i) => {
      let h = l->List.take(i)->Option.getExn
      let t = l->List.drop(i+2)->Option.getExn
      List.concat(h,t)->defuse_fast
    }
    | None => l
  }
}

// fuse(list{"a", "a"})->Utils.list_dump

let solvePart1 = d => 240
let solvePart2 = d => 4455

//let result = charList->defuse_fast->List.toArray->Js.Array2.joinWith(_, "")
//log(result)

//Day5_Data.result -> Js.String2.length -> log

