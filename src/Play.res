let log = Js.Console.log

type nonEmpty<'a> = {head: 'a, more: list<'a>}
let x = {head: 1, more: list{2, 3, 4, 5}}

let head = x => x.head

let tail = l => {
  switch l {
  | {more: list{head, ...more}, _} => Some({head, more})
  | {more: list{}, _} => None
  }
}

//x->tail->log

let c = x => x

//type nonEmpty_1<'a> = First('a) | c('a)

open Stdlib
List.foldLeft0(list{10, 7, 5, 1}, (a, b) => a - b)->log
List.foldRight0(list{10, 7, 5, 1}, (a, b) => a - b)->log

List.foldLeft0(list{"a", "b", "c", "d"}, (a, b) => a ++ b)->log
List.foldRight0(list{"a", "b", "c", "d"}, (a, b) => a ++ b)->log
