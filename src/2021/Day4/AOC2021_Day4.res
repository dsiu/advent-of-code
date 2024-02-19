open Belt
open Utils
let log = Js.Console.log

module Draws = {
  type t = array<int>

  let make = line => line->Js.String2.split(_, ",")->Array.keepMap(Int.fromString)
}

module Board = {
  type t = Array2D.t<int>
  let toString = t => t->Array2D.toString(Stdlib.Int.toString)

  let make = (lines): t => {
    lines->Array.map(x =>
      x
      ->Js.String2.split(" ")
      ->Array.keepMap(s => {
        s->Js.String2.length > 0 ? s->Js.String2.trim->Int.fromString : None
      })
    )
  }

  // determines if all all int in candidates exist in match_draws
  let match = (candidates, match_draws) => {
    let can_set = candidates->Set.Int.fromArray
    let draws_set = match_draws->Set.Int.fromArray

    Set.Int.subset(can_set, draws_set) ? Some(can_set->Set.Int.toArray) : None
  }

  // returns None if not winning, Some(array) of unmatched numbers
  let solve = (t, match_draws) => {
    let rec helper = (t, i, getter, limit) => {
      i < limit
        ? t
          ->getter(i)
          ->Option.flatMap(row => {
            switch row->match(match_draws) {
            | Some(matched) => Some(matched)
            | None => helper(t, i + 1, getter, limit)
            }
          })
        : None
    }

    let matchY = helper(t, 0, Array2D.getYEquals, t->Array2D.lengthY)
    let matchX = helper(t, 0, Array2D.getXEquals, t->Array2D.lengthX)

    let remove_marked = (t, m) => {
      Set.Int.diff(t->Array2D.flatten->Set.Int.fromArray, m->Set.Int.fromArray)->Set.Int.toArray
    }

    switch (matchX, matchY) {
    | (None, None) => None
    | (_, _) => remove_marked(t, match_draws)->Some
    }
  }
}

module Boards = {
  type t = array<Board.t>

  let make = lines => {
    lines->Array.map(b => {
      b->splitNewline(_)->Array.map(Js.String2.trim)->Board.make
    })
  }

  let toString = t => t->Printable.Array.toString(Board.toString)

  let solvePart1 = (t, draws) => {
    let rec helper = (t, ds, i, limit) => {
      i <= limit
        ? {
            let keys = draws->Array.slice(~offset=0, ~len=i)
            let results = t->Array.keepMap(Board.solve(_, keys))
            switch results->Array.length {
            | 0 => helper(t, ds, i + 1, limit)
            | 1 => Some(draws->Array.get(i - 1)->Option.getExn, results[0])
            | _ => raise(Not_found)
            }
          }
        : None
    }

    helper(t, draws, 5, draws->Array.length)
  }

  let solvePart2 = (t, draws) => {
    let rec helper = (t, ds, i, limit) => {
      i <= limit
        ? {
            let keys = draws->Array.slice(~offset=0, ~len=i)
            let results = t->Array.keepMap(Board.solve(_, keys))

            let results_not = t->Array.keepMap(b => {
              switch b->Board.solve(keys) {
              | Some(_) => None
              | None => Some(b)
              }
            })

            switch (results->Array.length, results_not->Array.length) {
            | (1, 0) => Some(draws->Array.get(i - 1)->Option.getExn, results[0])
            | (_, _) => helper(results_not, ds, i + 1, limit)
            }
          }
        : None
    }

    helper(t, draws, 5, draws->Array.length)
  }
}

let parse = data => {
  let lines = data->splitDoubleNewline->Array.map(Js.String2.trim)
  let draws = lines[0]->Option.getExn->Draws.make
  let boards = lines->Array.sliceToEnd(1)->Boards.make
  (draws, boards)
}

let solvePart1 = data => {
  let (draws, boards) = data->parse
  let (last_key, unmatched) = boards->Boards.solvePart1(draws)->Option.getExn
  last_key * unmatched->Option.getExn->Array.reduce(0, add)
}

let solvePart2 = data => {
  let (draws, boards) = data->parse
  let (last_key, unmatched) = boards->Boards.solvePart2(draws)->Option.getExn
  last_key * unmatched->Option.getExn->Array.reduce(0, add)
}
