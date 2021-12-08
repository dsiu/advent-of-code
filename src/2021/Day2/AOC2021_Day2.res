open Belt
open Utils
let log = Js.Console.log

module Move = {
  type t = Forward(int) | Down(int) | Up(int)

  exception InvalidMove(string)

  let make = (s, n) => {
    switch s {
    | "forward" => Forward(n)
    | "down" => Down(n)
    | "up" => Up(n)
    | _ => InvalidMove(s)->raise
    }
  }
}

module Submarine = {
  type t = {
    h: int,
    d: int,
    aim: int,
  }

  let make = {h: 0, d: 0, aim: 0}

  let move = (t, move: Move.t) => {
    switch move {
    | Forward(n) => {...t, h: t.h + n}
    | Down(n) => {...t, d: t.d + n}
    | Up(n) => {...t, d: t.d - n}
    }
  }
  let moveWithAim = (t, move: Move.t) => {
    switch move {
    | Forward(n) => {...t, h: t.h + n, d: t.d + t.aim * n}
    | Down(n) => {...t, aim: t.aim + n}
    | Up(n) => {...t, aim: t.aim - n}
    }
  }
}

let run = (xs, f) => {
  xs->Array.reduce(Submarine.make, (a, x) => a->f(x))
}

let runPart1 = run(_, Submarine.move)
let runPart2 = run(_, Submarine.moveWithAim)

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    let s = x->Js.String2.trim->Js.String2.split(" ")
    Move.make(
      s->Array.get(0)->Option.getExn,
      s->Array.get(1)->Option.getExn->Int.fromString->Option.getExn,
    )
  })

let answer = (t: Submarine.t) => {t.h * t.d}

let solvePart1 = data => {
  data->parse->runPart1->answer
}

let solvePart2 = data => {
  data->parse->runPart2->answer
}
