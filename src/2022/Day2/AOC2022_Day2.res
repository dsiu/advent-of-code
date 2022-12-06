open Belt
open Utils
let log = Js.Console.log

type shape = Rock | Paper | Scissors
type result = Loss | Draw | Win
type round = Round(shape, shape)
type shapeResult = ShapeResult(shape, result)

exception InvalidShape(string)

let makeP1Shape = s => {
  switch s {
  | "A" => Rock
  | "B" => Paper
  | "C" => Scissors
  | _ as s => raise(InvalidShape(s))
  }
}

let makeP2Shape = s => {
  switch s {
  | "X" => Rock
  | "Y" => Paper
  | "Z" => Scissors
  | _ as s => raise(InvalidShape(s))
  }
}

let player2Result: round => result = round => {
  switch round {
  | Round(Rock, Paper) => Win
  | Round(Paper, Scissors) => Win
  | Round(Scissors, Rock) => Win
  | Round(Rock, Rock)
  | Round(Paper, Paper)
  | Round(Scissors, Scissors) =>
    Draw
  | _ => Loss
  }
}

let scoreShape: shape => int = s => {
  switch s {
  | Rock => 1
  | Paper => 2
  | Scissors => 3
  }
}

let scoreResult: result => int = r => {
  switch r {
  | Loss => 0
  | Draw => 3
  | Win => 6
  }
}

exception InvalidResult(string)

let makeResult = s => {
  switch s {
  | "X" => Loss
  | "Y" => Draw
  | "Z" => Win
  | _ as s => raise(InvalidResult(s))
  }
}

let scoreRound: round => int = r => {
  let Round(_, p2) = r
  scoreResult(player2Result(r)) + scoreShape(p2)
}

let roundFromResult: shapeResult => round = (ShapeResult(shape, result)) => {
  let p2s =
    [Rock, Paper, Scissors]
    ->Array.keep(p2Shape => player2Result(Round(shape, p2Shape)) == result)
    ->Array.getExn(0)

  Round(shape, p2s)
}

@@warning("-8")
let makeRound = s => {
  open Js.String2

  let [p1, p2] = s->trim->split(_, " ")
  Round(makeP1Shape(p1), makeP2Shape(p2))
}

let makeShapeResult = s => {
  open Js.String2

  let [p1, r] = s->trim->split(_, " ")
  ShapeResult(makeP1Shape(p1), makeResult(r))
}

let parse = (data, f) => {
  open Array
  data->splitNewline->map(f)
}

let part1: array<round> => int = rounds => {
  rounds->Array.map(scoreRound)->sumIntArray
}

let part2: array<shapeResult> => int = rounds => {
  open FP_Utils
  rounds->Array.map(compose(roundFromResult, scoreRound))->sumIntArray
}

let solvePart1 = data => {
  data->parse(makeRound)->part1
}

let solvePart2 = data => {
  data->parse(makeShapeResult)->part2
}
