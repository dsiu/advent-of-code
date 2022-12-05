open Belt
open Utils
let log = Js.Console.log

type shape = Rock | Paper | Scissors
type result = Loss | Draw | Win
type round = Round(shape, shape)

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
  1 +
  switch s {
  | Rock => 1
  | Paper => 2
  | Scissors => 3
  }
}

let scoreResult: result => int = r => {
  3 *
  switch r {
  | Loss => 0
  | Draw => 3
  | Win => 6
  }
}

let scoreRound: round => int = r => {
  let Round(_, p2) = r
  scoreResult(player2Result(r)) + scoreShape(p2)
}

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
