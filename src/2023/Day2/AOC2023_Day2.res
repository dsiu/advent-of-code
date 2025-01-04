@@uncurried

open Stdlib
open Utils
let log = Console.log

type draw =
  | Red(int)
  | Blue(int)
  | Green(int)

let makeDraw = (str, n) => {
  switch str {
  | "red" => Red(n)
  | "blue" => Blue(n)
  | "green" => Green(n)
  | _ => failwith("invalid color")
  }
}

type game = {
  id: int,
  draws: array<draw>,
}

let parse: string => array<game> = data =>
  data
  ->splitNewline
  ->Array.map(l => {
    let ll = l->String.trim->String.split(": ")

    let id =
      ll
      ->Array.at(0)
      ->Option.flatMap(Utils.compose(String.replace(_, "Game ", ""), Int.fromString(_, ~radix=10)))
      ->Option.getUnsafe

    let draws =
      ll
      ->Array.at(1)
      ->Option.getUnsafe
      ->String.split("; ")
      ->Array.map(eachDraw =>
        eachDraw
        ->String.split(", ")
        ->Array.map(
          singleDraw => {
            let d = singleDraw->String.split(" ")
            let nColor =
              d->Array.at(0)->Option.flatMap(Int.fromString(~radix=10, ...))->Option.getUnsafe
            let color = d->Array.at(1)->Option.getUnsafe
            makeDraw(color, nColor)
          },
        )
      )
      ->Array.flat

    {id, draws} //draws
  })

/**
 * This function calculates the maximum number of each color in a given array of draws.
 * It takes an array of draws as input and returns a tuple of the maximum number of each color.
 *
 * @param {array<draw>} draws - An array of draws where each draw is either Red, Green, or Blue with an associated integer.
 * @returns {(draw, draw, draw)} - A tuple containing the maximum number of each color in the input array of draws.
 *
 * @example
 * let draws = [|Red(5), Green(3), Blue(7), Red(6), Green(4), Blue(2)|]
 * maxNumColorsEachGame(draws) // Returns (Red(6), Green(4), Blue(7))
 */
 @warning("-8")
let maxNumColorsEachGame: array<draw> => (draw, draw, draw) = draws => {
  let init = (Red(0), Green(0), Blue(0))
  draws->Array.reduce(init, (acc, x) => {
    let (Red(r), Green(g), Blue(b)) = acc
    switch x {
    | Red(n) => (Red(max(n, r)), Green(g), Blue(b))
    | Green(n) => (Red(r), Green(max(n, g)), Blue(b))
    | Blue(n) => (Red(r), Green(g), Blue(max(n, b)))
    }
  })
}

/**
 * This function filters games based on the maximum number of each color in their draws.
 * It takes an array of games and a tuple of color limits as input and returns an array of tuples.
 * Each tuple contains the id of a game and the maximum number of each color in its draws.
 * Only games where the maximum number of each color does not exceed the color limits are included in the output.
 *
 * @param {array<game>} games - An array of games where each game has an id and an array of draws.
 * @param {(draw, draw, draw)} colorLimits - A tuple of color limits where each limit is either Red, Green, or Blue with an associated integer.
 * @returns {array<(int, (draw, draw, draw))>} - An array of tuples where each tuple contains the id of a game and the maximum number of each color in its draws.
 *
 * @example
 * let games = [|game1, game2, game3|]
 * let colorLimits = (Red(10), Green(10), Blue(10))
 * maxColorWithLimits(games, colorLimits) // Returns an array of tuples where each tuple contains the id of a game and the maximum number of each color in its draws.
 */

@warning("-8")
let maxColorWithLimits: (array<game>, (draw, draw, draw)) => array<(int, (draw, draw, draw))> = (
  games,
  colorLimits,
) => {
  games->Array.filterMap(({id, draws}) => {
    let maxColors = draws->maxNumColorsEachGame
    let (Red(r), Green(g), Blue(b)) = maxColors
    let (Red(max_r), Green(max_g), Blue(max_b)) = colorLimits

    r <= max_r && g <= max_g && b <= max_b ? Some((id, maxColors)) : None
  })
}

let part1: array<game> => int = games => {
  let colorLimits = (Red(12), Green(13), Blue(14))
  maxColorWithLimits(games, colorLimits)->Array.map(((game, _)) => game)->sumIntArray
}

@warning("-8")
let part2: array<game> => int = games => {
  let maxInt = Int.Constants.maxValue
  let colorLimits = (Red(maxInt), Green(maxInt), Blue(maxInt))
  games
  ->maxColorWithLimits(colorLimits)
  ->Array.map(((_, (Red(r), Green(g), Blue(b)))) => {
    r * g * b
  })
  ->sumIntArray
}

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
