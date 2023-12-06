@@uncurried

open RescriptCore
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

let parse = data =>
  data
  ->splitNewline
  ->Array.map(l => {
    let ll = l->String.trim->String.split(": ")

    let id =
      ll
      ->Array.at(0)
      ->Option.getUnsafe
      ->String.replace("Game ", "")
      ->Int.fromString(~radix=10)
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
              d->Array.at(0)->Option.getUnsafe->Int.fromString(~radix=10)->Option.getUnsafe
            let color = d->Array.at(1)->Option.getUnsafe
            makeDraw(color, nColor)
          },
        )
      )
      ->Array.flat

    {id, draws} //draws
  })

let maxNumColorsEachGame = draws => {
  let init = (Red(0), Green(0), Blue(0))
  draws->Array.reduce(init, (acc, x) => {
    let (Red(r), Green(g), Blue(b)) = acc
    switch x {
    | Red(n) if n > r => (Red(n), Green(g), Blue(b))
    | Green(n) if n > g => (Red(r), Green(n), Blue(b))
    | Blue(n) if n > b => (Red(r), Green(g), Blue(n))
    | _ => acc
    }
  })
}

let maxColorWithLimits = (games, colorLimits) => {
  games
  ->Array.map(({id, draws}) => {
    let maxColors = draws->maxNumColorsEachGame
    let (Red(r), Green(g), Blue(b)) = maxColors
    let (Red(max_r), Green(max_g), Blue(max_b)) = colorLimits

    switch r <= max_r && g <= max_g && b <= max_b {
    | true => Some((id, maxColors))
    | false => None
    }
  })
  ->Array.keepSome
}

let part1 = games => {
  let colorLimits = (Red(12), Green(13), Blue(14))
  maxColorWithLimits(games, colorLimits)->Array.map(((game, draws)) => game)->sumIntArray
}

let part2 = games => {
  let maxInt = Int.Constants.maxValue
  let colorLimits = (Red(maxInt), Green(maxInt), Blue(maxInt))
  maxColorWithLimits(games, colorLimits)
  ->Array.map(((game, draws)) => {
    let (Red(r), Green(g), Blue(b)) = draws
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
