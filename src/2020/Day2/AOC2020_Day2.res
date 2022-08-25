open Belt
open Utils

let log = Js.Console.log
let data1 = `1-3 a: abcde
            1-3 b: cdefg
            2-9 c: ccccccccc`

module Password = {
  type t = {
    min: int,
    max: int,
    char: string,
    input: string,
  }

  let formatRe = %re("/(\d+)-(\d+)\s+([A-Za-z]+):\s+(.*)/i")

  let make = s => {
    let args = switch s->Js.Re.exec_(formatRe, _) {
    | Some(x) => Js.Re.captures(x)->Array.map(x => Js.Nullable.toOption(x)->Option.getExn)
    | None => []
    }
    {
      min: args->Array.getExn(1)->intFromStringExn,
      max: args->Array.getExn(2)->intFromStringExn,
      char: args->Array.getExn(3),
      input: args->Array.getExn(4),
    }
  }

  let isValidPart1 = t => {
    let count = t.input->Js.String2.split(t.char)->Array.size - 1
    count >= t.min && count <= t.max
  }

  let isValidPart2 = t => {
    let matchFirst = t.input->Js.String2.charAt(t.min - 1) == t.char
    let matchSecond = t.input->Js.String2.charAt(t.max - 1) == t.char
    matchFirst != matchSecond
  }
}

let solvePart1 = data => {
  let pws = data->Js.String2.split("\n")->Array.map(s => s->Js.String2.trim->Password.make)
  //  pws->Js.Console.log
  pws->Array.keep(Password.isValidPart1)->Array.size
}
let solvePart2 = data => {
  let pws = data->Js.String2.split("\n")->Array.map(s => s->Js.String2.trim->Password.make)
  //  pws->Js.Console.log
  pws->Array.keep(Password.isValidPart2)->Array.size
}
