let data = Day3_Data.data

type claim = {
  id: int,
  x: int,
  y: int,
  w: int,
  h: int,
}

// #1 @ 669,271: 17x11
let claimRe = %re("/#(\d+)\s+@\s+(\d+),(\d+):\s(\d+)x(\d+)/i")

let parseLine = s => {
  switch s->Js.String.trim->Js.Re.exec(claimRe) {
  | Some(x) => Js.Re.captures(x)->Belt.Array.map(x => {
      switch Js.Nullable.toOption(x) {
      | Some(y) => y
      | None => ""
      }
    })
  | None => []
  }
}

let makeClaim = xs => {
  {
    id: int_of_string(xs->Array.get(1)),
    x: int_of_string(xs->Array.get(2)),
    y: int_of_string(xs->Array.get(3)),
    w: int_of_string(xs->Array.get(4)),
    h: int_of_string(xs->Array.get(5)),
  }
}

let createClaim = x => {
  parseLine(x)->makeClaim
}

data |> Js.String.split("\n") |> Js.Array.map(createClaim) |> Js.Console.log
