open Belt
open Utils

let solvePart1 = data => {
  let year = 2020
  let entries = data->Js.String2.split("\n")->Array.map(s => s->intFromStringExn)

  entries->Array.reduce([], (a, x) => {
    let candidates = entries->Array.keep(y => y + x === year)

    switch candidates[0] {
    | Some(y) => a->Array.concat([y * x])
    | _ => a
    }
  })
}

let solvePart2 = data => {
  let year = 2020
  let entries = data->Js.String2.split("\n")->Array.map(s => s->intFromStringExn)

  entries->Array.reduce([], (ax, x) => {
    let yy = entries->Array.reduce([], (ay, y) => {
      let candidates = entries->Array.keep(z => z + y + x === year)

      switch candidates[0] {
      | Some(z) => ay->Array.concat([x * y * z])
      | _ => ay
      }
    })

    switch yy[0] {
    | Some(r) => ax->Array.concat([r])
    | _ => ax
    }
  })
}
