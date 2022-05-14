open Belt
open Utils
let log = Js.Console.log

module Polymer = {
  type template = list<string>
  type rules = HashMap.String.t<string>

  type t = {
    template: template,
    rules: rules,
  }

  let make = (template, rules) => {
    {
      template: template->List.fromArray,
      rules: {
        let r = HashMap.String.make(~hintSize=40)
        rules->Array.forEach(((k, v)) => {
          r->HashMap.String.set(k, v)->ignore
        })
        r
      },
    }
  }

  let morph = (a: string, b: string, rules): string => {
    let k = a ++ b
    rules->HashMap.String.get(k)->Option.getExn
  }

  let iterate_no_tail_opt = ({template, rules}: t) => {
    let rec inner = (l, rules) => {
      switch l {
      | list{} => list{}
      | list{last} => list{last}
      | list{h1, h2, ...rest} =>
        List.concat(list{h1, morph(h1, h2, rules)}, inner(list{h2, ...rest}, rules))
      }
    }
    inner(template, rules)
  }

  let iterate_tail_opt = ({template, rules}: t) => {
    let rec inner = (l, rules, acc) => {
      switch l {
      | list{} => acc
      | list{last} => List.concat(acc, list{last})
      | list{h1, h2, ...rest} =>
        inner(list{h2, ...rest}, rules, List.concat(acc, list{h1, morph(h1, h2, rules)}))
      }
    }
    inner(template, rules, list{})
  }

  // should refactor this. 2021 Day 12 uses this too
  let increase = (h, k) => {
    switch h->HashMap.String.get(k) {
    | Some(x) => h->HashMap.String.set(k, x + 1)
    | None => h->HashMap.String.set(k, 1)
    }
    h
  }

  let iterate = ({template, rules}: t) => {
    let rec inner = (l, rules, acc) => {
      switch l {
      | list{} => acc
      | list{last} => acc->increase(last)
      | list{h1, h2, ...rest} => {
          acc->increase(h1)->ignore
          acc->increase(morph(h1, h2, rules))->ignore
          inner(list{h2, ...rest}, rules, acc)
        }
      }
    }
    let acc = HashMap.String.make(~hintSize=40)
    inner(template, rules, acc)
  }

  let iterateN = ({template, rules}: t, n) => {
    let rec inner = (t, r, n) => {
      switch n {
      | 0 => t
      | _ => inner(iterate({template: t, rules: r}), r, n - 1)
      }
    }
    inner(template, rules, n)
  }

  let solve_with_result = (t, n) => {
    let r = {
      let ret = t->iterateN(n)
      ret->List.reduce(HashMap.String.make(~hintSize=10), (acc, k) => {
        switch acc->HashMap.String.get(k) {
        | Some(v) => acc->HashMap.String.set(k, v + 1)
        | None => acc->HashMap.String.set(k, 1)
        }
        acc
      })
    }->HashMap.String.toArray

    let (max_p, max_n) = r->Array.reduce(("", 0), (acc, (k, v)) => {
      let (_, va) = acc
      v > va ? (k, v) : acc
    })
    let (min_p, min_n) = r->Array.reduce(("", max_int), (acc, (k, v)) => {
      let (_, va) = acc
      v < va ? (k, v) : acc
    })
    max_n - min_n
  }

  let solve = (t, n) => {
    t->iteranteN(n)->HashMap.String.toArray
    let (max_p, max_n) = r->Array.reduce(("", 0), (acc, (k, v)) => {
      let (_, va) = acc
      v > va ? (k, v) : acc
    })
    let (min_p, min_n) = r->Array.reduce(("", max_int), (acc, (k, v)) => {
      let (_, va) = acc
      v < va ? (k, v) : acc
    })
    max_n - min_n
  }

  let part1 = solve(_, 10)
  let part2 = solve(_, 40)
}

let parse = data => {
  open Js.String2
  open Option

  let parsed = data->splitDoubleNewline
  let template = parsed[0]->Option.getExn
  let rules = parsed[1]->Option.getExn
  (
    template->trim->split(""),
    rules
    ->splitNewline
    ->Array.map(x => {
      let s = x->trim->split(" -> ")
      (s[0]->getExn, s[1]->getExn)
    }),
  )
}

let solvePart1 = data => {
  let (template, rules) = data->parse
  let p = Polymer.make(template, rules)
  //  p.template->List.toArray->Js.log
  //  p.rules->HashMap.String.toArray->Js.log
  p->Polymer.part1
}

let solvePart2 = data => {
  let (template, rules) = data->parse
  let p = Polymer.make(template, rules)
  //  p.template->List.toArray->Js.log
  //  p.rules->HashMap.String.toArray->Js.log
  p->Polymer.part2
}
