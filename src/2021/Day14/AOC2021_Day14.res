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

  let iterate = ({template, rules}: t) => {
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

  let iterateN = ({template, rules}: t, n) => {
    let rec inner = (t, r, n) => {
      switch n {
      | 0 => t
      | _ => inner(iterate({template: t, rules: r}), r, n - 1)
      }
    }
    inner(template, rules, n)
  }

  let part1 = t => {
    let r = {
      let ret = t->iterateN(10)
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
  data->ignore
  2
}
