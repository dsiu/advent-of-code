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

  let iterateN_tail_opt = ({template, rules}: t, n) => {
    let rec inner = (t, r, n) => {
      switch n {
      | 0 => t
      | _ => inner(iterate_tail_opt({template: t, rules: r}), r, n - 1)
      }
    }
    inner(template, rules, n)
  }

  let solve_with_result = (t, n) => {
    let r = {
      let ret = t->iterateN_tail_opt(n)
      ret->List.reduce(HashMap.String.make(~hintSize=10), (acc, k) => {
        switch acc->HashMap.String.get(k) {
        | Some(v) => acc->HashMap.String.set(k, v + 1)
        | None => acc->HashMap.String.set(k, 1)
        }
        acc
      })
    }->HashMap.String.toArray

    let (_, max_n) = r->Array.reduce(("", 0), (acc, (k, v)) => {
      let (_, va) = acc
      v > va ? (k, v) : acc
    })
    let (_, min_n) = r->Array.reduce(("", max_int), (acc, (k, v)) => {
      let (_, va) = acc
      v < va ? (k, v) : acc
    })
    max_n - min_n
  }

  // should refactor this. 2021 Day 12 uses this too
  let update_value = (h, k, f) => {
    switch h->HashMap.String.get(k) {
    | Some(x) => h->HashMap.String.set(k, f(Some(x)))
    | None => h->HashMap.String.set(k, f(None))
    }
    h
  }

  let increase_by_n = (v, n) => {
    switch v {
    | Some(x) => x + n
    | None => n
    }
  }

  let increase_by_1 = (h, k) => update_value(h, k, increase_by_n(_, 1))

  let genPairsMap = template => {
    let rec inner = (l, acc) => {
      switch l {
      | list{} => acc
      | list{last} => acc->increase_by_1(last)
      | list{h1, h2, ...rest} =>
        acc->increase_by_1(h1 ++ h2)->ignore
        inner(list{h2, ...rest}, acc)
      }
    }
    let acc = HashMap.String.make(~hintSize=40)
    inner(template, acc)
  }

  // given a string of 2 char "ab", generate 2 new keys in [k1, k2]. k1 = ac, k2=cb where c = morph(a,b)
  // if string is 1 char, return string
  let genNewKeys = (k: string, rules) => {
    switch Js.String2.length(k) {
    | 1 => [k]
    | 2 => {
        let a = Js.String2.substring(k, ~from=0, ~to_=1)
        let b = Js.String2.substring(k, ~from=1, ~to_=2)
        let c = morph(a, b, rules)
        [a ++ c, c ++ b]
      }
    | _ => raise(Not_found)
    }
  }

  let iterate = (m, rules) => {
    let m' = HashMap.String.make(~hintSize=40)
    m->HashMap.String.forEach((k, v) => {
      k
      ->genNewKeys(rules)
      ->Array.forEach(k' => {
        m'
        ->update_value(k', v' => {
          switch v' {
          | Some(x) => v + x
          | None => v
          }
        })
        ->ignore
      })
    })
    m'
  }

  let iterateN = ({template, rules}: t, n) => {
    let rec inner = (m, r, n) => {
      switch n {
      | 0 => m
      | _ => inner(iterate(m, r), r, n - 1)
      }
    }
    let init = genPairsMap(template)

    inner(init, rules, n)
  }

  let countPolymers = m => {
    let r = HashMap.String.make(~hintSize=40)
    m->HashMap.String.forEach((k, v) => {
      k
      ->Js.String2.split("")
      ->Array.forEach(c => {
        r
        ->update_value(c, v' => {
          switch v' {
          | Some(x) => x + v
          | None => v
          }
        })
        ->ignore
      })
    })
    r
  }

  let solve = (t, n) => {
    let r = t->iterateN(n)
    r->HashMap.String.toArray->Js.log
    r->countPolymers
  }

  //  let solve = (t, n) => {
  //    t->iteranteN(n)->HashMap.String.toArray
  //    let (max_p, max_n) = r->Array.reduce(("", 0), (acc, (k, v)) => {
  //      let (_, va) = acc
  //      v > va ? (k, v) : acc
  //    })
  //    let (min_p, min_n) = r->Array.reduce(("", max_int), (acc, (k, v)) => {
  //      let (_, va) = acc
  //      v < va ? (k, v) : acc
  //    })
  //    max_n - min_n
  //  }

  let part1 = solve_with_result(_, 10)
  let part2 = solve(_, 10)
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
  p->Polymer.part2->HashMap.String.toArray->Js.log
  2
}
