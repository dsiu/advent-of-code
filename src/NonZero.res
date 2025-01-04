//
// ref: https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html
//
// requires Haskell's PatternSynonyms extension so ReScript won't work....


let log = Console.log
let log2 = Console.log2

type nonZero<'a> = UnsafeNonZero('a)

let unNonZero: nonZero<'a> => 'a = (UnsafeNonZero(a)) => a

let nonZero: 'a => option<nonZero<'a>> = a => {
  switch a {
  | 0 => None
  | i => Some(UnsafeNonZero(i))
  }
}

@warning("-8")
let safeDivide: (int, option<nonZero<int>>) => int = (a, b) => {
  switch b {
  | Some(UnsafeNonZero(b)) => a / b
  }
}

safeDivide(1, nonZero(2))->log2("case 1")
safeDivide(3, nonZero(0))->log2("case 2")
