open Stdlib
module P = Res_parser

let charListToString = chars =>
  chars->Belt.List.map(Char.escaped)->Belt.List.reduce("", String.concat)
let anyChar = P.satisfy(char => char != '"')
let string = P.many(anyChar)->P.map(charListToString)
let zero = P.char('0')
let oneThroughNine = P.satisfy(c => c >= '1' && '9' >= c)
let charToString = c => c->int_of_char->Js.String.fromCharCode
let rec concatStringList = chars => {
  switch chars {
  | list{} => ""
  | list{head, ...rest} => head ++ concatStringList(rest)
  }
}
let digit = zero->P.orElse(oneThroughNine)->P.map(charToString)
let digits = P.atLeastOne(digit)->P.map(concatStringList)
let whitespace = P.satisfy(char => {
  switch char {
  | ' ' | '\n' | '\r' | '\t' => true
  | _ => false
  }
})
let manyWhitespace = P.many(whitespace)
