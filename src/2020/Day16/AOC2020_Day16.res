//
// https://work.njae.me.uk/2020/12/25/advent-of-code-2020-day-16/
//

open Belt
open Utils
open FP_Utils
let log = Js.Console.log
let log2 = Js.Console.log2

type range = Range(int, int)
type body = Body(range, range)
type ruleSet = RuleSet(Map.String.t<body>)
type ticket = Ticket(array<int>)

let rangeToString = (Range(a, b)) => a->Int.toString ++ "-" ++ b->Int.toString
let bodyToString = (Body(r1, r2)) => r1->rangeToString ++ " or " ++ r2->rangeToString

let inRange = (Range(lower, upper), value) => lower <= value && value <= upper
let matchesRule = (Body(a, b), value) => inRange(a, value) || inRange(b, value)

let validForAnyField = (RuleSet(rules), value) => {
  open Array
  some(rules->Map.String.valuesToArray, matchesRule(_, value))
}

let ticketErrorRate = (rules, tickets: array<ticket>) => {
  open Array
  sumIntArray(tickets->map((Ticket(t)) => t->keep(v => !validForAnyField(rules, v)))->flatten)
}

let isValidTicket = (rules, Ticket(ticket)) => {
  open Array
  every(map(ticket, v => validForAnyField(rules, v)), identity)
}

let part1 = ticketErrorRate

@@warning("-8")
let parse = data => {
  let trim = Js.String2.trim
  let split = Js.String2.split
  let map = Array.map
  let intFromStrEx = Int.fromString->compose(Option.getExn)

  let [rules, my, nearby] = data->splitDoubleNewline->map(x => x->splitNewline->map(trim))

  let parseRange = s => {
    let [a, b] = s->split("-")->Array.map(intFromStrEx)
    Range(a, b)
  }

  let parseRule = s => {
    let [ruleStr, rangesStr] = s->split(": ")
    let [r1, r2] = rangesStr->split(" or ")->Array.map(parseRange)
    (ruleStr, Body(r1, r2))
  }

  let ruleSet = rules->Array.map(parseRule)->Map.String.fromArray->RuleSet

  let parseTicket = s => {
    s->split(",")->map(intFromStrEx)->Ticket
  }

  let myTicket = my->Array.getExn(1)->parseTicket
  let nearbyTickets = nearby->Array.sliceToEnd(1)->map(parseTicket)

  //  ruleSet->Printable.MapString.toString(bodyToString)->log
  //  myTicket->log
  //  nearbyTickets->log

  (ruleSet, myTicket, nearbyTickets)
}

let solvePart1 = data => {
  let (rules, _myTicket, nearbyTickets) = data->parse
  part1(rules, nearbyTickets)
}

let solvePart2 = data => {
  let (rules, myTicket, nearbyTickets) = data->parse
  isValidTicket(rules, myTicket)->log
  nearbyTickets->Array.map(isValidTicket(rules, _))->log
  2
}
