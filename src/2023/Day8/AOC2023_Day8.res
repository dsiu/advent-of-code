@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type direction = L | R
type node = Node(string, string)
type desert = Stdlib.Map.t<string, node>

module State = {
  type t = {here: string, steps: int}

  let compare = ({here: h1, steps: s1}, {here: h2, steps: s2}) => {
    let h = String.compare(h1, h2)
    h->Ordering.isEqual ? Int.compare(s1, s2) : h
  }
}

/**
 * This function is used to determine the direction based on the current step.
 * It takes an array of directions and the current step as arguments.
 * The function uses the modulus operator to cycle through the array of directions based on the current step.
 * This ensures that the direction is always within the bounds of the array.
 *
 * @param {array<direction>} directions - An array of directions (L or R).
 * @param {int} steps - The current step in the journey.
 * @return {direction} - The direction for the current step.
 */
let getDirection: (array<direction>, int) => direction = (directions, steps) => {
  directions->Array.getUnsafe(Int.mod(steps, directions->Array.length))
}

/**
 * This function checks if the current location is the goal.
 * It takes a state object as an argument, which contains the current location and the number of steps taken.
 * The function checks if the last character of the current location string is "Z", which signifies the goal.
 * If the last character is "Z", the function returns true, otherwise it returns false.
 *
 * @param {State.t} state - The current state of the journey, containing the current location and the number of steps taken.
 * @return {bool} - Returns true if the current location is the goal, otherwise returns false.
 */
let isGoal: State.t => bool = ({here, steps: _}) => {
  here->String.last == "Z"
}

/**
 * This function is used to determine the next step in the journey.
 * It takes the current desert map, the current state, and the direction as arguments.
 * The function retrieves the node from the desert map based on the current location.
 * The node contains two locations, thereL and thereR, representing the next locations if the direction is L or R respectively.
 * The function then increments the steps and updates the current location based on the direction.
 *
 * @param {desert} desert - The current desert map, a map of nodes where each node represents a location in the desert.
 * @param {State.t} state - The current state of the journey, containing the current location and the number of steps taken.
 * @param {direction} direction - The direction for the current step (L or R).
 * @return {State.t} - The updated state after taking the step in the given direction.
 */
let step: (desert, State.t, direction) => State.t = (desert, {here, steps}, direction) => {
  let Node(thereL, thereR) = desert->Stdlib.Map.get(here)->Option.getExn
  switch direction {
  | L => {here: thereL, steps: steps + 1}
  | R => {here: thereR, steps: steps + 1}
  }
}

/**
 * This function is used to walk through the desert.
 * It takes the current desert map, an array of directions, and the current state as arguments.
 * The function checks if the current location is the goal. If it is, it returns the current state.
 * If the current location is not the goal, it determines the next step based on the current direction and state, and recursively calls itself with the updated state.
 * This ensures that the function will continue to walk through the desert until it reaches the goal.
 *
 * @param {desert} desert - The current desert map, a map of nodes where each node represents a location in the desert.
 * @param {array<direction>} directions - An array of directions (L or R).
 * @param {State.t} state - The current state of the journey, containing the current location and the number of steps taken.
 * @return {State.t} - The final state after reaching the goal.
 */
let rec walk: (desert, array<direction>, State.t) => State.t = (desert, directions, state) => {
  state->isGoal
    ? state
    : walk(desert, directions, step(desert, state, getDirection(directions, state.steps)))
}

/**
 * This function is used to solve the first part of the problem.
 * It takes an array of directions and the desert map as arguments.
 * The function sets the starting location to "AAA" and the number of steps to 0.
 * It then calls the walk function with the desert map, the array of directions, and the starting state.
 * The walk function will continue to walk through the desert until it reaches the goal.
 * Once the goal is reached, the function returns the number of steps taken to reach the goal.
 *
 * @param {array<direction>, desert} directions, desert - An array of directions (L or R) and the desert map.
 * @return {int} - The number of steps taken to reach the goal.
 */
let part1: ((array<direction>, desert)) => int = ((directions, desert)) => {
  open State
  let start = {here: "AAA", steps: 0}
  let goal = walk(desert, directions, start)
  goal.steps
}

/**
 * This function is used to solve the second part of the problem.
 * It takes an array of directions and the desert map as arguments.
 * The function retrieves all the keys from the desert map and filters out those that do not end with "A".
 * For each remaining key, it simulates a walk through the desert starting from that location and calculates the number of steps to reach the goal.
 * It then converts these step counts to BigInts and calculates the least common multiple (LCM) of all the step counts.
 * The function returns this LCM as the solution to the second part of the problem.
 *
 * @param {array<direction>, desert} directions, desert - An array of directions (L or R) and the desert map.
 * @return {BigInt.t} - The least common multiple of the number of steps taken to reach the goal from all starting locations ending with "A".
 */
let part2: ((array<direction>, desert)) => BigInt.t = ((directions, desert)) => {
  open State
  desert
  ->Stdlib.Map.keys
  ->Iterator.toArray
  ->Array.filter(Fn.compose3(String.last, String.compare("A", ...), Ordering.isEqual, ...))
  ->Array.map(s => walk(desert, directions, {here: s, steps: 0}).steps)
  ->Array.map(BigInt.fromInt)
  ->Array.foldl1(Math.BigInt.lcm)
}

module ProblemParser = {
  module P = ReludeParse.Parser
  open P.Infix
  let justSpace: P.t<unit> = P.void(P.many(P.str(" ")))
  let debug = P.tapLog

  let mkNode = a => b => {
    Node(a, b)
  }

  let mkDesertLine = a => b => {
    (a, b)
  }

  let nameP = P.\"<$$>"(P.many(P.anyAlphaOrDigit), l => l->List.toArray->Array.join(""))

  let nodeP =
    mkNode->\"<$>"(P.str("(")->\"*>"(nameP)->\"<*"(P.str(", ")))->\"<*>"(nameP->\"<*"(P.str(")")))

  let desertLineP =
    mkDesertLine->\"<$>"(justSpace->\"*>"(nameP)->\"<*"(P.str(" = ")))->\"<*>"(nodeP)

  let mkDesert = a => {
    Stdlib.Map.fromArray(a->List.toArray)
  }

  let desertP = mkDesert->\"<$>"(P.sepBy(P.eol, desertLineP))

  let directionP = L->\"<$"(P.str("L"))->\"<|>"(R->\"<$"(P.str("R")))

  let mkProblem: list<direction> => desert => (array<direction>, desert) = (a: list<direction>) => (
    b: desert,
  ) => {
    (a->List.toArray, b)
  }

  let problemP = mkProblem->\"<$>"(P.many(directionP)->\"<*"(P.many1(P.eol)))->\"<*>"(desertP)

  let run: string => (array<direction>, desert) = data => {
    //    P.runParser("(AAA, BBB)", nodeP)->log
    //    P.runParser("CCC = (DDD, EEE) ", desertLineP)->log
    //    P.runParser(`C11 = (D22, E33)\nF4F = (G5G, 6HH)`, desertP)->log
    //    P.runParser(`LRRLRR`, dLineP)->log
    data->P.runParser(problemP)->Result.getExn
  }
}

let solvePart1 = data => {
  data->ProblemParser.run->part1
}

let solvePart2 = data => {
  data->ProblemParser.run->part2
}
