open Jest
open Expect

open Belt

let data = AOC2021_Day18_Data.data
let sampleData = AOC2021_Day18_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2021_Day18)
open AOC2021_Day18.SnailFish
open AOC2021_Day18.SnailFish.Parser
open Tree

describe("2021 Day18", () => {
  let parseAndReduce = x => {
    x->parseAndGetResult->reduce
  }

  let makeReduceTests = List.map(_, ((t, r)) => {
    (t->parseAndReduce->treeToString, r)
  })

  let reduce_tests =
    list{
      ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
      ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
      ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
      ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"),
    }->makeReduceTests

  testAll("reduce", reduce_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let makeMagnitudeTests = List.map(_, ((t, r)) => {
    (t->parseAndReduce->magnitude, r)
  })

  let magnitude_tests =
    list{
      ("[[1,2],[[3,4],5]]", 143),
      ("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384),
      ("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445),
      ("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791),
      ("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137),
      ("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488),
    }->makeMagnitudeTests

  testAll("magnitude", magnitude_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let makeSumOfTests = List.map(_, (((a, b), r)) => {
    (snailAdd(a->parseAndGetResult, b->parseAndGetResult)->treeToString, r)
  })

  let sumOf_tests =
    list{
      (("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]"), "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"),
    }->makeSumOfTests

  testAll("sumOf", sumOf_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 4140

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 4088

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 3993

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 4536

    expect(result)->toEqual(expected)
  })
})
