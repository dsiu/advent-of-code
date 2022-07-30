open Jest
open Expect

open Belt

let data = AOC2021_Day16_Data.data
let sampleData = AOC2021_Day16_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2021_Day16)
open AOC2021_Day16
module P = Res_parser
module Pac = Packet

let makeLiteral = (ver, literal) => Pac.Packet(Version(ver), TypeID(4), Payload_Literal(literal))
let anyChar = P.satisfy(_ => true)
describe("2021 Day16", () => {
  describe("Parser Utils", () => {
    test("hex string to binary", () => {
      let result = hexStrToBinStr("D2FE28")

      let expected = "110100101111111000101000"
      expect(result)->toEqual(expected)
    })

    test("sequenceN", () => {
      let input = "123456"
      let p = Pac.sequenceN(anyChar, 6)->P.map(binCharListToStr)
      let result = P.run(p, input)->Result.getExn->fst
      let expected = "123456"

      expect(result)->toEqual(expected)
    })

    test("sequenceN_", () => {
      let input = "123456"
      let p = Pac.sequenceN_(anyChar, 6)->P.map(binCharListToStr)
      let result = P.run(p, input)->Result.getExn->fst
      let expected = "123456"

      expect(result)->toEqual(expected)
    })
  })

  describe("Packet Parser", () => {
    test("literal 2021", () => {
      let input = "110100101111111000101000" // literal 2021
      let result = Pac.parse(input)->Result.getExn->fst
      let expected = makeLiteral(6, 2021)
      expect(result)->toEqual(expected)
    })

    test("literal 10", () => {
      let input = "11010001010" // literal 2021
      let result = Pac.parse(input)->Result.getExn->fst
      let expected = makeLiteral(6, 10)
      expect(result)->toEqual(expected)
    })

    test("literal 20", () => {
      let input = "0101001000100100" // literal 2021
      let result = Packet.parse(input)->Result.getExn->fst
      let expected = makeLiteral(2, 20)
      expect(result)->toEqual(expected)
    })

    test("op type 0 simple", () => {
      let input = "00111000000000000110111101000101001010010001001000000000" // op, type 0, len 27
      let result = Packet.parse(input)->Result.getExn->fst
      let expected = Pac.Packet(
        Version(1),
        TypeID(6),
        Payload_Operator(Operator_Type_0(27, list{makeLiteral(6, 10), makeLiteral(2, 20)})),
      )
      expect(result)->toEqual(expected)
    })

    test("op type 1 simple", () => {
      let input = "11101110000000001101010000001100100000100011000001100000" // op, type 0, len 27
      let result = Packet.parse(input)->Result.getExn->fst
      let expected = Pac.Packet(
        Version(7),
        TypeID(3),
        Payload_Operator(
          Operator_Type_1(3, list{makeLiteral(2, 1), makeLiteral(4, 2), makeLiteral(1, 3)}),
        ),
      )
      expect(result)->toEqual(expected)
    })
  })

  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 1

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 1

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Sample Data", () => {
    let result = solvePart2(sampleData)
    let expected = 2

    expect(result)->toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = solvePart2(data)
    let expected = 2

    expect(result)->toEqual(expected)
  })
})
