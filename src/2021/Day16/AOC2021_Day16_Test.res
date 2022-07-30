open Jest
open Expect

open Belt

let data = AOC2021_Day16_Data.data
let sampleData = AOC2021_Day16_Data_Sample.data
//let {solvePart1, solvePart2} = module(AOC2021_Day16)
open AOC2021_Day16
module P = Res_parser
module Pac = Packet

let literal = (ver, l) => Pac.Packet(Version(ver), TypeID(4), Pac.Literal(l))
let op_type_0 = (ver, t, (len, xs)) => Pac.Packet(Version(ver), TypeID(t), Pac.Op_Type_0(len, xs))
let op_type_1 = (ver, t, (len, xs)) => Pac.Packet(Version(ver), TypeID(t), Pac.Op_Type_1(len, xs))

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
      let expected = literal(6, 2021)
      expect(result)->toEqual(expected)
    })

    test("literal 10", () => {
      let input = "11010001010" // literal 2021
      let result = Pac.parse(input)->Result.getExn->fst
      let expected = literal(6, 10)
      expect(result)->toEqual(expected)
    })

    test("literal 20", () => {
      let input = "0101001000100100" // literal 2021
      let result = Packet.parse(input)->Result.getExn->fst
      let expected = literal(2, 20)
      expect(result)->toEqual(expected)
    })

    test("op type 0 simple", () => {
      let input = "00111000000000000110111101000101001010010001001000000000" // op, type 0, len 27
      let result = Packet.parse(input)->Result.getExn->fst
      let expected = op_type_0(1, 6, (27, list{literal(6, 10), literal(2, 20)}))

      expect(result)->toEqual(expected)
    })

    test("op type 1 simple", () => {
      let input = "11101110000000001101010000001100100000100011000001100000" // op, type 0, len 27
      let result = Packet.parse(input)->Result.getExn->fst
      let expected = op_type_1(7, 3, (3, list{literal(2, 1), literal(4, 2), literal(1, 3)}))
      expect(result)->toEqual(expected)
    })

    test("example 1", () => {
      // represents an operator packet (version 4) which contains an operator packet (version 1) which contains an operator packet (version 5) which contains a literal value (version 6); this packet has a version sum of 16.
      let input = "8A004A801A8002F478"->hexStrToBinStr // op, type 0, len 27
      let result = Packet.parse(input)->Result.getExn->fst
      let versionSum = Pac.version_sum(result)

      let expected = op_type_1(
        4,
        2,
        (1, list{op_type_1(1, 2, (1, list{op_type_0(5, 2, (11, list{literal(6, 15)}))}))}),
      )
      expect((result, versionSum))->toEqual((expected, 16))
    })

    test("example 2", () => {
      // represents an operator packet (version 3) which contains two sub-packets; each sub-packet is an operator packet that contains two literal values. This packet has a version sum of 12.
      let input = "620080001611562C8802118E34"->hexStrToBinStr // op, type 0, len 27
      let result = Packet.parse(input)->Result.getExn->fst
      let versionSum = Pac.version_sum(result)

      let expected = op_type_1(
        3,
        0,
        (
          2,
          list{
            op_type_0(0, 0, (22, list{literal(0, 10), literal(5, 11)})),
            op_type_1(1, 0, (2, list{literal(0, 12), literal(3, 13)})),
          },
        ),
      )

      expect((result, versionSum))->toEqual((expected, 12))
    })

    test("example 3", () => {
      // has the same structure as the previous example, but the outermost packet uses a different length type ID. This packet has a version sum of 23.
      let input = "C0015000016115A2E0802F182340"->hexStrToBinStr // op, type 0, len 27
      let result = Packet.parse(input)->Result.getExn->fst
      let versionSum = Pac.version_sum(result)

      let expected = op_type_0(
        6,
        0,
        (
          84,
          list{
            op_type_0(0, 0, (22, list{literal(0, 10), literal(6, 11)})),
            op_type_1(4, 0, (2, list{literal(7, 12), literal(0, 13)})),
          },
        ),
      )
      expect((result, versionSum))->toEqual((expected, 23))
    })

    test("example 4", () => {
      // has the same structure as the previous example, but the outermost packet uses a different length type ID. This packet has a version sum of 23.
      let input = "A0016C880162017C3686B18A3D4780"->hexStrToBinStr // op, type 0, len 27
      let result = Packet.parse(input)->Result.getExn->fst
      let versionSum = Pac.version_sum(result)

      let expected = op_type_0(
        5,
        0,
        (
          91,
          list{
            op_type_1(
              1,
              0,
              (
                1,
                list{
                  op_type_1(
                    3,
                    0,
                    (
                      5,
                      list{
                        literal(7, 6),
                        literal(6, 6),
                        literal(5, 12),
                        literal(2, 15),
                        literal(2, 15),
                      },
                    ),
                  ),
                },
              ),
            ),
          },
        ),
      )
      expect((result, versionSum))->toEqual((expected, 31))
    })
  })

  test("Part 1 - Sample Data", () => {
    let result = solvePart1(sampleData)
    let expected = 31

    expect(result)->toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = solvePart1(data)
    let expected = 1012

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
