open Jest
open Expect
open! Expect.Operators
open AOC2020_Day4

//open Belt

let data = AOC2020_Day4_Data.data
let testData = AOC2020_Day4_Data_Test.data

describe("2020 Day4", () => {
  test("Part 1 - Test Data", () => {
    let result = AOC2020_Day4.solvePart1(testData)
    let expected = 2

    expect(result) |> toEqual(expected)
  })

  test("Part 1 - Solve", () => {
    let result = AOC2020_Day4.solvePart1(data)
    let expected = 206

    expect(result) |> toEqual(expected)
  })

  test("validators yr", () => {
    open Passport
    let yr = validateStringAsInt(_, between(_, 1920, 2002))

    let result = [yr("1970"), yr("2010"), yr("2002")]
    let expected = [true, false, true]

    expect(result) |> toEqual(expected)
  })

  test("validators hgt", () => {
    open Passport
    let hgt = hgtValidator

    let result = [
      hgt("149cm"),
      hgt("193cm"),
      hgt("160cm"),
      hgt("59in"),
      hgt("78in"),
      hgt("60in"),
      hgt("cm"),
      hgt("cm161"),
      hgt("in"),
      hgt("in64"),
    ]
    let expected = [false, true, true, true, false, true, false, false, false, false]

    expect(result) |> toEqual(expected)
  })

  test("validators hcl", () => {
    open Passport
    let hcl = hclValidator

    let result = [
      hcl("#123456"),
      hcl("6543218"),
      hcl("#abcdef"),
      hcl("#abcdefg"),
      hcl("#1234567"),
      hcl("#ab12cg"),
      hcl("#ab12ce"),
    ]
    let expected = [true, false, true, false, false, false, true]

    expect(result) |> toEqual(expected)
  })

  test("validators ecl", () => {
    open Passport
    let ecl = eclValidator

    let result = [
      ecl("amb"),
      ecl("blu"),
      ecl("brn"),
      ecl("gry"),
      ecl("grn"),
      ecl("hzl"),
      ecl("oth"),
      ecl("blue"),
      ecl(""),
      ecl("black"),
    ]
    let expected = [true, true, true, true, true, true, true, false, false, false]

    expect(result) |> toEqual(expected)
  })

  test("validators pid", () => {
    open Passport
    let pv = pidValidator

    let result = [
      pv("123456789"),
      pv("000011112"),
      pv("1234567890"),
      pv("aaaabbbbcc"),
      pv("x23456789"),
    ]
    let expected = [true, true, false, false, false]

    expect(result) |> toEqual(expected)
  })

  test("Part 2 - Solve", () => {
    let result = AOC2020_Day4.solvePart2(data)
    let expected = 123

    expect(result) |> toEqual(expected)
  })
})
