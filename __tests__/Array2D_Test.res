open Jest
open Expect

open Belt

describe("make", () => {
  //  open Expect
  //  open! Expect.Operators

  test("make - int", () => {
    let a = Array2D.make((3, 2), -1)
    let _ = [
      a->Array2D.set((0, 0), 4),
      a->Array2D.set((1, 0), 5),
      a->Array2D.set((2, 0), 6),
      a->Array2D.set((0, 1), 7),
      a->Array2D.set((1, 1), 8),
      a->Array2D.set((2, 1), 9),
    ]
    let result = a
    let expected = [[4, 5, 6], [7, 8, 9]]

    expect(result)->toEqual(expected)
  })

  test("make - string", () => {
    let a = Array2D.make((3, 2), "")
    let _ = [
      a->Array2D.set((0, 0), "a"),
      a->Array2D.set((1, 0), "b"),
      a->Array2D.set((2, 0), "c"),
      a->Array2D.set((0, 1), "d"),
      a->Array2D.set((1, 1), "e"),
      a->Array2D.set((2, 1), "f"),
    ]
    let result = a

    let expected = [["a", "b", "c"], ["d", "e", "f"]]
    //    let result = [true, true, true, true]

    expect(result)->toEqual(expected)
  })
})

describe("get / set", () => {
  let s = Array2D.make((2, 2), "")

  let _ = [
    s->Array2D.set((0, 0), "e"),
    s->Array2D.set((1, 0), "f"),
    s->Array2D.set((0, 1), "g"),
    s->Array2D.set((1, 1), "h"),
  ]

  let get_string_tests = list{
    (s->Array2D.get((1, 0))->Option.getExn, "f"),
    (s->Array2D.get((0, 1))->Option.getExn, "g"),
    (s->Array2D.get((0, 0))->Option.getExn, "e"),
    (s->Array2D.get((1, 1))->Option.getExn, "h"),
  }

  testAll("get - string", get_string_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let a = Array2D.make((2, 2), -1)

  let _ = [
    a->Array2D.set((0, 0), 1),
    a->Array2D.set((1, 0), 2),
    a->Array2D.set((0, 1), 3),
    a->Array2D.set((1, 1), 4),
  ]

  let b = a->Array2D.copy

  let get_tests = list{
    (a->Array2D.get((0, 0)), Some(1)),
    (a->Array2D.get((1, 0)), Some(2)),
    (a->Array2D.get((0, 1)), Some(3)),
    (a->Array2D.get((1, 1)), Some(4)),
  }

  testAll("get - int", get_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let _ = [a->Array2D.set((0, 0), 5), a->Array2D.set((0, 1), 7)]

  let set_tests = list{
    (a->Array2D.get((0, 0)), Some(5)),
    (a->Array2D.get((1, 0)), Some(2)),
    (a->Array2D.get((0, 1)), Some(7)),
    (a->Array2D.get((1, 1)), Some(4)),
  }

  testAll("set - int", set_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let _ = [b->Array2D.setYEquals(1, [9, 11])]

  let setYEquals_test = list{
    (b->Array2D.get((0, 0)), Some(1)),
    (b->Array2D.get((1, 0)), Some(2)),
    (b->Array2D.get((0, 1)), Some(9)),
    (b->Array2D.get((1, 1)), Some(11)),
  }

  testAll("setYEquals", setYEquals_test, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })
})

describe("setYEquals / getYEquals", () => {
  let a = Array2D.make((2, 2), -1)
  let _ = [
    a->Array2D.set((0, 0), 1),
    a->Array2D.set((1, 0), 2),
    a->Array2D.set((0, 1), 3),
    a->Array2D.set((1, 1), 4),
  ]

  let _ = []
})

describe("keep / map / getXYEquals / crop", () => {
  let a = Array2D.make((3, 4), -1)
  let _ = [
    a->Array2D.set((0, 0), 355),
    a->Array2D.set((1, 0), 907),
    a->Array2D.set((2, 0), 707),
    a->Array2D.set((0, 1), 404),
    a->Array2D.set((1, 1), 559),
    a->Array2D.set((2, 1), 514),
    a->Array2D.set((0, 2), 320),
    a->Array2D.set((1, 2), 891),
    a->Array2D.set((2, 2), 982),
    a->Array2D.set((0, 3), 744),
    a->Array2D.set((1, 3), 97),
    a->Array2D.set((2, 3), 876),
  ]

  test("map - int", () => {
    let result = a->Array2D.map(x => {x * 2})
    let expected = [[710, 1814, 1414], [808, 1118, 1028], [640, 1782, 1964], [1488, 194, 1752]]

    expect(result)->toEqual(expected)
  })

  let getXEquals_tests = list{
    (a->Array2D.getXEquals(0), Some([355, 404, 320, 744])),
    (a->Array2D.getXEquals(1), Some([907, 559, 891, 97])),
    (a->Array2D.getXEquals(2), Some([707, 514, 982, 876])),
  }

  testAll("getXEquals", getXEquals_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let getYEquals_tests = list{
    (a->Array2D.getYEquals(0), Some([355, 907, 707])),
    (a->Array2D.getYEquals(1), Some([404, 559, 514])),
    (a->Array2D.getYEquals(2), Some([320, 891, 982])),
  }

  testAll("getYEquals", getYEquals_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let crop_tests = list{
    (a->Array2D.crop((1, 2), ~len_x=1, ~len_y=2), [[891], [97]]),
    (a->Array2D.crop((1, 1), ~len_x=2, ~len_y=2), [[559, 514], [891, 982]]),
  }

  testAll("crop", crop_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })
})
