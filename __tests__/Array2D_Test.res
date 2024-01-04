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

describe("Array2D.make", () => {
  test("should create a 2D array with given dimensions and initial value", () => {
    let result = Array2D.make((3, 2), 0)
    expect(result)->toEqual([[0, 0, 0], [0, 0, 0]])
  })

  test("should create an empty 2D array when dimensions are zero", () => {
    let result = Array2D.make((0, 0), 0)
    expect(result)->toEqual([])
  })

  test("should create a 2D array with single row when y dimension is 1", () => {
    let result = Array2D.make((3, 1), "test")
    expect(result)->toEqual([["test", "test", "test"]])
  })

  test("should create a 2D array with single column when x dimension is 1", () => {
    let result = Array2D.make((1, 3), "test")
    expect(result)->toEqual([["test"], ["test"], ["test"]])
  })
})

describe("copy", () => {
  test("should create a copy of a 2D array", () => {
    let original = Array2D.make((3, 2), 0)
    let copy = Array2D.copy(original)
    expect(copy)->toEqual(original)
  })

  test("should not affect the original array when modifying the copy", () => {
    let original = Array2D.make((3, 2), 0)
    let copy = Array2D.copy(original)
    Array2D.set(copy, (1, 1), 1)->ignore
    expect(original)->toEqual(Array2D.make((3, 2), 0))
  })

  test("should create a copy of an empty 2D array", () => {
    let original = Array2D.make((0, 0), 0)
    let copy = Array2D.copy(original)
    expect(copy)->toEqual(original)
  })
})

describe("lengthY", () => {
  test("should return the number of rows in the 2D array", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.lengthY)->toEqual(2)
  })

  test("should return 0 for an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 0)
    expect(array2D->Array2D.lengthY)->toEqual(0)
  })
})

describe("lengthX", () => {
  test("should return the number of columns in the 2D array", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.lengthX)->toEqual(3)
  })

  test("should return 0 for an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 0)
    expect(array2D->Array2D.lengthX)->toEqual(0)
  })
})

describe("isValidXY", () => {
  test("should return true for valid coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.isValidXY((1, 1)))->toBe(true)
  })

  test("should return false for coordinates outside the array", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.isValidXY((3, 2)))->toBe(false)
  })

  test("should return false for negative coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.isValidXY((-1, -1)))->toBe(false)
  })

  test("should return true for edge coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.isValidXY((2, 1)))->toBe(true)
  })
})

describe("set", () => {
  test("should set the value at the given coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    Array2D.set(array2D, (1, 1), 1)->ignore
    expect(array2D->Array2D.get((1, 1)))->toEqual(Some(1))
  })

  test("should not change other values", () => {
    let array2D = Array2D.make((3, 2), 0)
    Array2D.set(array2D, (1, 1), 1)->ignore
    expect(array2D->Array2D.get((0, 0)))->toEqual(Some(0))
  })

  test("should return false for invalid coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(Array2D.set(array2D, (3, 2), 1))->toBe(false)
  })
})

describe("setYEquals", () => {
  test("should set the entire row to the given array", () => {
    let array2D = Array2D.make((3, 2), 0)
    Array2D.setYEquals(array2D, 1, [1, 1, 1])->ignore
    expect(array2D->Array2D.getYEquals(1))->toEqual(Some([1, 1, 1]))
  })

  test("should not change other rows", () => {
    let array2D = Array2D.make((3, 2), 0)
    Array2D.setYEquals(array2D, 1, [1, 1, 1])->ignore
    expect(array2D->Array2D.getYEquals(0))->toEqual(Some([0, 0, 0]))
  })
})

describe("get", () => {
  test("should return the value at the given coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.get((1, 1)))->toEqual(Some(0))
  })

  test("should return None for invalid coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.get((3, 2)))->toEqual(None)
  })
})

describe("getExn", () => {
  test("should return the value at the given coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.getExn((1, 1)))->toEqual(0)
  })

  test("should throw an exception for invalid coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    let fn = () => array2D->Array2D.getExn((3, 2))
    expect(fn)->toThrow
  })
})

describe("getYEquals", () => {
  test("should return the row at the given index", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.getYEquals(1))->toEqual(Some([0, 0, 0]))
  })

  test("should return None for invalid index", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.getYEquals(2))->toEqual(None)
  })
})

describe("getXEquals", () => {
  test("should return the column at the given index", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.getXEquals(1))->toEqual(Some([0, 0]))
  })

  test("should return None for invalid index", () => {
    let array2D = Array2D.make((3, 2), 0)
    expect(array2D->Array2D.getXEquals(3))->toEqual(None)
  })
})

describe("map", () => {
  test("should apply the function to each element in the 2D array", () => {
    let array2D = Array2D.make((3, 2), 1)
    let result = array2D->Array2D.map(x => x * 2)
    expect(result)->toEqual([[2, 2, 2], [2, 2, 2]])
  })

  test("should return an empty 2D array when given an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 1)
    let result = array2D->Array2D.map(x => x * 2)
    expect(result)->toEqual([])
  })

  test("should not modify the original 2D array", () => {
    let array2D = Array2D.make((3, 2), 1)
    let _ = array2D->Array2D.map(x => x * 2)
    expect(array2D)->toEqual([[1, 1, 1], [1, 1, 1]])
  })
})

describe("mapU", () => {
  test("should apply the function to each element in the 2D array", () => {
    let array2D = Array2D.make((3, 2), 1)
    let result = array2D->Array2D.mapU((. x) => x * 2)
    expect(result)->toEqual([[2, 2, 2], [2, 2, 2]])
  })

  test("should return an empty 2D array when given an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 1)
    let result = array2D->Array2D.mapU((. x) => x * 2)
    expect(result)->toEqual([])
  })

  test("should not modify the original 2D array", () => {
    let array2D = Array2D.make((3, 2), 1)
    let _ = array2D->Array2D.mapU((. x) => x * 2)
    expect(array2D)->toEqual([[1, 1, 1], [1, 1, 1]])
  })
})

describe("mapWithIndex", () => {
  test("should apply the function to each element with its coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    let result = array2D->Array2D.mapWithIndex((coords, _) => coords)
    expect(result)->toEqual([[(0, 0), (1, 0), (2, 0)], [(0, 1), (1, 1), (2, 1)]])
  })

  test("should return an empty 2D array when given an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 0)
    let result = array2D->Array2D.mapWithIndex((coords, _) => coords)
    expect(result)->toEqual([])
  })

  test("should not modify the original 2D array", () => {
    let array2D = Array2D.make((3, 2), 0)
    let _ = array2D->Array2D.mapWithIndex((coords, _) => coords)
    expect(array2D)->toEqual([[0, 0, 0], [0, 0, 0]])
  })
})

describe("mapWithIndexU", () => {
  test("should apply the function to each element with its coordinates", () => {
    let array2D = Array2D.make((3, 2), 0)
    let result = array2D->Array2D.mapWithIndexU((. coords, _) => coords)
    expect(result)->toEqual([[(0, 0), (1, 0), (2, 0)], [(0, 1), (1, 1), (2, 1)]])
  })

  test("should return an empty 2D array when given an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 0)
    let result = array2D->Array2D.mapWithIndexU((. coords, _) => coords)
    expect(result)->toEqual([])
  })

  test("should not modify the original 2D array", () => {
    let array2D = Array2D.make((3, 2), 0)
    let _ = array2D->Array2D.mapWithIndexU((. coords, _) => coords)
    expect(array2D)->toEqual([[0, 0, 0], [0, 0, 0]])
  })
})

describe("reduce", () => {
  test("should correctly reduce a 2D array to a single value", () => {
    let array2D = Array2D.make((3, 2), 1)
    let result = array2D->Array2D.reduce(0, (acc, x) => acc + x)
    expect(result)->toEqual(6)
  })

  test("should return the initial value for an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 0)
    let result = array2D->Array2D.reduce(10, (acc, x) => acc + x)
    expect(result)->toEqual(10)
  })
})

describe("reduceU", () => {
  test("should correctly reduce a 2D array to a single value", () => {
    let array2D = Array2D.make((3, 2), 1)
    let result = array2D->Array2D.reduceU(0, (. acc, x) => acc + x)
    expect(result)->toEqual(6)
  })

  test("should return the initial value for an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 0)
    let result = array2D->Array2D.reduceU(10, (. acc, x) => acc + x)
    expect(result)->toEqual(10)
  })
})

describe("reduceWithIndex", () => {
  test("should correctly reduce a 2D array to a single value with index", () => {
    let array2D = Array2D.make((3, 2), 1)
    let result = array2D->Array2D.reduceWithIndex(0, (acc, x, _) => acc + x)
    expect(result)->toEqual(6)
  })

  test("should return the initial value for an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 0)
    let result = array2D->Array2D.reduceWithIndex(10, (acc, x, _) => acc + x)
    expect(result)->toEqual(10)
  })

  test("should correctly use the index in the reduction function", () => {
    let array2D = Array2D.make((3, 2), 1)
    let result = array2D->Array2D.reduceWithIndex(0, (acc, x, (xi, yi)) => acc + xi + yi)
    expect(result)->toEqual(9)
  })
})

describe("reduceWithIndexU", () => {
  test("should correctly reduce a 2D array to a single value with index", () => {
    let array2D = Array2D.make((3, 2), 1)
    let result = array2D->Array2D.reduceWithIndexU(0, (. acc, x, _) => acc + x)
    expect(result)->toEqual(6)
  })

  test("should return the initial value for an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 0)
    let result = array2D->Array2D.reduceWithIndexU(10, (. acc, x, _) => acc + x)
    expect(result)->toEqual(10)
  })

  test("should correctly use the index in the reduction function", () => {
    let array2D = Array2D.make((3, 2), 1)
    let result = array2D->Array2D.reduceWithIndexU(0, (. acc, x, (xi, yi)) => acc + xi + yi)
    expect(result)->toEqual(9)
  })
})

describe("flatten", () => {
  test("should flatten a 2D array into a 1D array", () => {
    let array2D = Array2D.make((3, 2), 1)
    let result = array2D->Array2D.flatten
    expect(result)->toEqual([1, 1, 1, 1, 1, 1])
  })

  test("should return an empty array when given an empty 2D array", () => {
    let array2D = Array2D.make((0, 0), 0)
    let result = array2D->Array2D.flatten
    expect(result)->toEqual([])
  })

  test("should flatten a 2D array with different values", () => {
    let array2D = [[1, 2, 3], [4, 5, 6]]
    let result = array2D->Array2D.flatten
    expect(result)->toEqual([1, 2, 3, 4, 5, 6])
  })
})

describe("crop", () => {
  let array2D = [[355, 907, 707], [404, 559, 514], [320, 891, 982], [744, 97, 876]]

  test("should correctly crop a 2D array (1)", () => {
    let result = array2D->Array2D.crop((1, 1), ~len_x=2, ~len_y=2)
    expect(result)->toEqual([[559, 514], [891, 982]])
  })

  test("should correctly crop a 2D array (2)", () => {
    let result = array2D->Array2D.crop((2, 1), ~len_x=1, ~len_y=3)
    expect(result)->toEqual([[514], [982], [876]])
  })

  test("should correctly crop a 2D array (3)", () => {
    let result = array2D->Array2D.crop((0, 2), ~len_x=1, ~len_y=2)
    expect(result)->toEqual([[320], [744]])
  })

  test("should return an empty array when cropping outside the array", () => {
    let result = array2D->Array2D.crop((5, 5), ~len_x=3, ~len_y=3)
    expect(result)->toEqual([])
  })

  test("should return a smaller array when cropping at the edge of the array (1)", () => {
    let result = array2D->Array2D.crop((2, 3), ~len_x=3, ~len_y=3)
    expect(result)->toEqual([[876]])
  })

  test("should return a smaller array when cropping at the edge of the array (2)", () => {
    let result = array2D->Array2D.crop((1, 2), ~len_x=10, ~len_y=20)
    expect(result)->toEqual([[891, 982], [97, 876]])
  })

  test("should return the correct array when cropping with zero length", () => {
    let result = array2D->Array2D.crop((1, 1), ~len_x=0, ~len_y=0)
    expect(result)->toEqual([])
  })
})

describe("eq", () => {
  test("should return true for identical 2D arrays", () => {
    let array2D1 = Array2D.make((3, 2), 1)
    let array2D2 = Array2D.make((3, 2), 1)
    expect(Array2D.eq(array2D1, array2D2))->toBe(true)
  })

  test("should return false for 2D arrays with different dimensions", () => {
    let array2D1 = Array2D.make((3, 2), 1)
    let array2D2 = Array2D.make((2, 2), 1)
    expect(Array2D.eq(array2D1, array2D2))->toBe(false)
  })

  test("should return false for 2D arrays with different values", () => {
    let array2D1 = Array2D.make((3, 2), 1)
    let array2D2 = Array2D.make((3, 2), 2)
    expect(Array2D.eq(array2D1, array2D2))->toBe(false)
  })

  test("should return true for empty 2D arrays", () => {
    let array2D1 = Array2D.make((0, 0), 0)
    let array2D2 = Array2D.make((0, 0), 0)
    expect(Array2D.eq(array2D1, array2D2))->toBe(true)
  })
})

///
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
  //  let a = Array2D.make((3, 4), -1)
  //  let _ = [
  //    a->Array2D.set((0, 0), 355),
  //    a->Array2D.set((1, 0), 907),
  //    a->Array2D.set((2, 0), 707),
  //    a->Array2D.set((0, 1), 404),
  //    a->Array2D.set((1, 1), 559),
  //    a->Array2D.set((2, 1), 514),
  //    a->Array2D.set((0, 2), 320),
  //    a->Array2D.set((1, 2), 891),
  //    a->Array2D.set((2, 2), 982),
  //    a->Array2D.set((0, 3), 744),
  //    a->Array2D.set((1, 3), 97),
  //    a->Array2D.set((2, 3), 876),
  //  ]

  let a = [[355, 907, 707], [404, 559, 514], [320, 891, 982], [744, 97, 876]]

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
