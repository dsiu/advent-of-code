open Jest
open Expect

//module Stack = Stack_List_Float
module S = Stack.StackFloat

describe("Stack Float", () => {
  let push_tests = list{
    (S.start->S.push(79.0), S.Contents(list{79.0})),
    (S.start->S.push(724.09)->S.push(433.0), S.Contents(list{433.0, 724.09})),
  }

  testAll("push", push_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let pop_tests = list{
    (S.start->S.one->S.three->S.four->S.pop, (4.0, S.Contents(list{3.0, 1.0}))),
    (S.start->S.five->S.two->S.pop, (2.0, S.Contents(list{5.0}))),
  }

  testAll("pop", pop_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let core_tests = list{
    (
      S.start->S.one->S.five->S.two->S.binary((x, y) => x -. y +. 944.0, _),
      S.Contents(list{947.0, 1.0}),
    ),
    (S.start->S.three->S.four->S.unary(x => x *. 522.0, _), S.Contents(list{2088.0, 3.0})),
    (S.start->S.five->S.four->S.dup, S.Contents(list{4.0, 4.0, 5.0})),
    (S.start->S.five->S.three->S.dup->S.mul, S.Contents(list{9.0, 5.0})),
    (S.start->S.two->S.dup->S.mul, S.Contents(list{4.0})),
    (S.start->S.five->S.three->S.swap->S.dup->S.add, S.Contents(list{10.0, 3.0})),
    (S.start->S.four->S.three->S.drop->S.dup->S.add, S.Contents(list{8.0})),
  }

  testAll("core", core_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let math_tests = list{
    (S.start->S.three->S.two->S.div, S.Contents(list{1.5})),
    (S.start->S.two->S.five->S.sub, S.Contents(list{-3.0})),
    (S.start->S.one->S.two->S.add->S.three->S.sub, S.Contents(list{0.0})),
    (S.start->S.three->S.neg, S.Contents(list{-3.0})),
    (S.start->S.two, S.Contents(list{2.0})),
    (S.start->S.two->S.dup, S.Contents(list{2.0, 2.0})),
    (S.start->S.three->S.dup->S.mul, S.Contents(list{9.0})),
    (S.start->S.three->S.five->S.square, S.Contents(list{25.0, 3.0})),
    (S.start->S.one->S.four->S.cube, S.Contents(list{64.0, 1.0})),
    (S.start->S.three->S.square->S.sum_numbers_upto, S.Contents(list{45.0})),
  }

  testAll("math", math_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })
})
