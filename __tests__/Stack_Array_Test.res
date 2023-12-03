open Jest
open Expect

module Stack = Stack_Array

describe("Stack", () => {
  let push_tests = list{([]->Stack.push(79), [79]), ([614, 154]->Stack.push(728), [728, 614, 154])}

  testAll("push", push_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let pop_tests = list{
    ([]->Stack.pop, (None, [])),
    ([681]->Stack.pop, (Some(681), [])),
    ([404, 240, 942]->Stack.pop, (Some(404), [240, 942])),
  }

  testAll("pop", pop_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })

  let peek_tests = list{
    ([]->Stack.peek, None),
    ([269]->Stack.peek, Some(269)),
    ([917, 815, 924]->Stack.peek, Some(917)),
  }

  testAll("peek", peek_tests, ((result, expected)) => {
    expect(result)->toEqual(expected)
  })
})
