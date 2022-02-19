open Jest2
module Stack = Stack_Array

describe("Stack", () => {
  let push_tests = [([]->Stack.push(79), [79]), ([614, 154]->Stack.push(728), [728, 614, 154])]

  testEach2("push", push_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  let pop_tests = [
    ([]->Stack.pop, (None, [])),
    ([681]->Stack.pop, (Some(681), [])),
    ([404, 240, 942]->Stack.pop, (Some(404), [240, 942])),
  ]

  testEach2("pop", pop_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  let peek_tests = [
    ([]->Stack.peek, None),
    ([269]->Stack.peek, Some(269)),
    ([917, 815, 924]->Stack.peek, Some(917)),
  ]

  testEach2("peek", peek_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })
})
