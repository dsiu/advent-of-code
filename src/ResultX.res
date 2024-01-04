// Result for handling async promise
//

let asyncFlatMap = (result, fn) => {
  switch result {
  | Error(_) as error => error->Promise.resolve
  | Ok(value) => fn(value)
  }
}

//let asyncFlatMap = (
//  result: result<'value, 'error>,
//  fn: 'value => Promise.t<result<'newValue, 'error>>,
//): Promise.t<result<'newValue, 'error>> => {
//  switch result {
//  | Error(_) as error => error->Promise.resolve
//  | Ok(value) => fn(value)
//  }
//}
