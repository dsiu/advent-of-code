// Result for handling async promise
//

let mapError = (result, fn) =>
  switch result {
  | Ok(_) as ok => ok
  | Error(error) => Error(fn(error))
  }

let fromOption = (option, error) =>
  switch option {
  | Some(value) => Ok(value)
  | None => Error(error)
  }

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
