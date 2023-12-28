module type StackItem = {
  type t
  let one: t
  let two: t
  let three: t
  let four: t
  let five: t
  let add: (t, t) => t
  let mul: (t, t) => t
  let sub: (t, t) => t
  let div: (t, t) => t
  let neg: t => t
}

module MakeStack = (Item: StackItem) => {
  // ==============================================
  // Types
  // ==============================================

  type stack = Contents(list<Item.t>)

  // ==============================================
  // Stack primitives
  // ==============================================

  /// Push a value on the stack
  let push = (Contents(contents), x) => Contents(list{x, ...contents})

  /// Pop a value from the stack and return it
  /// and the new stack as a tuple
  let pop = (Contents(contents)) => {
    switch contents {
    | list{top, ...rest} => {
        let newStack = Contents(rest)
        (top, newStack)
      }

    | list{} => raise(Not_found)
    }
  }

  // ==============================================
  // Operator core
  // ==============================================

  // pop the top two elements
  // do a binary operation on them
  // push the result
  let binary = (mathFn, stack) => {
    let (y, stack') = stack->pop
    let (x, stack'') = stack'->pop
    let z = mathFn(x, y)
    stack''->push(z)
  }

  // pop the top element
  // do a unary operation on it
  // push the result
  let unary = (f, stack) => {
    let (x, stack') = stack->pop
    stack'->push(f(x))
  }

  // ==============================================
  // Other core
  // ==============================================

  /// Pop and show the top value on the stack
  let show = stack => {
    let (x, _) = stack->pop
    x->Js.log
    stack // keep going with same stack
  }

  let show2 = (stack, str) => {
    let (x, _) = stack->pop
    x->Js.log2(str)
    stack // keep going with same stack
  }

  /// Duplicate the top value on the stack
  let dup = stack => {
    let (x, _) = stack->pop
    stack->push(x)
  }

  /// Swap the top two values
  let swap = stack => {
    let (x, s) = stack->pop
    let (y, s') = s->pop
    s'->push(x)->push(y)
  }

  /// Drop the top value on the stack
  let drop = stack => {
    let (_, s) = stack->pop //pop the top of the stack
    s //return the rest
  }

  // ==============================================
  // Words based on primitives
  // ==============================================

  // Constants
  // -------------------------------
  let empty = Contents(list{})
  let start = empty

  // Numbers
  // -------------------------------
  let one = push(_, Item.one)
  let two = push(_, Item.two)
  let three = push(_, Item.three)
  let four = push(_, Item.four)
  let five = push(_, Item.five)

  // Math functions
  // -------------------------------
  let mul = binary(Item.mul, _)
  let add = binary(Item.add, _)
  let sub = binary(Item.sub, _)
  let div = binary(Item.div, _)

  let neg = unary(x => Item.neg(x), _)

  // ==============================================
  // Words based on composition
  // ==============================================

  let {compose, compose4} = module(Stdlib.Function)
  let square = compose(dup, mul)

  let cube = compose4(dup, dup, mul, mul)

  //  let sum_numbers_upto = [dup, one, add, mul, two, div]->composeN

  let sum_numbers_upto = compose(compose4(dup, one, add, mul), compose(two, div))
}

module IntOps = {
  type t = int
  let one = 1
  let two = 2
  let three = 3
  let four = 4
  let five = 5

  let add = (x, y) => x + y
  let mul = (x, y) => x * y
  let sub = (x, y) => x - y
  let div = (x, y) => x / y
  let neg = x => -x
}

module StackInt = MakeStack(IntOps)

module FloatOps = {
  type t = float
  let one = 1.0
  let two = 2.0
  let three = 3.0
  let four = 4.0
  let five = 5.0

  let add = (x, y) => x +. y
  let mul = (x, y) => x *. y
  let sub = (x, y) => x -. y
  let div = (x, y) => x /. y
  let neg = x => 0.0 -. x
}

module StackFloat = MakeStack(FloatOps)
