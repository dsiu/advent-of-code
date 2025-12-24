open Utils
  let log = Console.log
  let log2 = Console.log2

  // Type aliases for clarity
  type direction = string
  type steps = int
  type position = int
  type instruction = (direction, steps)
  type rotationCount = int

  // Constants
  let startPosition = 50
  let trackLength = 100

  // Parse input into instructions
  let parse: string => array<instruction> = data =>
    data
    ->splitNewline
    ->Array.map(String.trim)
    ->Array.map(line => {
      let chars = line->Array.fromString
      (
        chars[0]->Option.getOrThrow,
        chars->Array.sliceToEnd(~start=1)->Array.join("")->Int.fromString->Option.getOrThrow,
      )
    })

  // Mathematical modulo that handles negative numbers correctly
  let math_mod = (n, m) => (n % m + m) % m

  // Calculate new position based on direction and steps
  let calculateNewPosition = (current: position, dir: direction, steps: steps): position => {
    switch dir {
    | "L" => current - steps
    | "R" => current + steps
    | _ => raise(Failure("Invalid direction"))
    }
  }

  // Normalize position to be within track bounds
  let normalizePosition = (pos: position): position => {
    let normalized = math_mod(pos, trackLength)
    normalized < 0 ? normalized + trackLength : normalized
  }

  // Calculate total rotations including boundary corrections
  let calculateRotations = (nextPosition: position, currentPosition: position): rotationCount => {
    let rotations = Math.Int.abs(nextPosition / trackLength)
    let correction = nextPosition <= 0 ? 1 : 0
    let adjustedCorrection = nextPosition < 0 && currentPosition == 0 ? correction - 1 : correction
    rotations + adjustedCorrection
  }

  // Count total rotations around the track
  let solvePart2 = data => {
    data
    ->parse
    ->Array.reduce((startPosition, 0), ((currentPosition, count), (dir, steps)) => {
      let nextPosition = calculateNewPosition(currentPosition, dir, steps)
      let rotations = calculateRotations(nextPosition, currentPosition)
      let normalizedPosition = math_mod(nextPosition, trackLength)
      (normalizedPosition, count + rotations)
    })
    ->Pair.second
  }

  // Count how many times the position crosses zero
  let solvePart1 = data => {
    data
    ->parse
    ->Array.reduce((startPosition, []), ((currentPosition, result), (dir, steps)) => {
      let nextPosition = calculateNewPosition(currentPosition, dir, steps)
      let normalizedPosition = normalizePosition(nextPosition)

      result->Array.push(normalizedPosition)
      (normalizedPosition, result)
    })
    ->Pair.second
    ->Array.filter(x => x == 0)
    ->Array.length
  }
