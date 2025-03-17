# ReScript Project Commands and Guidelines

## Build Commands
- Build all: `yarn build` or `rescript`
- Watch mode: `yarn watch`
- Clean build: `yarn clean`
- Run tests: `yarn test`
- Run specific test file: `yarn jest __tests__/FileName_Test.res.mjs`
- Run tests with specific pattern: `yarn jest -t "test pattern"`
- Reformat code: `yarn reformat`

## Code Style
- Use camelCase for variable and function names
- Use PascalCase for modules, types, and constructors
- Follow ReScript naming conventions (lowercase first letter for functions, uppercase for modules)
- Use pattern matching over if/else when possible
- Prefer explicit type annotations for function parameters and return values
- Handle errors with Result or Option types, not exceptions
- Use immutable data structures when possible
- Place interface files (.resi) beside implementation files (.res)

## Project Structure
- Tests should be named with suffix `_Test.res`
- Tests are in `__tests__/` directory
- Implementation code is in `src/` directory
- Use Jest's `open Jest` and `open Expect` for testing
- Follow functional programming patterns