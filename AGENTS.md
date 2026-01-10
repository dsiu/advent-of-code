# ReScript Project Commands and Guidelines

## Documentation References

**IMPORTANT**: When working with code in this repository, always refer to these official documentation sources:

### ReScript Language Reference
- **Language Manual**: https://rescript-lang.org/docs/manual/introduction
- **LLM-Full.txt**: https://rescript-lang.org/llms/manual/llm-full.txt
- **Use for**:
  - ReScript syntax and language features
  - Standard library APIs
  - Type system details
  - External bindings and interop patterns
  - Best practices and idioms
- Ensure suggestions match ReScript v12

## Build Commands

- Build all: `rescript` or `yarn build`
- Watch mode: `rescript watch` or `yarn watch`
- Clean build: `rescript clean` or `yarn clean`
- Run all tests: `yarn test`
- Run single test file: `yarn jest __tests__/FileName_Test.res.mjs`
- Run tests with specific pattern: `yarn jest -t "test pattern"`
- Reformat code: `./scripts/reformat.sh` or `yarn reformat`
- Build a single package: `yarn workspace <pkg> build`

## Code Style

- **ReScript v12** with ES modules (`.res.mjs` output)
- camelCase for functions/vars; PascalCase for modules/types/constructors
- Prefer pattern matching over if/else
- Use `Option`/`Result`; avoid exceptions
- Immutable by default; avoid mutation
- Provide explicit type annotations for public functions and parameters
- Use modern libraries: `StdlibFp`, `Tablecloth`, workspace packages
- **Never use legacy `Belt` or `Js` modules**
- Use `async/await` syntax for promises

## Project Structure

- Implementation code in `src/` directory
- Tests in `__tests__/` directory, named with `_Test.res` suffix
- Interface files (`.resi`) required beside implementation files (`.res`) for proper module signatures
- Module type signatures and functors are common patterns
- Jest config: test match pattern is `**/*_Test.res.(js|ts|jsx|tsx|mjs)`
- Use `open Jest` and `open Expect` for testing
- Follow functional programming patterns
- Modular architecture with functor-based implementations

## Advent of Code Directory Structure

Each year and day follows a consistent pattern:

```
src/
├── <YEAR>/
│   ├── Day<N>/
│   │   ├── AOC<YEAR>_Day<N>.res          # Main solution logic (solvePart1, solvePart2 functions)
│   │   ├── AOC<YEAR>_Day<N>_Data.res     # Puzzle input data
│   │   ├── AOC<YEAR>_Day<N>_Data_Sample.res # Sample data for testing
│   │   ├── AOC<YEAR>_Day<N>_Test.res     # Jest tests (included in __tests__/ via symlink or copy)
│   │   └── AOC<YEAR>_Day<N>_Run.res      # Manual test runner (optional, for debugging)
│   └── DayX/                              # Template directory for next day
```

**File responsibilities:**
- **AOC<YEAR>_Day<N>.res**: Core logic with `solvePart1` and `solvePart2` functions, type aliases, and helper functions
- **AOC<YEAR>_Day<N>_Data.res**: Actual puzzle input as a string constant `data`
- **AOC<YEAR>_Day<N>_Data_Sample.res**: Sample/example input as a string constant `data`
- **AOC<YEAR>_Day<N>_Test.res**: Jest test suite with sample and full data test cases
- **AOC<YEAR>_Day<N>_Run.res**: Manual test execution (optional, for quick debugging)

**Testing pattern:**
- Test both sample data and full puzzle data
- Use `testAll` for parameterized edge case testing
- Each test has a single `expect` call with `toEqual` assertion
