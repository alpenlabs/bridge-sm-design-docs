# Bridge State Machines Design Doc

This repository contains the specification for the State Machines in the Bridge. This implementation currently refers to the Mainnet V1 Release. For comments/discussions/diagrams, please refer to this [Notion Document](https://www.notion.so/Bridge-State-Machine-2af901ba000f80698189dd2e0a18d692?source=copy_link).
The specification is written in Haskell and provides a general sense of what each state holds and how the state machines will work. The canonical implementation will be written in Rust in the [strata-bridge](https://github.com/alpenlabs/strata-bridge.git) repo.

## Development

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) 9.12+ (required for base 4.21.0.0 and GHC2024)
- [Cabal](https://www.haskell.org/cabal/) 3.0+
- [Just](https://github.com/casey/just) - Command runner for development tasks

### Quick Start

Build the project:
```bash
just build
```

Run all CI checks locally:
```bash
just ci
```

Verify you're ready for a PR (checks git tree is clean and runs CI):
```bash
just pr
```

### Available Commands

The project uses [Just](https://github.com/casey/just) for task automation. Run `just` or `just --list` to see all available commands:

- `just build` - Build the project
- `just test` - Run the test suite
- `just clean` - Remove build artifacts
- `just lint` - Run HLint for code linting
- `just format` - Format code with Fourmolu (modifies files)
- `just format-check` - Check formatting without modifying files
- `just repl` - Open a Cabal REPL
- `just docs` - Generate Haddock documentation
- `just ci` - Run all CI checks (format-check, lint, build, test)
- `just pr` - Verify git tree is clean and run all CI checks (ready for PR)
- `just watch` - Watch for changes and rebuild (requires `fd` and `entr`)
- `just setup` - Install development tools (HLint and Fourmolu)

### Code Quality

The project enforces code quality through:

- **HLint** - Haskell linting (configured in `.hlint.yaml`)
- **Fourmolu** - Code formatting (configured in `fourmolu.yaml`)
- **Compilation checks** - All code must compile with `-Wall`

Before submitting a PR, run `just pr` to verify your git tree is clean and all checks pass locally.
