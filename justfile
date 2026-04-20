# Justfile for bridge-sm
# Run 'just --list' to see all available recipes

# Default recipe - show available commands
default:
    @just --list

# Check if HLint is installed
ensure-hlint:
    #!/usr/bin/env bash
    if ! command -v hlint &> /dev/null; then
        echo "Error: hlint is not installed."
        echo "Install it with: cabal install hlint"
        exit 1
    fi

# Check if Fourmolu is installed
ensure-fourmolu:
    #!/usr/bin/env bash
    if ! command -v fourmolu &> /dev/null; then
        echo "Error: fourmolu is not installed."
        echo "Install it with: cabal install fourmolu"
        exit 1
    fi

# Check if watch tools (fd and entr) are installed
ensure-watch-tools:
    #!/usr/bin/env bash
    if ! command -v fd &> /dev/null; then
        echo "Error: fd is not installed."
        echo "Install it with your package manager (e.g., 'brew install fd' on macOS)"
        exit 1
    fi
    if ! command -v entr &> /dev/null; then
        echo "Error: entr is not installed."
        echo "Install it with your package manager (e.g., 'brew install entr' on macOS)"
        exit 1
    fi

# Build the project
build:
    cabal build all

# Run tests
test:
    cabal test all --test-show-details=streaming

# Clean build artifacts
clean:
    cabal clean
    rm -rf dist-newstyle/

# Run HLint on source and test files
lint: ensure-hlint
    hlint src/ test/

# Format code with Fourmolu (modifies files in place)
format: ensure-fourmolu
    fourmolu --mode inplace src/ test/

# Check formatting without modifying files (CI-friendly)
format-check: ensure-fourmolu
    fourmolu --mode check src/ test/

# Open Cabal REPL
repl:
    cabal repl bridge-sm

# Generate Haddock documentation
docs:
    cabal haddock --haddock-html --haddock-quickjump --haddock-hyperlink-source

# Run all CI checks locally (format-check, lint, build, test)
ci: format-check lint build test

# Run CI checks and verify git tree is clean (ready for PR)
pr:
    #!/usr/bin/env bash
    set -euo pipefail
    if ! git diff-index --quiet HEAD --; then
        echo "Error: Git working tree is dirty. Please commit or stash your changes first."
        git status --short
        exit 1
    fi
    echo "Git tree is clean. Running CI checks..."
    just ci

# Watch for changes and rebuild (requires 'fd' and 'entr')
watch: ensure-watch-tools
    fd -e hs | entr -c just build

# Install development tools (HLint and Fourmolu via Cabal)
setup:
    @echo "Installing HLint..."
    cabal install --overwrite-policy=always hlint
    @echo "Installing Fourmolu..."
    cabal install --overwrite-policy=always fourmolu
    @echo ""
    @echo "Note: 'just watch' also requires 'fd' and 'entr'"
    @echo "Install them with your package manager (e.g., 'brew install fd entr' on macOS)"

# Deep clean including Cabal cache
nuke:
    rm -rf dist-newstyle/ ~/.cabal/packages ~/.cabal/store
