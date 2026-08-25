# Contributing to PhysioExperiment

Thank you for your interest in contributing to PhysioExperiment.

## Development Setup

```bash
git clone https://github.com/x-biosignal/PhysioExperiment.git
cd PhysioExperiment

# Install R dependencies
Rscript -e "remotes::install_deps(dependencies = TRUE)"

# Load package for development
Rscript -e "devtools::load_all()"

# GUI development (optional)
cd inst/gui
npm ci
npm run dev
```

## Workflow

1. Create a branch: `git checkout -b feat/your-feature`
2. Make changes following the conventions below
3. Run tests: `Rscript -e "devtools::test()"`
4. Run check: `R CMD check .`
5. Submit a pull request to `main`

## Coding Conventions

- **R style**: Follow existing patterns in the codebase
- **S4 classes**: Use `setClass()`, `setMethod()`, `setGeneric()`
- **Function naming**: `verbNoun()` — e.g., `filterSignals()`, `readEDF()`
- **File naming**: Prefix-based — `io-`, `ops-`, `stats-`, `vis-`, `db-`, `utils-`
- **Documentation**: Roxygen2 with `@param`, `@return`, `@export`, `@examples`
- **Validation**: `stopifnot()` for class checks, informative `stop()` messages
- **Tests**: `tests/testthat/test-{feature}.R` using `make_pe_2d()` / `make_pe_3d()`

## Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/):

- `feat:` — new feature
- `fix:` — bug fix
- `docs:` — documentation only
- `test:` — adding or fixing tests
- `refactor:` — code change that neither fixes a bug nor adds a feature

## Adding a New Feature

1. Create or edit the appropriate `R/{prefix}-{feature}.R` file
2. Add roxygen2 documentation with `@export`
3. Run `Rscript -e "roxygen2::roxygenise()"` to update NAMESPACE and man/
4. Add the file to the `Collate` field in `DESCRIPTION`
5. Write tests in `tests/testthat/test-{feature}.R`
6. Run `R CMD check .`

## Reporting Issues

Please use [GitHub Issues](https://github.com/x-biosignal/PhysioExperiment/issues) with:
- A minimal reproducible example
- Output of `sessionInfo()`
- Expected vs actual behavior
