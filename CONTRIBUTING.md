# Contributing Guide

Thank you for considering a contribution to the Quant Research & Economic Intelligence Platform.

## Development Setup

```bash
git clone https://github.com/yourusername/quant-research-platform.git
cd quant-research-platform
Rscript -e "renv::restore()"
```

## Code Standards

- All exported functions must have Roxygen2 documentation
- All new functions must be accompanied by at least one `testthat` unit test
- Run `lintr::lint_dir("R/")` before submitting — zero linting errors required
- Use `snake_case` for all variable and function names
- Maximum line length: 120 characters
- No `T`/`F` abbreviations — use `TRUE`/`FALSE`

## Pull Request Process

1. Branch from `develop`: `git checkout -b feat/your-feature develop`
2. Write code + tests
3. Run `testthat::test_dir("tests/testthat/")` — all must pass
4. Run `lintr::lint_dir("R/")` — clean
5. Update documentation if public API changed
6. Submit PR to `develop` with a clear description of changes and motivation
7. At least one reviewer approval required before merge

## Commit Convention

Use Conventional Commits:

```
feat(module): short description
fix(module): what was broken and why
test(module): what is being tested
docs(module): what documentation changed
refactor(module): what changed and why
ci: CI pipeline changes
```

## Statistical Correctness

For statistical contributions, include:
- Mathematical derivation in comments
- Unit tests with synthetic data where the true parameter is known
- Discussion of assumptions and limitations

## Questions?

Open a GitHub Issue with the `question` label.
