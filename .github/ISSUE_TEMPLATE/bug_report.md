---
name: Bug Report
about: Report a reproducible bug in the platform
title: '[BUG] '
labels: bug
assignees: ''
---

## Bug Description
A clear, concise description of what the bug is.

## Reproduction Steps

```r
# Minimal reproducible example
library(here)
source(here("R", "utilities", "config.R"))
# ... code that triggers the bug
```

## Expected Behaviour
What you expected to happen.

## Actual Behaviour
What actually happened (include full error message and traceback).

## Environment
- R version: `R.version.string`
- Platform: (Windows/macOS/Linux)
- renv.lock hash: (run `tools::md5sum("renv.lock")`)
- Module affected: (e.g. `R/simulations/monte_carlo.R`)

## Additional Context
Any additional context, screenshots, or data samples.
