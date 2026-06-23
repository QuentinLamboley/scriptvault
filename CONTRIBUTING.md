# Contributing to ScriptVault

Thank you for contributing. ScriptVault is intended for researchers who need reliable local versioning and reproducibility records without mandatory Git, GitHub, cloud storage, telemetry, or accounts.

## Before opening an issue

- Search existing issues first.
- Never upload a `.scriptvault/` folder, private scripts, raw data, credentials, personal paths, or participant information.
- Reduce problems to a temporary project with synthetic files whenever possible.

## Development setup

```r
install.packages(c("devtools", "testthat", "roxygen2", "rcmdcheck"))
devtools::load_all()
devtools::test()
devtools::check()
```

The package intentionally uses base R plus a small set of transparent dependencies. Avoid adding dependencies unless they provide a clear, durable benefit for local scientific workflows.

## Pull requests

1. Create a focused branch.
2. Add or update tests in `tests/testthat/`.
3. Update `NEWS.md` for user-visible changes.
4. Run `devtools::document()` if roxygen comments or exports change.
5. Run `devtools::test()` and `devtools::check()`.
6. Explain any effect on local storage, restoration safety, encryption expectations, hashing, file discovery, or reproducibility metadata.

## Design rules

- **Local-first:** core features must work without a network connection.
- **Privacy-first:** no telemetry, project uploads, hidden network activity, or account requirement.
- **Safe restoration:** never overwrite project files without an explicit user action and a recovery path.
- **Auditable history:** operations that alter vault state should remain traceable.
- **Scientific clarity:** branch, tag, run, and artifact concepts should be understandable to users who do not use Git.

## Code style

Use two-space indentation, explicit namespaces for external packages (for example `DBI::dbExecute()`), descriptive error messages, and portable paths via `fs`.
