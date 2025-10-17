# Repository Guidelines

## Project Structure & Module Organization
R package sources live in `evidenceguide/R`, grouped by responsibility (auth, file dialog, upload, polling, pipeline, tidying, utils). Generated documentation will land in `evidenceguide/man`, while tests sit in `evidenceguide/tests/testthat` with fixtures under `fixtures/`. The `vignettes/quickstart.Rmd` file introduces the end-to-end workflow, and `.Rbuildignore` keeps non-package assets out of builds.

## Build, Test, and Development Commands
Use `R CMD build evidenceguide` to create a source tarball and `R CMD check evidenceguide_*.tar.gz` for full QA. During development, run unit tests via `Rscript -e "testthat::test_dir('evidenceguide/tests/testthat')"` and format documentation with `Rscript -e "devtools::document('evidenceguide')"` if you add roxygen comments.

## Coding Style & Naming Conventions
All user-facing functions are exported with the `eg_` prefix; internal helpers stay unexported. Follow tidyverse-style indentation (two spaces) and keep line width under 100 characters. Prefer explicit namespace calls (`package::fn()`) and provide short, high-signal comments only when logic is non-obvious. New modules should mirror the existing filename pattern (e.g., `pipeline.R`).

## Testing Guidelines
Tests rely on `testthat` edition 3 and local fixtures; add fixtures in JSON under `tests/testthat/fixtures`. Name tests `test-<topic>.R` and cover both success paths and error signalling. When mocking HTTP workflows, inject a custom request performer via `options(evidenceguide.http_perform = ...)` to keep tests hermetic. Aim to exercise new code paths with deterministic inputs before raising a PR.

## Commit & Pull Request Guidelines
Write commits in the imperative mood (`Add polling backoff helper`) and keep changesets focused per module. Pull requests should include: a concise summary, linked Evidence Guide issue (if any), affected commands, and screenshots or console snippets when UX changes occur. Flag any follow-up work or API assumptions in the PR description so reviewers have full context.

## Security & Configuration Tips
Never hard-code API keys; rely on `eg_set_api_key()` or `~/.Renviron`. Prefer staging endpoints via `eg_set_base_url()` when testing unreleased servers, and scrub fixtures of user data before committing.
