# Contributing to ovid

How code in this repository is laid out and written. These follow common practice in well-used R and Shiny projects
(the tidyverse style guide, golem's app layout, Mastering Shiny, lintr's defaults, rOpenSci). Older code is brought in
line as each file is changed, not all at once.

## Structure

1. **app.R only wires the app together.** Helpers live in the app's `pages/` folder; aim for a few hundred lines.
2. **One file per page, named by kind**, in `<APP>/pages/`:
   - `mod_<page>.R` for a page that is a Shiny module (UI and server together);
   - `fct_<topic>.R` for logic with no Shiny in it;
   - `utils_<topic>.R` for small helpers.

   The data pipeline stays in `R/`.
3. **Pages become Shiny modules** one at a time, each checked in the app (GS's Archive is the first,
   `GS/pages/mod_archive.R`). Whatever a module needs from the rest of the app is passed in. Anything that acts on the
   whole app from inside a module (switching page, updating another page's input) names the app's session.
4. **Keep logic out of reactive code.** Calculations go in plain functions (`fct_` or `R/`) that a test can call without
   opening the app (for example `archive_filter()`).
5. **No CSS or JavaScript inside R strings.** They go in `<APP>/www/` or `core/www/`.
6. **Shared code goes to core** once agreed. Core is sourced, not a package, for now.

## Readability

7. **Formatting is automatic.** Use the Air formatter (or styler) on a file when you change it; settings are in
   `air.toml`. Whole apps are never reformatted in one go.
8. **Lines of 120 characters or fewer** (set in `.lintr`).
9. **One statement per line.** No `;` to chain statements.
10. **snake_case names**: nouns for values, verbs for functions.
11. **Keep functions small enough to follow.** Split one that does several jobs.
12. **Every file starts with 1 to 3 comment lines** saying what it is. Comments say why, not what. Every formula cites
    its source (author, year).

`Rscript lint_changed.R` lists style problems in the R files you have changed (it never fails the tests).

## Reliability

13. **Errors are handled, not hidden.** In Shiny: `req()` for missing input, `validate(need())` for problems the user
    can fix, `tryCatch` with a notice that says what failed and what to do. A `try()` or `suppressWarnings()` that
    hides a problem needs a comment saying why it is safe.
14. **Packages come from build/renv.lock.** Nothing is installed while an app starts.
15. **No data-specific values in code.** They go in the project's set-up tables.

## Tests and commits

16. **Every fix or feature adds a test.** Keep each app's `tests/run_tests.R`; new tests may use testthat. Pages are
    checked in a real browser by `core/uitest.R`.
17. **`Rscript run_all_tests.R`** runs every suite; all must pass before a push.
18. **Each change gets a CHANGELOG line** in the app it touches.
19. **Commits are small and about one thing.**
20. **The launcher** (`core/launcher_app`, opened by `Open ovid.command`) only starts, shows and stops the apps; its logic is
    in `logic.R` and tested by `core/tests/run_tests.R`. A new package an app uses goes in `build/renv.lock`
    (`Rscript build/make_lockfile.R`) and then into the runtimes (`bash build/make_runtime_mac.sh`, `build\make_runtime_win.ps1`,
    with `build/snapshot.txt` moved to a date whose binaries match the new lockfile).
