# build

`renv.lock` pins the R version and the exact version of every package the five ovid apps and the shared core use, with
everything those packages need, as installed on the Mac the apps are developed on. It is the base for a future bundled R and
launcher. renv is not activated in the apps: there is no `.Rprofile` or `renv/` library, and the apps load packages as before.

**Refresh** (after installing or updating a package), from the Ovid_Apps folder:

```bash
Rscript build/make_lockfile.R
```

The list comes from each app's `launcher/run_app.R`, from `renv::dependencies()` over `core/` and the apps, and from the
packages used only when present. The core tests fail when a package the code uses is missing from the lockfile.

**Restore** on another machine (R of the version in the lockfile, and renv installed), from the Ovid_Apps folder:

```r
renv::restore(lockfile = "build/renv.lock")
```

All packages come from CRAN. `writexl` is optional (Excel writing in tests) and is pinned only when installed.
