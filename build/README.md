# build

`renv.lock` pins the R version and the exact version of every package the five ovid apps and the shared core use, with
everything those packages need, as installed on the Mac the apps are developed on. It is the base for a future bundled R and
launcher, and the one lockfile: the launchers (core/launcher.R) install from it on a first start and the tests on GitHub
(.github/workflows/tests.yml) restore it. renv is not activated in the apps: there is no `.Rprofile` or `renv/` library, and the apps load packages as before.

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

## The Mac runtime

`bash build/make_runtime_mac.sh` (from Ovid_Apps, on an Apple silicon Mac with CRAN R 4.6 installed) makes
`build/out/ovid-runtime-mac-arm64`: R and the packages above, runnable from any folder (about 437 MB, 192 MB as `.tar.gz`).
It copies R from `/Library/Frameworks/R.framework` (no docs, tests or debug symbols) and the packages from that R's library,
stops if a package's version differs from the lockfile, rewrites the compiled files' R.framework paths to `@executable_path`
and re-signs them, then checks that it runs with no other library. `--install` copies it to `runtime/` beside
`Open ovid.command`, which then uses it. The output is not committed; it is uploaded to a release.
