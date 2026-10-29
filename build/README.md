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

## The runtimes

A runtime is R and every package above, runnable from any folder with no other R or library. Beside the launcher as
`runtime/`, `Open ovid.command` (Mac) or `Open ovid.bat` (Windows) uses it. Not committed: built in `build/out/` and
uploaded to a release.

| Runtime | Made by, on | Size (packed) |
| --- | --- | --- |
| `ovid-runtime-mac-arm64` | `bash build/make_runtime_mac.sh`, Apple silicon | 437 MB (192 MB `.tar.gz`) |
| `ovid-runtime-mac-x86_64` | `bash build/make_runtime_mac.sh --arch x86_64`, Intel, or Apple silicon under Rosetta | 370 MB (176 MB `.tar.gz`) |
| `ovid-runtime-win-x64` | `powershell -ExecutionPolicy Bypass -File build\make_runtime_win.ps1`, Windows 10 or later | not yet built (CI) |

Every runtime has the lockfile's exact versions. Packages come as binaries from the dated snapshot in `build/snapshot.txt`
(Posit Package Manager's CRAN as it was then, whose binaries match the lockfile); what the snapshot lacks (ncdf4 for macOS)
comes from CRAN and must match too. `build/runtime_packages.R` installs and checks them for both scripts, and writes
`versions.txt` beside the runtime. After a lockfile refresh, move the snapshot date to one that matches; `--allow-newer`
(`-AllowNewer`) accepts newer versions and lists them.

- **Mac**: R is copied from `/Library/Frameworks/R.framework` when that is the right arch and version (with its packages),
  else from CRAN's installer for that arch, checked as notarized and unpacked, never installed. No docs, tests or debug
  symbols; the compiled files' R.framework paths are rewritten to `@executable_path` and re-signed ad hoc.
- **Windows**: CRAN's R installer runs silently into the output folder (no Start menu, registry entry or file association).

`build/check_runtime.R` is the proof: from a clean copy holding the runtime, it starts the five apps the way the launcher
does, checks each page answers HTTP 200 and stops them. `.github/workflows/runtimes.yml` (by hand, or on a `v*` tag) builds
all three on their own systems (macos-14, macos-15-intel, windows-latest), runs that check and every test suite with the runtime,
and keeps the runtimes as artifacts.
