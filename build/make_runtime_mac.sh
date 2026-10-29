#!/bin/bash
# build/make_runtime_mac.sh  --  Makes ovid-runtime-mac-arm64: a portable R (the CRAN R.framework installed on this Mac) with
# every package build/renv.lock pins, that runs from any folder. "Open ovid.command" uses it when it sits beside it as runtime/.
#
#   bash build/make_runtime_mac.sh            (from the Ovid_Apps folder; Apple silicon, CRAN R 4.6 in /Library/Frameworks)
#   bash build/make_runtime_mac.sh --install  (also copies it to runtime/ beside "Open ovid.command")
#
# How: R's own files are copied (no docs, tests or debug symbols); the library holds R's base and recommended packages plus
# the lockfile's packages, copied from this R's library; every compiled file's absolute R.framework paths are rewritten to
# @executable_path (so they resolve from wherever the folder is) and re-signed ad hoc; bin/R finds its home from where it is.
# Output in build/out/ (not in git): the folder and a .tar.gz for a release.
set -euo pipefail
cd "$(dirname "$0")/.."
ROOT="$(pwd)"
FW=/Library/Frameworks/R.framework/Versions/Current/Resources
FWREAL="$(cd "$FW" && pwd -P)"                                  # .../Versions/4.6/Resources
NAME=ovid-runtime-mac-arm64
OUT="$ROOT/build/out/$NAME"
R="$OUT/R"
[ "$(uname -m)" = "arm64" ] || { echo "This builds the Apple silicon runtime; run it on an arm64 Mac." >&2; exit 1; }
[ -x "$FW/bin/exec/R" ] || { echo "No CRAN R.framework in /Library/Frameworks." >&2; exit 1; }

echo "1/6 Copying R from $FWREAL"
rm -rf "$OUT"; mkdir -p "$R"
rsync -a --exclude 'library/' --exclude 'doc/' --exclude 'tests/' --exclude '*.dSYM' "$FWREAL/" "$R/"

echo "2/6 Copying the packages (R's own, and those build/renv.lock pins)"
mkdir -p "$R/library"
RLIB="$R/library" "$FW/bin/Rscript" -e '
lock <- jsonlite::read_json("build/renv.lock")
own <- rownames(installed.packages(lib.loc = .Library, priority = c("base", "recommended")))
want <- unique(c(own, names(lock$Packages)))
bad <- character()
for (p in want) { d <- system.file(package = p); if (!nzchar(d)) { bad <- c(bad, paste(p, "(not installed)")); next }
  v <- lock$Packages[[p]]$Version
  if (!is.null(v) && packageVersion(p) != package_version(v)) bad <- c(bad, sprintf("%s %s (lockfile %s)", p, packageVersion(p), v))
  file.copy(d, Sys.getenv("RLIB"), recursive = TRUE) }
if (length(bad)) stop("Not as the lockfile says: ", paste(bad, collapse = "; "), ". Refresh it: Rscript build/make_lockfile.R")
cat("  ", length(want), "packages\n")' 2>&1 | sed 's/^/  /'

echo "3/6 bin/R finds its home from where it is"
sed -i '' 's|^R_HOME_DIR=.*|R_HOME_DIR="$(cd "$(dirname "$0")/.." \&\& pwd -P)"|' "$R/bin/R"
sed -i '' "s|$FWREAL|\${R_HOME}|g; s|/Library/Frameworks/R.framework/Resources|\${R_HOME}|g" "$R/etc/Renviron"

echo "4/6 Rewriting R.framework paths in compiled files to @executable_path"
n=0
while IFS= read -r -d '' f; do
  file -b "$f" | grep -q 'Mach-O' || continue
  changed=0
  for old in $(otool -L "$f" | tail -n +2 | awk '{print $1}' | grep '^/Library/Frameworks/R.framework/' || true); do
    rel="${old#/Library/Frameworks/R.framework/Versions/*/Resources/}"; rel="${rel#/Library/Frameworks/R.framework/Resources/}"
    install_name_tool -change "$old" "@executable_path/../../$rel" "$f" 2>/dev/null; changed=1
  done
  id=$(otool -D "$f" | tail -n +2 | head -1)
  if [[ "$id" == /Library/Frameworks/R.framework/* ]]; then
    rel="${id#/Library/Frameworks/R.framework/Versions/*/Resources/}"; install_name_tool -id "@executable_path/../../$rel" "$f" 2>/dev/null; changed=1
  fi
  if [ $changed = 1 ]; then codesign --force -s - "$f" 2>/dev/null; n=$((n + 1)); fi
done < <(find "$R" -type f \( -name '*.so' -o -name '*.dylib' -o -perm -u+x \) -print0)
echo "  $n files rewritten"
left=$(find "$R" -type f \( -name '*.so' -o -name '*.dylib' \) -exec otool -L {} \; 2>/dev/null | grep -c '/Library/Frameworks/R.framework' || true)
[ "$left" = 0 ] || { echo "  $left references to /Library/Frameworks/R.framework remain" >&2; exit 1; }

echo "5/6 Checking it runs on its own (no user or site library, R.framework out of reach)"
RHOME="$R" R_LIBS_USER="$OUT/none" R_LIBS_SITE="$OUT/none" "$R/bin/Rscript" -e '
stopifnot(normalizePath(R.home()) == normalizePath(Sys.getenv("RHOME")), length(.libPaths()) == 1)
for (p in c("shiny", "data.table", "ggplot2", "DT", "plotly", "processx", "callr", "ncdf4", "readxl")) suppressPackageStartupMessages(library(p, character.only = TRUE))
cat("  R", as.character(getRversion()), "from", R.home(), "with", nrow(installed.packages()), "packages\n")'

echo "6/6 Packing"
( cd "$ROOT/build/out" && tar -czf "$NAME.tar.gz" "$NAME" )
du -sh "$OUT" "$ROOT/build/out/$NAME.tar.gz" | sed 's/^/  /'
if [ "${1:-}" = "--install" ]; then rm -rf "$ROOT/runtime"; cp -R "$OUT" "$ROOT/runtime"; echo "  installed as runtime/"; fi
