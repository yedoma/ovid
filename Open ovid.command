#!/bin/bash
# Double-click to open the ovid launcher: one window that starts CS, WLR, GT, GS and RS (core/launcher_app).
# R: the portable runtime in runtime/ beside this file when there is one (build/make_runtime_mac.sh), else the R installed
# on this computer. With the installed R, the first start installs the packages build/renv.lock lists (once).
cd "$(dirname "$0")" || exit 1
ROOT="$(pwd)"
if [ -x "$ROOT/runtime/R/bin/Rscript" ]; then
  R="$ROOT/runtime/R/bin/Rscript"
  export RHOME="$ROOT/runtime/R" R_LIBS_USER="$ROOT/runtime/none" R_LIBS_SITE="$ROOT/runtime/none"   # only the runtime's own library
else
  R=$(command -v Rscript || echo /Library/Frameworks/R.framework/Resources/bin/Rscript)
fi
OVID_ROOT="$ROOT" "$R" --vanilla "$ROOT/core/launcher_app/start.R"
