#!/bin/bash
# Double-click to open Ovid WLR. Needs R (cran.r-project.org); packages install themselves on first run.
cd "$(dirname "$0")/WLR" || exit 1
R=$(command -v Rscript || echo /Library/Frameworks/R.framework/Resources/bin/Rscript)
"$R" launcher/run_app.R
