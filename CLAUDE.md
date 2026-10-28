# Ovid Apps

R/Shiny apps CS, WLR, GT, GS, RS on a shared `core/` (sourced, not a package). Rules for how code is laid out and
written are in CONTRIBUTING.md; follow them for every change.

## Agents (in .claude/agents)
- Before editing anything in `core/`, run **core-impact** on the function or file and use its verdict to pick suites.
- After finishing a change, and always before a commit or push, run **test-runner**, **conventions-reviewer** and
  **docs-checker** (they are independent, so run them in parallel) and fix what they report or say why not.
- Every fix or feature needs a test and a CHANGELOG line in the app it touches (core changes also in the root
  CHANGELOG.md when several apps are affected).
