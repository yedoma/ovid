# lint_changed.R  --  Style check (CONTRIBUTING.md) of the R files changed in the working tree or in commits not yet
# pushed. Lists the problems by file and rule; it never fails, so older code is brought in line as it is changed.
#   Rscript lint_changed.R          the changed files
#   Rscript lint_changed.R CS/app.R files named here

if (!requireNamespace("lintr", quietly = TRUE)) {
  cat("lintr is not installed: install.packages(\"lintr\") to use this check.\n")
  quit(status = 0)
}
repo <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
})
files <- commandArgs(trailingOnly = TRUE)
if (!length(files)) {
  git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(repo), ...), stdout = TRUE, stderr = FALSE))
  files <- unique(c(git("diff", "--name-only", shQuote("@{upstream}...HEAD")), substring(git("status", "--porcelain"), 4)))
}
files <- files[grepl("\\.R$", files) & file.exists(file.path(repo, files))]
if (!length(files)) {
  cat("No changed R files.\n")
  quit(status = 0)
}
total <- 0L
for (f in files) {
  l <- as.data.frame(lintr::lint(file.path(repo, f), parse_settings = TRUE))
  total <- total + nrow(l)
  if (!nrow(l)) next
  by_rule <- sort(table(l$linter), decreasing = TRUE)
  cat(sprintf("%s: %d (%s)\n", f, nrow(l), paste(sprintf("%s %d", names(by_rule), by_rule), collapse = ", ")))
}
cat(sprintf("%d style note(s) in %d file(s). Fix those in the lines you changed; see CONTRIBUTING.md.\n", total, length(files)))
