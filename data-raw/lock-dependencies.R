# Regenerate renv.lock: the exact package versions the project is checked with.
#
# DESCRIPTION declares *which* packages (Imports, Suggests, and the toolchain
# in Config/Needs/dev); renv.lock pins *which versions*, plus the R version
# and the repository. The project is not an renv project -- there is no
# .Rprofile and no private library -- so the lockfile is read only when a
# machine is deliberately brought in line with it:
#
#   renv::restore(lockfile = "renv.lock", prompt = FALSE)
#
# Run from the package root, with the library you have just checked
# (`make check`, 0/0/0) on .libPaths():
#
#   make lock
#
# renv's own explicit snapshot records Suggests (dev = TRUE) but not
# Config/Needs/dev, so the package list is built here from DESCRIPTION and
# passed explicitly. Every package must be installed: a lockfile that silently
# omits one is worse than no lockfile.

declared_packages <- function(desc, fields) {
  fields[fields %in% colnames(desc)] |>
    lapply(function(field) strsplit(desc[1, field], ",")[[1]]) |>
    unlist() |>
    (\(x) trimws(sub("[(].*$", "", x)))() |>
    (\(x) x[nzchar(x) & x != "R"])() |>
    unique() |>
    sort()
}

if (!file.exists("DESCRIPTION")) {
  stop("No DESCRIPTION in ", getwd(), ". Run this from the package root ",
       "(make lock).", call. = FALSE)
}

desc <- read.dcf("DESCRIPTION")
packages <- declared_packages(
  desc, c("Depends", "Imports", "LinkingTo", "Suggests", "Config/Needs/dev")
)

missing <- packages[!vapply(packages, requireNamespace, logical(1),
                            quietly = TRUE)]
if (length(missing)) {
  stop("Declared in DESCRIPTION but not installed: ",
       paste(missing, collapse = ", "), ". Install them, run make check, ",
       "then make lock again.", call. = FALSE)
}

renv::snapshot(
  project = ".",
  lockfile = "renv.lock",
  packages = packages,
  library = .libPaths(),
  prompt = FALSE
)
