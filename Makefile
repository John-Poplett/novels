# Development entry points for the novels package.
#
# `man/` is git-ignored, so every check must regenerate the Rd files first or
# R CMD check reports stale codoc mismatches that do not exist in the source.
# These targets do that for you; run `devtools::document()` by hand only when
# you want the Rd output without a check.

R := Rscript

.PHONY: help test document check check-full data clean

help:
	@echo "make test        fast test loop (pkgload + testthat)"
	@echo "make document    regenerate man/ and NAMESPACE from roxygen comments"
	@echo "make check       package check, vignettes not built (~37 min)"
	@echo "make check-full  release gate, vignettes built; must be 0/0/0"
	@echo "make data        rebuild data/novels.rda from the pinned archive"
	@echo "make clean       remove check artifacts and rendered docs"

test:
	$(R) -e 'pkgload::load_all(quiet = TRUE); testthat::test_dir("tests/testthat")'

document:
	$(R) -e 'roxygen2::roxygenise()'

check: document
	$(R) -e 'devtools::check(document = FALSE, args = c("--no-manual", "--no-build-vignettes"))'

check-full: document
	$(R) -e 'devtools::check(document = FALSE, args = c("--no-manual"))'

data:
	$(R) -e 'targets::tar_make()'

clean:
	rm -rf *.Rcheck novels_*.tar.gz inst/doc doc
