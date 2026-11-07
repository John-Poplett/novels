# Development entry points for the novels package.
#
# `man/` is git-ignored, so every check must regenerate the Rd files first or
# R CMD check reports stale codoc mismatches that do not exist in the source.
# These targets do that for you; run `devtools::document()` by hand only when
# you want the Rd output without a check.

R := Rscript

.PHONY: help test lint document check check-full data clean

# data/novels.rda is git-ignored and built by the pipeline, but three things
# need it before they can run: the tests load it, and roxygen loads the whole
# package (`LazyData: true`), so `document` and therefore every check needs it
# too. Without the file those steps die with `'novels' is not an exported
# object from 'namespace:novels'`, which points nowhere near the real cause.
# Stop first and say what to do. Deliberately not a dependency on `data`:
# silently downloading 71 MB mid-check is worse than one explicit command.
data/novels.rda:
	@echo "data/novels.rda is missing -- run 'make data' first."
	@echo "It builds the corpus from the pinned archive (NOVELS_ARCHIVE_URL)."
	@exit 1

help:
	@echo "make test        fast test loop (pkgload + testthat)"
	@echo "make lint        tracked text files are LF-only, no trailing blanks"
	@echo "make document    regenerate man/ and NAMESPACE from roxygen comments"
	@echo "make check       package check, vignettes built once (~7 min)"
	@echo "make check-full  release gate, vignettes re-built; must be 0/0/0"
	@echo "make data        rebuild data/novels.rda from the pinned archive"
	@echo "make clean       remove check artifacts and rendered docs"

test: data/novels.rda
	$(R) -e 'pkgload::load_all(quiet = TRUE); testthat::test_dir("tests/testthat")'

# R CMD check inspects neither line endings nor trailing blanks, and both have
# already slipped into this repo once (a CSV shipped CRLF, docs carried stray
# spaces). Tracked files only: build artifacts and the pipeline store are not
# ours to police. Pattern quoting matters -- unquoted, the shell expands the
# globs here and `git ls-files` loses the recursive match.
lint:
	@bad=0; \
	for f in $$(git ls-files '*.R' '*.Rmd' '*.md' '*.yaml' '*.yml' \
	    '*.csv' 'Makefile' 'DESCRIPTION' 'NAMESPACE' '.Rbuildignore' 'LICENSE'); do \
	  hits=$$(grep -nE '[[:blank:]]+$$' "$$f"); \
	  if [ -n "$$hits" ]; then \
	    echo "trailing whitespace: $$f"; \
	    echo "$$hits" | sed 's/^/    /'; \
	    bad=1; \
	  fi; \
	  cr=$$(tr -dc '\r' < "$$f" | wc -c | tr -d ' '); \
	  if [ "$$cr" != "0" ]; then \
	    echo "CRLF line endings: $$f"; \
	    bad=1; \
	  fi; \
	done; \
	if [ "$$bad" -ne 0 ]; then \
	  echo "lint: FAILED"; \
	  exit 1; \
	fi; \
	echo "lint: clean ($$(git ls-files '*.R' '*.Rmd' '*.md' '*.yaml' '*.yml' \
	    '*.csv' 'Makefile' 'DESCRIPTION' 'NAMESPACE' '.Rbuildignore' 'LICENSE' \
	    | wc -l | tr -d ' ') files)"

document: data/novels.rda
	$(R) -e 'roxygen2::roxygenise()'

check: document
	$(R) -e 'devtools::check(document = FALSE, args = c("--no-manual", "--no-build-vignettes"))'

check-full: document
	$(R) -e 'devtools::check(document = FALSE, args = c("--no-manual"))'

data:
	$(R) -e 'targets::tar_make()'

clean:
	rm -rf *.Rcheck novels_*.tar.gz inst/doc doc
