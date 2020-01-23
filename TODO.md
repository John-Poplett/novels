# TODO
* Document fixes and caveates from original study:
  * Corrupt meta-data file
  * Duplicate files in dataset
  * Authors duplicated across folds
  * Comment on download counts
* Integrate with gutenbergr
  * Define a novels\_metadata function
  * Use author\_id and gutenberg\_ids 
* Create an isTidy() function to validate metadata files
* Must pass devtools::check()
* Expand dataset
  * Automate retrieval of text from Gutenberg.org
  * Create an extensions directory for larger data sets
* Standardize on directory and CSV format. Document both.
* Revise glmnet vignette
  * Make it use glmnet fold ids
  * 
* Create vignettes for
  * [RMallet](https://github.com/mimno/RMallet)
