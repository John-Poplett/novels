# Author Fold Assignments

This directory contains the stored author-grouped fold assignments used by the novels package.

## File: `author_folds.csv`

- Contains one row per author in the dataset
- Columns:
  - `author`: Author name (character)
  - `scheme`: Assignment scheme name (`author5_v1`)
  - `fold`: Fold number (integer 1-5)

## Usage

The pipeline reads this file to ensure consistent fold assignments across different environments and builds.

## Adding New Authors

When adding new authors to the dataset:

1. **Append only**: Add new rows for new authors only; never modify existing rows
2. **Maintain structure**: New rows must conform to same schema
3. **Choose the fold by the balance rule** (2026-11-25): take the new books in
   gutenberg_id order; each new author goes to the fold with the fewest corpus
   rows at that moment, ties to the lower fold. First applied to the two rows
   recovered from the 2013 archive (`recovered_rows.csv`): "Morrow, W. C." to
   fold 5 (153 rows to 154), then "Stoddard, Lorimer" to fold 3 (154 to 155).
   Do not re-run `data-raw/author-folds.R` (it refuses to overwrite)
4. **Verify compatibility**: Ensure the new assignments are consistent with existing ones

## Important Notes

- This assignment is based on seed 42 and author order as they appear in the first appearance
- The fold table is **append-only** - any change to existing rows would invalidate baselines
- Only positional seeding (not stored scheme) should be used when re-assigning folds with new authors

## File: `recovered_rows.csv`

The two books the 2013 archive holds as text files but `novel_meta.txt`
lost: 23177 (its line breaks off before the count) and 23178 (no line).
`recoverLostRows()` (`R/corpusRepair.R`) checks each row against its folder
(class, fold and genre) and appends it to the parsed metadata.

- `gutenberg_id`, `genre`, `fold`, `response`: from the file's folder, `path`
- `download_count`: the Wayback Machine capture of the book's page nearest
  the scrape (5 November 2012), at `count_timestamp` and `count_url`
- `note`: why the row was lost, and any decision taken about it
