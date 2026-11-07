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
3. **Update pipeline**: Re-run `data-raw/author-folds.R` (it will refuse to overwrite)
4. **Verify compatibility**: Ensure the new assignments are consistent with existing ones

## Important Notes

- This assignment is based on seed 42 and author order as they appear in the first appearance
- The fold table is **append-only** - any change to existing rows would invalidate baselines
- Only positional seeding (not stored scheme) should be used when re-assigning folds with new authors