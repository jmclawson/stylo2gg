# Rename a category of `stylo` data for `stylo2gg`.

Rename a category of `stylo` data for `stylo2gg`.

## Usage

``` r
rename_category(df, before, after)
```

## Arguments

- df:

  An object saved from running **`stylo`** on a corpus

- before:

  A string representing the name of the category in the `stylo` object

- after:

  A string representing the new name to be used for this category

## Details

This change will not overwrite values in the originating `df` object.

## Examples

``` r
if (FALSE) { # \dontrun{
my_data <- stylo()
my_data %>% rename_category("NA", "unknown")
} # }
```
