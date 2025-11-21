# Log a reproducible stylometric analysis from `stylo`.

Log a reproducible stylometric analysis from `stylo`.

## Usage

``` r
stylo_log(stylo_object, log_label = NULL, add_dir_date = FALSE)
```

## Arguments

- stylo_object:

  An object created from running **`stylo`** on a corpus

- log_label:

  A label added to the directory holding data for replication. Default
  value is "stylo_log".

- add_dir_date:

  Add the current date to the log_label. Default value is `FALSE`

## Details

`stylo_log()` will add directories and files in the working directory to
cache parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
my_data <- stylo()
my_data %>% stylo_log()

# Pipe directly
stylo() |> stylo_log()
} # }
```
