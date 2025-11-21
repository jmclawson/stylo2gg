# Replicate a logged stylometric analysis or create and log a reproducible analysis with `stylo`.

Replicate a logged stylometric analysis or create and log a reproducible
analysis with `stylo`.

## Usage

``` r
stylo_replicate(
  date_time = NULL,
  log_label = NULL,
  add_dir_date = FALSE,
  log_date = Sys.Date(),
  ...
)
```

## Arguments

- date_time:

  The date and time of a previously-logged analysis to reproduce. If
  `date_time` is undefined, `stylo_replicate` will run `stylo(...)`,
  passing remaining parameters to that function.

- log_label:

  A label added to the directory holding data for replication. Default
  value is "stylo_log".

- add_dir_date:

  Add the date to the log_label. Default value is `FALSE`

- log_date:

  Specify a date to use when logging the current work. Default value is
  the current date.

- ...:

  other arguments passed to `stylo`

## Details

`stylo_replicate()` might add directories and files within the working
directory to cache parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run stylo() and log the process
my_data <- stylo_replicate() # in lieu of stylo()
my_data %>% stylo2gg()

# Reproduce previous work from logged parameters 
reproduced_data <- stylo_replicate("2023-01-27 13:46:26")
} # }
```
