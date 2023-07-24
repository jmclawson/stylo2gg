#' Rename a category of \code{stylo} data for \code{stylo2gg}.
#'
#' @param df An object saved from running \strong{\code{stylo}}
#' on a corpus
#' @param before A string representing the name of the category 
#' in the \code{stylo} object
#' @param after A string representing the new name to be used 
#' for this category
#'
#' @details
#' This change will not overwrite values in the originating 
#' \code{df} object.
#'
#' @examples
#' \dontrun{
#' my_data <- stylo()
#' my_data %>% rename_category("NA", "unknown")
#'}
#'
#' @export rename_category

rename_category <- function(df, before, after){
  rownames(df$table.with.all.freqs) <- 
    str_replace_all(rownames(df$table.with.all.freqs), before, after)
  return(df)
}