#' Replicate a logged stylometric analysis or create and log 
#' a reproducible analysis with \code{stylo}.
#'
#' @param date_time The date and time of a previously-logged
#' analysis to reproduce. If \code{date_time} is 
#' undefined, \code{stylo_replicate} will run 
#' \code{stylo(...)}, passing remaining parameters to that 
#' function.
#' @param log_label A label added to the directory holding
#' data for replication. Default value is "stylo_log".
#' @param add_dir_date Add the date to the log_label. 
#' Default value is \code{FALSE}
#' @param log_date Specify a date to use when logging the 
#' current work. Default value is the current date.
#' @param ... other arguments passed to \code{stylo}
#'
#' @details
#' \code{stylo_replicate()} might add directories and files 
#' within the working directory to cache parameters. 
#'
#' @examples
#' \dontrun{
#' # Run stylo() and log the process
#' my_data <- stylo_replicate() # in lieu of stylo()
#' my_data %>% stylo2gg()
#'
#' # Reproduce previous work from logged parameters 
#' reproduced_data <- stylo_replicate("2023-01-27 13:46:26")
#' }
#'
#' @import dplyr
#' @importFrom stringr str_replace_all str_extract str_detect
#' @export stylo_replicate


stylo_replicate <- function(
    date_time = NULL,
    log_label = NULL,
    add_dir_date = FALSE,
    log_date = Sys.Date(),
    ...){
  
  if(is.null(date_time)){
    stylo(...) |> 
      stylo_log(log_label,
                add_dir_date)
  } else {
    slog <- manage_stylo_log_meta(log_label, 
                                  log_date, 
                                  add_dir_date)
    
    log_path <- file.path(slog$dir, slog$label) |> 
      paste0(".txt")
    
    log_table <- process_log(log_path) |> 
      # add any corpus.dir from call to config
      mutate(
        config = call |> 
          str_extract('corpus.dir[ ]?=[ ]?".*"') |> 
          {\(x) ifelse(is.na(x), "", paste0(x, ", "))}() |> 
          paste0(config))
    
    # limit to the command for the chosen date
    stylo_call <- log_table |> 
      filter(date == date_time)
    
    if(str_detect(stylo_call$call, "replicated")) {
      stylo_call <- stylo_call |> 
        pull(call) |> 
        str_replace_all("\n","") |> 
        paste0(collapse=" ") |> 
        str_replace_all("[ ]{1,}", " ")
    } else {
      stylo_call <- stylo_call |> 
        pull(config)
      
      stylo_call <- paste0(
        "stylo::stylo(gui = FALSE,",
        stylo_call,
        ", replicated = \"",
        date_time,
        "\")")
    }
    
    eval(parse(text = stylo_call)) |> stylo_log()
  }
}
