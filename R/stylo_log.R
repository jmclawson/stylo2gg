#' Log a reproducible stylometric analysis from \code{stylo}.
#'
#' @param stylo_object An object created from running 
#' \strong{\code{stylo}} on a corpus
#' @param log_label A label added to the directory holding
#' data for replication. Default value is "stylo_log".
#' @param add_dir_date Add the current date to the 
#' log_label. Default value is \code{FALSE}
#'
#' @details
#' \code{stylo_log()} will add directories and files in the 
#' working directory to cache parameters. 
#'
#' @examples
#' \dontrun{
#' my_data <- stylo()
#' my_data %>% stylo_log()
#'
#' # Pipe directly
#' stylo() |> stylo_log()
#' }
#'
#' @import dplyr
#' @export stylo_log


##### stylo_log #####
# Pipe from stylo() directly into stylo_log() 
# or wrap stylo() in stylo_log()
# Examples:
#    stylo() |> stylo_log()
#    stylo_log(stylo())

stylo_log <- function(
    stylo_object,
    log_label = NULL,
    add_dir_date = FALSE){
  this_object <- eval(stylo_object)
  
  slog <- manage_stylo_log_meta(log_label, 
                                log_date = Sys.Date(),
                                add_dir_date)
  
  if(!dir.exists(slog$dir)) {
    dir.create(slog$dir)
  }
  
  if (!file.exists("stylo_config.txt")) {
    stylo_config <- "(Error: stylo_config.txt file not found)"
  } else{
    stylo_config <- readLines("stylo_config.txt") |> 
      paste(collapse="\n")
  }
  
  the_call <- deparse(stylo_object$call) |> 
    paste0(collapse = "\n")
  
  stylo_object_log <- 
    paste0(
      "Processed:\n", 
      Sys.time(),
      "\n\n",
      "Call:\n",
      the_call,
      "\n\n",
      "Config:\n",
      stylo_config,
      "\n\n=====",
      "\n\n"
    )
  
  log_path <- file.path(slog$dir, slog$label) |> 
    paste0(".txt")
  
  cat(stylo_object_log, file = log_path, append = TRUE)
  
  log_files(slog$dir)
  
  return(this_object)
}

