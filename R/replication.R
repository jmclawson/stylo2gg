##### manage_stylo_log_meta() #####
# Handles a couple things that are used by both stylo_log() 
# and stylo_replicate(). Not exported.

manage_stylo_log_meta <- function(
    log_label, 
    log_date,
    add_dir_date){
  if(is.null(log_label)) {
    log_label <- "stylo_log"
  }
  
  if(add_dir_date) {
    dir_label <- paste(log_label, log_date)
  } else {
    dir_label <- log_label
  }
  
  log_label <- paste(log_label, log_date)
  
  return(list(label=log_label, dir=dir_label))
}

##### process_log #####
# Parses the log file as a table. Not exported.
# Example:
#    process_log("stylo_log/stylo_log 2023-01-27.txt")

process_log <- function(log_path){
  if(!file.exists(log_path)) {
    message("There is no log file found. Please be sure to set appropriate values for the log_label and add_dir_date arguments.")
  } else {
    log_whole <- readLines(log_path)
    all_processed <- 
      grep("Processed:", log_whole, ignore.case = TRUE)
    all_call <- 
      grep("Call:", log_whole, ignore.case = TRUE)
    all_config <- 
      grep("Config:", log_whole, ignore.case = TRUE)
    all_end <- 
      grep("=====", log_whole, ignore.case = TRUE)
    
    log_table <- data.frame(
      date = log_whole[all_processed[1] + 1],
      call = log_whole[(all_call[1] + 1):(all_config[1]-2)] |> 
        paste0(collapse = ", "),
      config = log_whole[(all_config[1] + 1):(all_end[1]-1)] |> 
        paste0(collapse = ", ")
    )
    if (length(all_processed)>1) {
      for(i in 2:length(all_processed)) {
        this_row <- data.frame(
          date = log_whole[all_processed[i] + 1],
          call = log_whole[(all_call[i] + 1):(all_config[i]-2)] |> 
            paste0(collapse = " "),
          config = log_whole[(all_config[i] + 1):(all_end[i]-2)] |> 
            paste0(collapse = ", ")
        )
        
        log_table <- rbind(log_table, this_row)
      }
    }
    
    return(log_table)
  }
}

##### log_files #####
# Logs files that were modified near the same time as 
# stylo_config.txt. Not exported.

log_files <- function(log_dir) {
  file_data <- file.info(list.files()) |> 
    as.data.frame() |> 
    rownames_to_column(var = "file")
  
  # find the modification time of stylo_config.txt
  target_time <- file_data |> 
    filter(file == "stylo_config.txt") |> 
    pull("mtime")
  
  # add column of difference from target time
  file_data <- file_data |> 
    mutate(difference = difftime(mtime, target_time)) |> 
    # limit to things in the past 5 seconds
    filter(abs(difference) < 5,
           !isdir)
  
  new_filenames <- 
    target_time |> 
    str_replace_all(":", "-") |> 
    paste0(" - ", file_data$file)
  
  new_filenames <- file.path(log_dir, new_filenames)
  
  file.copy(from=file_data$file, to=new_filenames)
  
}