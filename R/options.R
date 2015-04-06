set_swirl_options <- function(...){
  args <- list(...)
  if(length(args) == 0){
    stop("Please provide arguments so that appropriate options can be set.")
  }
  
  # Create new options frame
  args_vector <- unlist(args)
  new_opts <- data.frame(
    name = names(args_vector),
    value = as.vector(args_vector))
  
  # If the options file does not exist, create it
  # else update the old options file
  if(!file.exists(opts_path())){
    write.csv(new_opts, file = opts_path(), row.names = FALSE)
  } else {
    old_opts <- read.csv(opts_path(), stringsAsFactors = FALSE, 
                                  header = TRUE)
    new_opts_file <- rbind(new_opts, old_opts[!(old_opts$name %in% new_opts$name),])
    new_opts_file$name <- as.character(new_opts_file$name)
    new_opts_file$value <- as.character(new_opts_file$value)
    new_opts_file <- new_opts_file[order(new_opts_file$name),]
    write.csv(new_opts_file, file = opts_path(), row.names = FALSE)
  }
  message("Options set successfully!")
  invisible()
}

get_swirl_option <- function(name){
  opts <- read.csv(opts_path(), stringsAsFactors = FALSE, 
                       header = TRUE)
  if(name %in% opts$name){
    opts[name == opts$name,]$value
  } else {
    stop(paste0("Option name '", name, "' not found."))
  }
}

delete_swirl_option <- function(name){
  opts <- read.csv(opts_path(), stringsAsFactors = FALSE, 
                   header = TRUE)
  if(name %in% opts$name){
    opts <- opts[name != opts$name,]
    write.csv(opts, file = opts_path(), row.names = FALSE)
  } else {
    stop(paste0("Option name '", name, "' not found."))
  }
  message(paste0("Option '", name, "' deleted successfully!"))
  invisible()
}

#' Get the options file path
#' @importFrom rappdirs user_data_dir
opts_path <- function(){
  # Find user data directory
  udd <- user_data_dir(appname = "swirl", appauthor = "swirldev", roaming = TRUE)
  
  # Construct path to swirl options
  file.path(udd, "swirl_options.csv")
}