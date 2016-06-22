swirl_language <- function(){
  lang <- getOption("swirl_language")
  langs <- c("chinese_simplified", "english", "french", "german", "korean", 
             "spanish", "turkish")
  
  if(is.null(lang) || !(lang %in% langs)){
    "english"
  } else {
    lang
  }
}

#' Select a language
#' 
#' Select a language for swirl's menus.
#' @param language The language that swirl's menus will use.
#' This must be one of the following values: \code{"chinese_simplified"}.
#' \code{"english"}, \code{"french"}, \code{"german"},
#' \code{"korean"}, \code{"spanish"}, or \code{"turkish"}.
#' If \code{NULL} the user will be asked to choose a language
#' interactively. The default value is \code{NULL}.
#' @param append_rprofile If \code{TRUE} this command will append 
#' \code{options(swirl_language = [selected language])} to the end of your 
#' Rprofile. The default value is \code{FALSE}.
#' 
#' @export
select_language <- function(language = NULL, append_rprofile = FALSE){
  langs <- c("chinese_simplified", "english", "french", "german", "korean", 
             "spanish", "turkish")
  if(is.null(language)){
    selection <- select.list(langs)
  } else if(!(language %in% langs)){
    stop("Invalid value for 'language.'")
  } else {
    selection <- language
  }
  
  options(swirl_language = selection)
  
  if(append_rprofile){
    opts <- paste0("options(swirl_language = '", selection, "')")
    cat(opts, "\n", file = file.path("~", ".Rprofile"), append = TRUE)
  }
}

# set working directory to swirl repo before using
#' @importFrom yaml yaml.load_file
compile_languages <- function(){
  ctime <- as.integer(Sys.time())
  clone_dir <- file.path(tempdir(), ctime)
  dir.create(clone_dir, showWarnings = FALSE)
  git_clone <- paste("git clone https://github.com/swirldev/translations.git", clone_dir)
  system(git_clone)
  
  menus_dir <- file.path(clone_dir, "menus")
  menus <- list.files(menus_dir, pattern = "yaml$", full.names = TRUE)
  
  for(i in menus){
    lang_name <- sub(".yaml$", "", basename(i))
    cmd <- paste0(lang_name, " <- wrap_encoding(yaml.load_file('", i, "'))")
    eval(parse(text=cmd))
  }
  
  comma_sep_langs <- paste(sub(".yaml$", "", basename(menus)), collapse = ", ")
  cmd <- paste0("save(", comma_sep_langs, ", file = file.path('R', 'sysdata.rda'))")
  eval(parse(text=cmd))
  unlink(clone_dir, recursive = TRUE, force = TRUE)
}

"%N%" <- function(f, y){
  result <- f(y)
  if(is.null(result)){
    y
  } else {
    result
  }
}

s <- function(){
  s_helper
}

s_helper <- function(x){
  cmd <- paste0(swirl_language(), "$`", x, "`")
  tryCatch(eval(parse(text=cmd)),
    warning = function(c) NULL
  )
}

# set working directory to swirl repo before using
# make sure the global env is clear before using

#' @importFrom stringr str_match
check_strings <- function(){
  load(file.path("R", "sysdata.rda"))
  langs <- ls()
  ##langs <- "english"
  
  for(i in list.files("R", pattern = "\\.R$")){
    source_code <- readLines(file.path("R", i), warn = FALSE)
    strings <- grep("s\\(\\)%N%", source_code)
    for(j in strings){
      for(l in langs){
        if(!(str_match(source_code[j], '"(.*?)"')[,2] %in% eval(parse(text = paste0("names(", l, ")"))))){
          message(l, " : '", str_match(source_code[j], '"(.*?)"')[,2], "' : ", i)
          ##cat('"', str_match(source_code[j], '"(.*?)"')[,2], '"', ':\n "', str_match(source_code[j], '"(.*?)"')[,2], '"\n\n',  sep = "")
        }
      }
    }
  }
}