#' @import utils
.onLoad <- function(libname, pkgname){
  newly_installed_version <- as.character(packageVersion("swirl"))
  if(file.exists(opts_path())){
    previously_installed_version <- get_swirl_option("version")
    if(previously_installed_version != newly_installed_version){
      set_swirl_options(version = newly_installed_version,
                        courses_dir = get_swirl_option("courses_dir"),
                        language = get_swirl_option("language"))
    }
  } else {
    set_swirl_options(version = newly_installed_version,
                      courses_dir = file.path(system.file("Courses", package = "swirl")),
                      language = "english")
  }
  invisible()
}

.onAttach <- function(...) {
  if(length(ls(envir=globalenv())) > 0) {
    packageStartupMessage(
      make_pretty(get_string("zzz", 1),
                  get_string("zzz", 2),
                  get_string("zzz", 3), skip_after=TRUE),
      make_pretty(get_string("zzz", 4),
                  get_string("zzz", 5), skip_after=TRUE),
      make_pretty(get_string("zzz", 6), skip_after=TRUE)
    )
  } else {
    packageStartupMessage(
      make_pretty(get_string("zzz", 7),
                  skip_after=TRUE)
    )
  }
  invisible()
}

make_pretty <- function(..., skip_before=TRUE, skip_after=FALSE) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  mes <- str_c("| ", wrapped, collapse = "\n")
  if(skip_before) mes <- paste0("\n", mes)
  if(skip_after) mes <- paste0(mes, "\n")
  mes
}