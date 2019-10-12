

find_missing_packages <- function(path = ".", recursive = TRUE) {

  out <- list(0)

  script_files <- list.files(path, recursive = recursive,
    full.names = TRUE, pattern = "\\.r$|\\.R$")

  if (length(script_files) == 0) print("no .r or .R files found!")

  required_packages <- character()

  for (i in 1:length(script_files)) {
    my_code <- readLines(script_files[i])
    m <- gregexpr("(?<=library\\().*(?=\\))|(?<=require\\().*(?=\\))", my_code, perl = TRUE)
    local_packages <- unlist(regmatches(my_code, m))
    required_packages <- sort(unique(c(required_packages, local_packages)))
  }

  out$required <- required_packages

  if (length(required_packages) > 0) {
    print("these packages are directly called in this codebase:")
    cat(paste(c(required_packages, "\n"), collapse = "\n"))

    installed_packages <- installed.packages()
    missing <- required_packages[!required_packages %in%
      installed_packages[,"Package"]]

    if (length(missing) > 0) {
      print("the following packages are required, but not installed:")
      cat(paste(c(missing, "\n"), collapse = "\n"))
      out$missing <- missing
      available_packages <- available.packages()
      non_cran_missing <- missing[missing %in% available_packages[,"Package"]]
      if (length(non_cran_missing) > 0) {
        print("of those missing, the following are not on CRAN:")
        cat(paste(c(non_cran_missing, "\n"), collapse = "\n"))
      }
    } else {
      print("all are installed already!")
    }
  } else {
    print("no packages directly called in this code base!")
  }
  return(out)

}
