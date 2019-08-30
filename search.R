#' @title search in R
#'
#' @description a function that searchs a directory recursively for
#' a string or vector of strings in each file. Returns a data.frame
#' containing information about the file, substring and in which line
#' the substring can be found.
#'
#' @param string A string or vector of strings specifying what there should be
#' searched for. This can also be a regular expression.
#' @param recursive Boolean indicating whether all dirs within the
#' target dir should be searched.
#' @param silent Boolean. Should the function return on the fly info
#' regarding number of files, and which files that are scanend?
#' @param case.sensitive boolean specifying whether case should
#' matter when searching files.
#' @param target string specifying what directroy that should
#' be searched.
#'
#' @return data.frame whitch coloumns 'file', 'string' and 'line'
#'
#' @note The more strings that are specified, the longer it takes
#' for the function to conduct a proper search. AVOID using extreme
#' regex parameters e.g. "." or "??" as this would result in many
#' matches.
#'
#' @examples
#' namespace = c("advanced", "delta", "eta")
#' dat = search(namespace, silent = F, case.sensitive = F)
#' nrow(dat)
#' dat
#'

search <- function(string, recursive = TRUE, silent = T, case.sensitive = F,
                   target = "package"){

  data = vector()
  files = list.files(target, recursive = T, full.names = T)
  nfiles = length(files)
  if (!silent) write(paste("scanning", nfiles, "files.."),stdout())
  for (i in 1:length(files)){
    linematch = vector()
    if (!silent) write(paste0("[",i,"/",nfiles,"] scanning ", files[i]),stdout())
    lines = suppressWarnings(readLines(files[i]))
    if (!case.sensitive) lines = tolower(lines)
    for (substring in string){
      if (!case.sensitive) substring = tolower(substring)
      regmatch = unlist(lapply(lines, function(x) grepl(substring, x)))
      linematch = c(linematch, seq(1,length(regmatch))[regmatch])
    }
    if (any(regmatch)){data = rbind(data, c(files[i], substring, paste(linematch, collapse = " "))) }
    regmatch = FALSE

  }

  # Process data.frame
  data <- data.frame(data, row.names = NULL)
  if (nrow(data) > 0){
    colnames(data) <- c("file", "string", "line")
    return(data)
  } else return(NULL)

}
