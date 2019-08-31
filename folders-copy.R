#' @title copyFolders
#' @description A function that is used to copy a directory from one
#'              location to another
#' @param from string. Path to directory to copy from.
#' @param to string. Path to new directory.
#' @export

# Function that copies all specified files and folders
copyFolders <- function(from, to,verbose=TRUE,...) {
  target <- file.path(to, basename(from))
  if (file.exists(target)) stop(paste0("Target folder ", target, " already exists."))

  if (verbose) write(paste0("* Copying files from '", from,"' to '", to,"'."), stdout())

  # Gets paths
  path <- data.frame(target = list.files(from, recursive = TRUE, all.files = TRUE, include.dirs = TRUE))
  path$from  <- file.path(from, path$target)
  path$to  <- file.path(to, basename(from), path$target)

  # Get type of file/folders
  path$type <- factor("file", levels = c("file", "folder", "link"))
  path$type[file.info(path$from)$isdir] <- "folder"

  # Make copy
  invisible(lapply(path$to[path$type == "folder"], dir.create, recursive = TRUE))
  invisible(file.copy(from = path$from[path$type == "file"], to = path$to[path$type == "file"]))

  if (verbose) write("* Copying files.. OK!", stdout())
}
