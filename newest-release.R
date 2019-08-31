
NewestQCPRelease <- function(release.dir = "", verbose = T) {

  files <- list.files(release.dir,recursive = FALSE, full.name = T)
  versions <- sort(files[grep(pattern = ".*Ver_\\d\\.\\d(\\.\\d(\\.\\d$)?)?",files)])
  newest.version = versions[length(versions)]
  if (verbose) write(paste("Newest Package:", newest.version),stdout())
  return(newest.version)
}
