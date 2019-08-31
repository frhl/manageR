#' @title validatePackageFiles
#' @description A function used by 'releasePackage.R' to ensure
#' that all files are maintained through copy. This is usefull, since
#' since some files are sometimes not copied due them being read-only.

ValidateCopy <- function(targetDev, targetRelease, verbose=TRUE,...){
  
  # Roughly checks if all files were copied
  write("* Validating copy..!", stdout())
  filesDev=list.files(targetDev, all.files = TRUE, recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
  filesRel=list.files(targetRelease, all.files = TRUE, recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
  dif = abs(length(filesDev) - length(filesRel))
  
  if (dif != 1){
    warning(paste(dif-1,"files were not copied!"))
    return(FALSE)
    } 
  else{
    write("* All files were succesfully copied!", stdout())
    return(TRUE)
    }
  
}
