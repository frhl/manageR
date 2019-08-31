#' check package for errors
#'
#' @description Metafunction of "releasePackage.R" used to check whether myfunc
#'              is correctly formatted for release.
#' @param targetDev String. Indicate the path to target developer dir.
#' @param ver String. What version should be checked?
#' @param package String. What package should be checked?
#' @param checks Vector containing string for the checks to be performed.
#'
#' @note This function is is used in the pa

checkPackage <- function(targetDev, targetRelease, ver, package="myfunc",
                         checks=c("DESCRIPTION", "SIZE", "VERSIONS", "LOGS", "EXPORT"),
                         verbose=TRUE){


  '%nin%' <- function(x,y) {!(x %in% y)}
  allReleases <- dirname(targetRelease)
  n_warnings = 0

  # Read description for potential erronous instructions
  ddata=readLines(paste(targetDev,"/DESCRIPTION",sep=""))

  if ("DESCRIPTION" %in% checks){

    ### CHECK DESCRIPTION DATE ###
    if (verbose) cat("* checking DESCRIPTION (date).. ")
    description_date=ddata[grep("\\Date\\b",ddata)[1]]
    description_date=gsub("(Date\\:)|\\ ", "", description_date)
    date=format(Sys.time(), paste0("%d-%b-%Y"))
    if ((tolower(description_date) != tolower(date))){
      write(paste0("\n* WARNING: Date in description '",description_date,"' do not match the actual date: '",date,"'."), stderr())
      n_warnings = n_warnings + 1
    } else
      if (verbose)cat("OK!\n")

    ### CHECK DESCRIPTION COLLATE ####
    # List myFunc files and regex current collate
    file_funcs <- list.files(file.path(targetDev,"R"))
    re_collate_files <- regexpr("'.+\\.R'", ddata, perl=TRUE)
    collate_matches = regmatches(ddata, re_collate_files)
    collate_matches = as.vector(gsub("'","",collate_matches))

    # Check if collate matches the files in myfunc
    collate_difference = file_funcs[file_funcs %nin% collate_matches]
    if (verbose) cat("* checking DESCRIPTION (collate)..")
    if (identical(collate_difference,character(0)) == FALSE){
      stop(paste0(" Invalid Collate: The collate in 'myfunc/DESCRIPTION' does not match the actual files present in 'myfunc/R'.",
                  " Please, edit the collate by adding: '",collate_difference,"' and try again."))}
    if (verbose) cat(" OK!\n")
  }


  if ("LOGS" %in% checks){

    ### CHECK THAT NEWS/CHANGELOG ARE UPDATED ###
    ctim_changelog <- format(file.info("E:/../myfunc/CHANGELOG")$mtime, "%d-%b-%Y") 
    ctim_news <- format(file.info("E:/../myfunc/myfunc/NEWS")$mtime, "%d-%b-%Y")

    if (ctim_news != date) {write(paste("* WARNING: NEWS is out of date."), stderr()); n_warnings = n_warnings + 1}
    if (ctim_changelog != date) {write(paste("* WARNING: CHANGELOG is out of date."), stderr()); n_warnings = n_warnings + 1}

  }

  if ("DESCRIPTION" %in% checks){

    ### CHECK THAT VERSION MATCHES THE DESCRIPTION ###
    if (verbose) cat("* Description version.. ")
    description_version=ddata[grep("\\bVersion\\b",ddata)]
    description_version=gsub("[^0-9.]", "", description_version)
    if ((description_version != ver) && (ver != "test")){stop(paste0("Version in description: '",description_version,
                                                                     "' and indicated version: '",ver,"' does not match!" ))}
    if (verbose) cat("OK!\n")


    ### CHECK THAT VERSION NAME IS UNIQUE ###
    available_releases=list.files(allReleases)
    available_releases=gsub("([^0-9.])", "", available_releases)

    if (verbose) cat("* Checking package name..")
    #if (paste0("Ver_",ver) %in% available_releases){
    #  write(paste0("WARNING. Version ",ver," already exists! Continuing will overwrite it!"),stderr())
    #  n_warnings = n_warnings + 1
    #  # Ask user for verification
    #  input=as.character(readline(prompt=paste0("Continue? [y/n]:  ")))
    #  if (tolower(input) != "y"){stop("Cancelled execution!")}
    #}

    ### CHECK THAT VERSION IS THE NEXT LOGICAL NUMBER ###
    if (ver != "test"){
      targetPathDirs <- list.files(allReleases,recursive = FALSE)
      dirs <- sort(targetPathDirs[grep(pattern = "^Ver_\\d\\.\\d(\\.\\d(\\.\\d$)?)?",targetPathDirs)])
      dirs <- substr(dirs,5,length(dirs))
      newest_version = dirs[length(dirs)]
      if (compareVersion(ver,newest_version) == -1){
        stop(paste0("The requestion version no. '",ver,"' is lower than the latest released version no."))}
    }
    if (verbose) cat(" OK!\n")
  }

  ### Mandatory checks: target exists
  #browser()
  if (verbose) cat("* Checking that target is not already released..")
  if (file.exists(targetRelease)){
    write(paste0("\n* ERROR Execution aborted. Target '",targetRelease,"' does already exists. Please, remove manually before attempting to release."),stderr())
    stop("Release was aborted.")
  }
  if (verbose) cat(" OK!\n")

  ### Check myfunc exports
  if ("EXPORT" %in% checks){

    # Prepare exports'
    if (verbose) cat("* checking NAMESPACE..")
    lines = readLines("NAMESPACE")
    exports = unlist(lapply(lines,  function(x) if (grepl("export", x)) return(gsub('export\\(|\\)|\\"', "", x))))

    # Prepare myfunc namespace
    ns <- asNamespace(package)
    result <- try(namespaceExport(ns, vars = exports), silent = T)
    if (class(result) == "try-error"){
      write("\n* WARNING. A fatal error will occur when trying to install myfunc:", stderr())
      write(paste0(result[1]), stderr())
      write("\nCheck myfunc/NAMESPACE, and ensure that the exports match the ones set in myfunc/R", stderr())
    } else {if (verbose) cat(" OK!\n")}
  }




  ### CHECK SIZE OF RELEASE ###
  if ("SIZE" %in% checks){
    folderSizeMB=round((sum(file.size(list.files(targetDev, all.files = TRUE, recursive = TRUE,
                                                 full.names = TRUE,include.dirs = TRUE))) )/(10^6))
    folderSize = ifelse(folderSizeMB > 1000, paste(round(folderSizeMB/1000,1), "GB"), paste(folderSizeMB, "MB"))

    write(paste("\n* Estimated Package Size: ", folderSize), stdout())
    if (folderSizeMB > 500) write("* WARNING: The package was found to be unusually large.", stderr())
  }

  ### FINAL CHECKS ####
  if (verbose) write(paste("* Destination Folder: ",targetRelease,".. OK!"), stdout())
  write(paste0("\nReady to release ", package ," (",ver,")..."), stderr())
  if ((n_warnings > 0) && (ver != "test")) {
    write(paste("IMPORTANT NOTE: YOU ARE MAKING A NON TEST-RELEASE WITH",n_warnings,"WARNING(S)."),stderr())
  }

  input=as.character(readline(prompt="Release? [yes/no]:  ")) # Request release permission from user
  if (tolower(input) != "yes"){stop("Cancelled execution!")}

  return(description_version)
}


