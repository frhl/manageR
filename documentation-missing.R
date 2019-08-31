#' @title  findMissingDocumentation
#' @description Searches through automatically generated .Rd files to and returns the file in which some crucial
#' documentation is missing. Please, ensure to document myfunc before running this function in order to get
#' up-to-date estimates.
#' @param outfile String path to write a .csv table
#' @param author vector, string or NULL. Which authors do you want to retain?
#' @note When using the author parameter, please note the functions discriminates between
#' capitalized letters. Thus, if you want to grab all use author = c("frhl","FRHL)
#'
#' @author frhl
#'

findMissingDocumentation <- function(outfile = NULL, author = NULL, verbose = TRUE){

  # Find files with missing documentation
  dir.ref <- "../myfunc/man"
  man.topics <- c("name","title","usage","arguments","value","description","author",
                  "examples","note","family","keyword","seealso")
  man.missing <- vector()
  man.topics.a <- paste(man.topics,"\\{",sep = "")
  if (verbose) write("NOTE: Remember to devtools::document() the package first!",stderr())
  # Iterate through markdown files
  for (file in list.files(dir.ref)){
    lines = readLines(file.path(dir.ref,file))
    tags = grepl(paste(man.topics.a, collapse = "|"),lines)
    # Only select files in which contains the correct author
    if (is.null(author) || any(grepl(paste(author,collapse = "|"),lines))){
      if (length(lines[tags]) != length(man.topics.a)){
        vars.present <- gsub("\\s*\\{[^\\}]+\\}|(\\\\)|\\{","",lines[tags])
        man.missing <- rbind(man.missing,c(file,as.vector(man.topics %in% vars.present)))
      }
    }
  }
  # Convert matrix to dataframe and rename
  df = as.data.frame(man.missing)
  colnames(df) <- paste(c("File",man.topics))
  colnames(df)[colnames(df)=="seealso"] <- "family"

  # Return
  if (!is.null(outfile)) {write.table(df, outfile, sep=",", col.names = colnames(df))}
  else {return(df)}
}



