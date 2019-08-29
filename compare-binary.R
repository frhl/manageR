#Functions for creating images and rereading them again as bytes for reference
read_binary_plot <- function(x){
  con = file(x,"rb") # To write error if not available: "No reference found"
  result = suppressWarnings(paste(readBin(con,"character", file.info(x)$size),collapse = "\n"))
  close(con)
  return (result)
}

