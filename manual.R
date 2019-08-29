# work in progress

.libPaths(lpath)

# Run this script on a computer with latex (not on server)
# manual is exported to release/documents

# Remove if manual already exists
setwd(path_documents)
if (file.exists("package.pdf")) unlink("package.pdf")
  
# Updating documentation with roxygen2
setwd(path_package)
devtools::document()
 
# write LATEX manual
setwd(path_documents)
system(paste(file.path(R.home("bin"), "R"),"CMD", "Rd2pdf", shQuote(path_package)))
  
# Debug LaTeX
if (F){
  Sys.which("pdflatex.exe") # If empty, no tex-binaries found..
  env.prev <- Sys.getenv("PATH") # The latex binary should be included here..
  bin = "C:/Users/frhl/AppData/Local/Programs/MiKTeX 2.9/miktex/bin/x64" # Find pdflatex.exe and put folder here
  Sys.setenv(PATH=paste(bin,sep=":")) # Change paths 
  Sys.which("pdflatex.exe") # now this shouldnt be empty!
  # Run the above code again.. 
  Sys.setenv(PATH=env.prev) # Reset path
}

  
  
