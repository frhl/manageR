
#' @title getTestImageReport
#'
#' @description This function will generate a report of the erronous tested images in
#' myfunc/tests/testthat/(..). Subsequently,  It will generated a .pdf containing visual
#' comparison of the images as well as information about the corresponding tests and code.
#'
#' @param path.my A valid path containing the folders of reference and result images.
#' @param limit Integer representing amount of maximum allowed comparisons to be saved in memory.
#' @param outfile Path for the generated .pdf-file.
#' @param extract.all A boolean indicated whether all images should be extracted regardless of a test failed or not.
#' @param reviewer String or vector specifying whether only a specific author should be extracted. E.g. "FRHL" or c("RVEJ","FRHL")
#' @param verbose< Print status. Recommended value is TRUE since iteration times may be long.
#'
#' @note The function requires a lot of memeory and should thus only be executed on my's
#' server unless the "limit" argument is specified. Moreover, specifying a reviwer may drastically
#' increase performance.
#'
#' @family testing
#' @author FRHL
#' @examples

#'
#' @export


getTestImageReport <- function(path.my=".../myfunc_Dev/myfunc",
                            outfile=NULL, limit=NULL, extract.all=FALSE,
                            reviewer=NULL, verbose=TRUE){
    ## Set paths to libs
    .libPaths(".../myfunc_Dev/R_Packages/R-3.2.3")
    devtools::load_all(".../myfunc_Dev/myfunc")
    require(png)
    require(myfunc)

    ## check if R is running x64 bit since this version
    ## has better memory usage
    if (Sys.getenv("R_ARCH") != "/x64"){
      write("Warning: You are not running x64 bit R. Vectors of certain sizes may not be allocated in memory.", stderr())
    }

    # Check inputs
    if (is.null(outfile)) stop("'outfile' must be specified!")
    if (is.null(reviewer)) write("Note: Specifying 'reviwer' will greatly reduce comparison time.",stderr())

    ## major paths
    path.test = file.path.simple(path.my,"tests/testthat/")
    path.func = file.path.simple(path.my,"R")

    ## Get path to test directories
    path.ref = file.path(path.test,"testReference")
    path.res = file.path(path.test,"testResults")
    file.ref = list.files(path.ref,recursive=TRUE)
    file.res = list.files(path.res,recursive=TRUE)
    file.func.path = file.path(path.func,list.files(path.func)) # Used for author info

    ## regex all pngs and sort
    file.ref=file.ref[grep(pattern="^.*\\.png",file.ref)]
    file.res=file.res[grep(pattern="^.*\\.png",file.res)]

    ## Ensure that all files have a correponding result
    if (length(file.ref) != length(file.res)) warning(paste("Amount of res/ref does not match. Setdiff:",list(setdiff(file.res,file.ref)),":the output is not a complete comparison!"))

    ## Only work on the intersect pngs
    file.common=intersect(file.ref,file.res)
    file.ref = sort(file.ref[file.ref %in% file.common])
    file.res = sort(file.res[file.res %in% file.common])

    ## Apply full path to files
    full.path.ref = file.path.simple(path.ref,file.ref)
    full.path.res = file.path.simple(path.res,file.res)
    all.plots = list()
    count.comparison=0

    ## Ensure that outfile is a .pdf
    if (!grepl(pattern="*.pdf",outfile) && !is.null(outfile)) stop("outfile is not a .pdf!")

    ## Restrict amount of comparisons if needed
    if (is.null(limit)){iter.len = length(file.common)} else {iter.len = limit}

    if (verbose){
        write("System Paths.. OK!",stdout())
        write(paste("Comparing",iter.len,"images.."),stdout())
        cat("[")
    }

    ## Compare image
    for (i in 1:iter.len){

        # File is being read..
        if (verbose) cat(".")

        # Gather author information


        func= sub("/.*","",file.ref)[i]
        main.func.path = file.func.path[grepl(paste0("/",func,".R"),file.func.path,perl=T)]
        if (identical(main.func.path,character(0))==FALSE){
          lines=suppressWarnings(readLines(main.func.path))
          if (length(lines)> 1){
            author=lines[grepl("@author",lines)]
            author=gsub("#' @author ","",author)
            rm(lines)
          }else{author=="N/A"}
        }


        # Check if comparison should be executed (This step conserves a lot of memory)
        if (is.null(reviewer) || any(grepl(author,reviewer)) == TRUE){

          ## Load images as matrices
          res = (readPNG(full.path.ref[i])) #[,,1:3]
          ref = (readPNG(full.path.res[i])) #[,,1:3]

          # Extract identical or all images
          if  ((!identical(res,ref)) || (extract.all == TRUE)){

              # Something is wrong with full.path.re[s/f].. maybe S3 classes? temp solution:
              pathRef = full.path.ref[i]
              pathRes = full.path.res[i]

              count.comparison = count.comparison + 1
              if (verbose) cat(count.comparison)

              ### Get the test-code chunk
              ## Get the ID of the test (Very ugly regex.. use regexpr when time)
              error.id = gsub(".*/","",full.path.ref[i])
              error.id = gsub("^\\w+\\.","",error.id,perl=TRUE)
              error.id = gsub("\\.png","",error.id,perl=TRUE)
              error.id = gsub("^\\w+\\.","",error.id,perl=TRUE)

              ## Get the curent test.func.R of interest
              ## Some functions require a name change.. REDO when back in DK
              test.func = file.path.simple(path.test,paste0("test.",func,".R"))
              test.func = gsub("forestplot","forestplots",test.func) # <-- remove this
              test.func.list = file.path.simple(path.test,list.files(path.test,pattern="*.R"))
              test.file = intersect(test.func,test.func.list)

              ## open up the file and regex lines used in test
              ##if (T) print(paste0("\\n *",i," -> ",func))

              if (identical(test.file,character(0))==FALSE){
                  lines = readLines(test.file)
                  look.line = grepl(as.character(error.id),lines)
                                          # Look for the line containing data
                  for (i in 1:length(look.line)){
                      if (look.line[i]==TRUE) what.line=i }
                  test.lines = lines[(what.line):(what.line+5)]
              }else{test.lines=paste("N/A file:",test.func)}

              #####################################
              ### Generate titles and subheadings #
              #####################################

              ## img.scale = 700
              ## png(paste0(func,".png"),width=5*img.scale,height=3.5*img.scale)
              plot.new()
              title(main=paste0("Reviewer: ", author,".     ",
                                "Function: ",func,".     ",
                                "ID: ",error.id),
                    sub=paste("placeholder",cex.main=3))

              ## Generate pixel by pixel data
              res.compare = res[,,1:3]
              ref.compare = ref[,,1:3]

              ## Pixels for reference
              static.pixels = ref.compare*0.25
              dif  = abs(res.compare-ref.compare)

              ## Convert inequalities to red
              dif[dif != 0] = 1
              dif[,,1] = dif[,,1]*0.929
              dif[,,2] = dif[,,2]*0.109
              dif[,,3] = dif[,,3]*0.141

              ## Total matrix value must be less than 0
              dif = abs(dif)*(1-0.25)
              final = static.pixels + dif

              ## Aspect ratio for plotting
              aspect.height = dim(res.compare)[1]; aspect.width = dim(res.compare)
              aspect.ratio = aspect.height/aspect.width

              xleft = 0
              ybottom = 0.5 # in the top of the iamge
              xright = 0.33*0.90 # 90% scaled down
              ytop = 0.5+0.33*0.90 # 90% scaled down

              ## Generate all images
              rasterImage(ref, xleft, ybottom, xright, ytop)
              rasterImage(res, xleft+0.33, ybottom, xright+0.33, ytop)
              rasterImage(final,xleft+0.66, ybottom, xright+0.66, ytop)

              ## text at apropiate images
              text(0.50,1,labels=c(date()),cex=2)
              text(0.33/2,0.95,labels=c("Reference"),cex=2)
              text(0.33/2,0.92,labels=c(pathRef),cex=0.30)  # path to ref
              text(0.50,0.95,labels=c("Result"),cex=2)
              text(0.50,0.92,labels=c(pathRes),cex=0.30)  # path to ref
              text(0.83,0.95,labels=c("Comparison"),cex=2)

              # Be more informative if all tests are to be extracted
              if (T){
                  if (identical(res,ref))
                      {text(0.50,0.92,labels=c("STATUS: OK."),col="green",cex=2)}
                  else {text(0.50,0.9,labels=c("STATUS: ERROR."),col="red",cex=2)}
              }

              ## Text from test.func.R
              text(0.50,0.40,"SUMMARY FROM TESTTHAT:",cex=2)
              text(0.50,0.37,paste("Path:",test.file),cex=1.5)
              text(0.10,seq(0.30,0.20,-0.02),adj=c(0,0),labels=test.lines,cex=1.5)

              ## Save plots
              all.plots[[length(all.plots)+1]] <- recordPlot()


              graphics.off()

          }
          ## Remove memory consuming objects
          rm(ref); rm(res)
          graphics.off()
        }

    }
    if (verbose) {cat("]"); write("\nComparing... OK!",stdout())}



    ## Generate a pdf file
    if (count.comparison != 0){

        ## Allow user to make own path
        if (is.null(outfile)){pdfname = "summary_plots.pdf"} else
          {pdfname = outfile}
        if (verbose) write(paste("Succes. Writing to",outfile,".."),stdout())
        pdf(pdfname,onefile=TRUE,width=11*2,height=8*2)
        for (i in all.plots){
            replayPlot(i)
        }
    } else if (verbose){
      write("Succes. No conflicting images found.",stdout())
    }
    graphics.off()
    write("Succes.",stdout())
}





