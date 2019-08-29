##' @name Misc
##' @rdname misc
##'
##' @title Helper functions
##' 
##' @description
##' n converts vector or data.frame to numeric\\
##' \%p\% paste the right and the left side together with no seperator\\
##'
##' @param x anything
##' @param y anything
##' @family Misc
NULL

# Convert vector or data.frame to numeric
#' @name misc
#' @rdname misc
#' @export
#' @examples 
#' \dontrun{ ##input The object that should be converted to numeric.}
n <- function(input) {
  if(is.data.frame(input) | is.matrix(input)) {
    return(data.frame(apply(input,2,function(z) as.numeric(as.character(z)))))
  }else {
    ## Simple version
    return(as.numeric(as.character(input)))
  }   
}

#' @name misc
#' @rdname misc
#' @export
#'
#' @examples
#' "bla" %p% "alb"
"%p%" <- function(x,y) {paste(x,y,sep="")}

#' @name misc
#' @rdname misc
#' @export
#'
#' @examples
#' "bla" %nin% c("alb","asg","faasg")
'%nin%' <- function(x,y) {!(x %in% y)}

#' @name misc
#' @rdname misc
#' @export
#' @examples 
#' \dontrun{c(1,2,3) %+=% 2}
'%+=%' <- function(x,y) {x+y}

#' @name misc
#' @rdname misc
#' @export
#' 
'%-=%' <- function(x,y) {x-y}

#' inverse of logit
#' @name misc
#' @rdname misc
#' @export
#'
#' @examples
#' \dontrun{
#' ##inverse of logit
#' }
invlogit <- function(x) exp(x)/(1 + exp(x))

#'  logit
#' @name misc
#' @rdname misc
#' @export
#'
#' @examples
#' \dontrun{
#' ## logit
#' }
#' 
logit= function(x)  log(x) - log(1 - x)
