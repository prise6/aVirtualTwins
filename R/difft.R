
#' Difference between twins
#' 
#' A reference class to represent difference between twin1 and twin2
#' 
#' Difft are calculated depending on the favorable outcome chosen. It is the 
#' second level of the outcome. For example, if the outcome is 0 and 1, the 
#' favorable outcome is 1. Then, \deqn{difft_i = twin1_i - twin2_i if T_i = 1}
#' \deqn{ difft_i = twin2_i - twin1_i if T_i = 0}.
#' So \emph{absolute} method is :
#' \deqn{P(Y = 1 | T = 1) - P(Y = 1 | T =0)}
#' So \emph{relative} method is :
#' \deqn{P(Y = 1 | T = 1)/P(Y = 1 | T =0)}
#' So \emph{absolute} method is :
#' \deqn{logit(P(Y = 1 | T = 1)) - logit(P(Y = 1 | T =0))}
#' 
#' @include object.R
#'   
#' @field vt.object VT.object (refClass) representing data
#' @field twin1 vector of \eqn{E(Y|T = real treatment)}
#' @field twin2 vector of \eqn{E(Y|T = another treatment)}
#' @field method Method available to compute difft : c("absolute", "relative",
#'   "logit"). Absolute is default value. See details.
#' @field difft vector of difference between twin1 and twin2
#'   
#' @name VT.difft
#'   
#' @export VT.difft
#'   
#' @seealso \code{\link{VT.forest}}, \code{\link{VT.forest.one}},
#'   \code{\link{VT.forest.double}}
#'   
#' @import methods
VT.difft <- setRefClass(
  Class = "VT.difft",
  
  fields = list(
    vt.object = "VT.object",
    twin1 = "vector",
    twin2 = "vector",
    method = "character",
    difft = "vector"
  ),
  
  methods = list(
    initialize = function(vt.object = VT.object(), method = "absolute", ...){
      
      .self$vt.object <- vt.object
      
      .self$twin1 <- .self$twin2  <- rep(NA, nrow(vt.object$data))
      
      if(!method %in% c("absolute", "relative", "logit")) stop(sprintf("Method %s is not valid", method))
      
      .self$method <- method
      
      .self$initFields(...)
    },
    
    computeDifft = function(){
      "Compute difference between twin1 and twin2. See details."
      
      if(sum(is.na(.self$twin1)) != 0 | sum(is.na(.self$twin2)) != 0 ) stop("Twins must be valid")
      
      if(.self$method == "absolute")
        .self$difft <- ifelse(.self$vt.object$data[, 2] == 1, .self$twin1 - .self$twin2, .self$twin2 - .self$twin1)
      if(.self$method == "relative")
        .self$difft <- ifelse(.self$vt.object$data[, 2] == 1, .self$twin1/.self$twin2, .self$twin2/.self$twin1)
      if(.self$method == "logit"){
        logit <- function(x) log(x/(1-x))
        .self$difft <- ifelse(.self$vt.object$data[, 2] == 1, logit(.self$twin1) - logit(.self$twin2), logit(.self$twin2) - logit(.self$twin1))
      }
      
      return(invisible(.self$difft)) # To see later
    }
  )
)