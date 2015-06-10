
#' Difference between twins
#' 
#' A reference class to represent difference between twin1 and twin2
#' 
#' Difft are calculated depending on the favorable outcome chosen. It is the
#' second level of the outcome. For example, if the outcome is 0 and 1, the
#' favorable outcome is 1. Then, \deqn{difft_i = twin1_i - twin2_i IF T_i =
#' 1} \deqn{ difft_i = twin2_i - twin1_i IF T_i = 0}
#' 
#' @include object.R
#'   
#' @field vt.object VT.object (refClass) representing data
#' @field twin1 vector of \eqn{E(Y|T = real treatment)}
#' @field twin2 vector of \eqn{E(Y|T = another treatment)}
#' @field difft vector of difference between twin1 and twin2
#'   
#' @name VT.difft
#'  
#' @seealso \code{\link{VT.forest}}, \code{\link{VT.forest.one}}, \code{\link{VT.forest.double}}
#' 
#' @import methods
VT.difft <- setRefClass(
  Class = "VT.difft",
  
  fields = list(
    vt.object = "VT.object",
    twin1 = "vector",
    twin2 = "vector",
    difft = "vector"
  ),
  
  methods = list(
    initialize = function(vt.object = VT.object(), ...){
      
      .self$vt.object <- vt.object
      
      .self$twin1 <- .self$twin2  <- rep(NA, nrow(vt.object$data))
      
      .self$initFields(...)
    },
    
    computeDifft = function(){
      "Compute difference between twin1 and twin2. See details."
      
      if(sum(is.na(.self$twin1)) != 0 | sum(is.na(.self$twin2)) != 0 ) stop("Twins must be valid")
      
      .self$difft <- ifelse(.self$vt.object$data[, 2] == 1, .self$twin1 - .self$twin2, .self$twin2 - .self$twin1)
      
      return(invisible(.self$difft)) # To see later
    }
  )
)