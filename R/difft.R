
#' A reference class to represent difference between twin1 and twin2
#'
#' @include object.R
#' 
#' @field vt.object VT.object (refClass) representing data
#' @field twin1 vector of \eqn{E(Y|T= real treatment)}
#' @field twin2 vector of \eqn{E(Y|T= antoher treatment)}
#' @field difft vector of difference between twin1 and twin2 
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
      "Compute difference between twin1 and twin2"
      
      if(sum(is.na(.self$twin1)) != 0 | sum(is.na(.self$twin2)) != 0 ) stop("Twins must be valid")
      
      .self$difft <- ifelse(.self$vt.object$data[, 2] == 1, .self$twin1 - .self$twin2, .self$twin2 - .self$twin1)
      
      return(invisible(.self$difft)) # To see later
    }
  )
)