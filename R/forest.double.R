# VT.FOREST.DOUBLE --------------------------------------------------------
# IF RUNNING DOUBLE FOREST COMPUTATION

#' Difft by double random forest
#' 
#' A reference class to compute twins via double random forests
#' 
#' \code{VT.forest.double} extends \code{VT.forest}.
#' 
#' \eqn{E(Y|T = 1)} if \eqn{T_i = 1} is estimated by OOB predictions from
#' \code{model_trt1}. 
#' \eqn{E(Y|T = 0)} if \eqn{T_i = 0} is estimated by OOB predictions from
#' \code{model_trt0}.
#' This is what \code{computeTwin1()} does.
#' 
#' Then \eqn{E(Y|T = 1)} if \eqn{T_i = 0} is estimated by model_trt1.
#' Then \eqn{E(Y|T = 0)} if \eqn{T_i = 1} is estimated by model_trt1.
#' This is what \code{computeTwin2()} does.
#' 
#' @include forest.R
#'   
#' @field model_trt1 a caret/RandomForest/randomForest object for treatment T =
#'   1
#' @field model_trt0 a caret/RandomForest/randomForest object for treatment T =
#'   0
#' @field ... field from parent class : \code{\link{VT.forest}}
#'   
#' @seealso \code{\link{VT.difft}}, \code{\link{VT.forest}},
#'   \code{\link{VT.forest.one}}
#'   
#' @export VT.forest.double
#'   
#' @name VT.forest.double
#'   
#' @import methods
VT.forest.double <- setRefClass(
  Class = "VT.forest.double",
  
  contains = "VT.forest",
  
  fields = list(
    model_trt1 = "ANY",
    model_trt0 = "ANY"
  ),
  
  methods = list(
    initialize = function(vt.object, model_trt1, model_trt0, ...){
      .self$checkModel(model_trt1)
      .self$checkModel(model_trt0)
      
      .self$model_trt1 <- model_trt1
      .self$model_trt0 <- model_trt0
      
      callSuper(vt.object, ...)
    },
    
    computeTwin1 = function(){
      "Compute twin1 with OOB predictions from double forests. See details."
      # Model with treatment (1)
      .self$twin1[.self$vt.object$data[, 2] == 1] <- VT.predict(rfor = .self$model_trt1, type = .self$vt.object$type)
      
      # Model without treatment (0)
      .self$twin1[vt.object$data[, 2] == 0] <- VT.predict(rfor = .self$model_trt0, type = .self$vt.object$type)
      return(.self$twin1)
      return(invisible(.self$twin1))
    },
    
    computeTwin2 = function(){
      "Compute twin2 by the other part of data in the other forest. See details."
      # Model with treatment (1)
      .self$twin2[.self$vt.object$data[, 2] == 1] <- VT.predict(.self$model_trt0, newdata = .self$vt.object$getX(1, interactions = F), type = .self$vt.object$type)
      
      # Model without treatment (0)
      .self$twin2[.self$vt.object$data[, 2] == 0] <- VT.predict(.self$model_trt1, newdata = .self$vt.object$getX(0, interactions = F), type = .self$vt.object$type)
      return(invisible(.self$twin2))
    }
  )
)