# VT.FOREST.DOUBLE --------------------------------------------------------
# IF RUNNING DOUBLE FOREST COMPUTATION

#' A reference class to compute twins via double random forests
#' 
#' @include forest.R
#' 
#' @field model_trt1 a caret/RandomForest/randomForest object for treatment T = 1
#' @field model_trt0 a caret/RandomForest/randomForest object for treatment T = 0
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
      "Compute twin1 with OOB predictions from double forests"
      # Model with treatment (1)
      .self$twin1[.self$vt.object$data[, 2] == 1] <- VT.predict(rfor = .self$model_trt1, type = .self$vt.object$type)
      
      # Model without treatment (0)
      .self$twin1[vt.object$data[, 2] == 0] <- VT.predict(rfor = .self$model_trt0, type = .self$vt.object$type)
      return(.self$twin1)
      return(invisible(.self$twin1))
    },
    
    computeTwin2 = function(){
      "Compute twin2 by the other part of data in the other forest"
      # Model with treatment (1)
      .self$twin2[.self$vt.object$data[, 2] == 1] <- VT.predict(.self$model_trt0, newdata = .self$vt.object$getX(1, interactions = F), type = .self$vt.object$type)
      
      # Model without treatment (0)
      .self$twin2[.self$vt.object$data[, 2] == 0] <- VT.predict(.self$model_trt1, newdata = .self$vt.object$getX(0, interactions = F), type = .self$vt.object$type)
      return(invisible(.self$twin2))
    }
  )
)