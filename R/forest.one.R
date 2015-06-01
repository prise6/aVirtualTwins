# VT.FOREST.ONE -----------------------------------------------------------
# IF RUNNING ONE FOREST COMPUTATION

#' A reference class to compute twins via one random forest
#' 
#' @include forest.R
#' 
#' @field model ANY a caret/RandomForest/randomForest class object
#' @field interactions logical set TRUE if model has been computed with interactions 
#' @field ... field from parent class : VT.forest
#' 
#' @import methods
VT.forest.one <- setRefClass(
  Class = "VT.forest.one",
  
  contains = "VT.forest",
  
  fields = list(
    model = "ANY",
    interactions = "logical"
  ),
  
  methods = list(
    initialize = function(vt.object, model, interactions = T, ...){
      .self$checkModel(model)
      
      .self$model <- model
      
      .self$interactions <- interactions
      
      callSuper(vt.object, ...)
    },
    
    computeTwin1 = function(){
      "Compute twin1 with OOB predictions"
      .self$twin1 <- as.vector(VT.predict(rfor = .self$model, type = .self$vt.object$type))
      return(invisible(.self$twin1))
    },
    
    computeTwin2 = function(){
      "Compute twin2 by switching treatment and applying random forest model"
      .self$twin2 <- as.vector(VT.predict(.self$model,
                                          newdata = .self$vt.object$getX(interactions = .self$interactions),
                                          .self$vt.object$type))
      return(invisible(.self$twin2))
    }
  )
)