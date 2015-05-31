# VT.FOREST.ONE -----------------------------------------------------------
# IF RUNNING ONE FOREST COMPUTATION
# model - modèle de forêt aléatoire issus du package caret, randomForest ou party (cforest)
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
      .self$twin1 <- as.vector(VT.predict(rfor = .self$model, type = .self$vt.object$type))
      return(invisible(.self$twin1))
    },
    
    computeTwin2 = function(){
      .self$twin2 <- as.vector(VT.predict(.self$model,
                                          newdata = .self$vt.object$getX(interactions = .self$interactions),
                                          .self$vt.object$type))
      return(invisible(.self$twin2))
    }
  )
)