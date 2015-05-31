# VT.FOREST.DOUBLE --------------------------------------------------------
# IF RUNNING DOUBLE FOREST COMPUTATION

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
      # Model with treatment (1)
      .self$twin1[.self$vt.object$data[, 2] == 1] <- VT.predict(rfor = .self$model_trt1, type = .self$vt.object$type)
      
      # Model without treatment (0)
      .self$twin1[vt.object$data[, 2] == 0] <- VT.predict(rfor = .self$model_trt0, type = .self$vt.object$type)
      return(.self$twin1)
      return(invisible(.self$twin1))
    },
    
    computeTwin2 = function(){
      # Model with treatment (1)
      .self$twin2[.self$vt.object$data[, 2] == 1] <- VT.predict(.self$model_trt0, newdata = .self$vt.object$getX(1, interactions = F), type = .self$vt.object$type)
      
      # Model without treatment (0)
      .self$twin2[.self$vt.object$data[, 2] == 0] <- VT.predict(.self$model_trt1, newdata = .self$vt.object$getX(0, interactions = F), type = .self$vt.object$type)
      return(invisible(.self$twin2))
    }
  )
)