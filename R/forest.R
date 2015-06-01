# FORESTS -----------------------------------------------------------------

#' A abstract reference class to compute twin via random forests
#' 
#' @include difft.R predict.R
#' 
#' @import methods 
VT.forest <- setRefClass(
  Class = "VT.forest",
  
  contains = "VT.difft",
  
  methods = list(  
    run = function(){
      "Compute twin1 and twin2 computation. Switch treatment if necessary."
      .self$computeTwin1()
      
      if(inherits(.self, "VT.forest.one")) .self$vt.object$switchTreatment() #if one forest
      
      .self$computeTwin2()
      
      if(inherits(.self, "VT.forest.one")) .self$vt.object$switchTreatment() #if one forest
      
      .self$computeDifft()
      
      .self$vt.object$computeDelta() # To see later
      
      return(invisible(.self))
    },
    
    checkModel = function(model){
      "Checking model class: Must be : train, RandomForest, randomForest"
      if(!(inherits(model, "train") | inherits(model, "RandomForest") | inherits(model, "randomForest"))){
        stop("Model is not recognized. Must be : train, RandomForest, randomForest")
      }
    },
    
    getFullData = function(){
      "Return twin1, twin2 and difft in column"
      if(length(.self$twin1) != nrow(.self$vt.object$data)) stop("Twin1 must have same length as data")
      if(length(.self$twin2) != nrow(.self$vt.object$data)) stop("Twin2 must have same length as data")
      
      if(length(.self$difft) != nrow(.self$vt.object$data)) stop("Difft must have same length as data")
      
      tmp <- cbind(.self$vt.object$data, .self$twin1, .self$twin2, .self$difft)
      
      colnames(tmp) <- c(colnames(.self$vt.object$data), "twin1", "twin2", "difft")
      
      return(tmp)
    }
  )
)
