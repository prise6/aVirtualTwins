# VT.TREE.CLASS -----------------------------------------------------------

#' A reference class to compute subgroups by classifiation tree
#' 
#' @include tree.R
#' 
#' @import methods
VT.tree.class <- setRefClass(
  Class = "VT.tree.class",
  
  contains = "VT.tree",
  
  methods = list(
    initialize = function(vt.difft, threshold = 0.05, sens = ">", screening = NULL){
      callSuper(vt.difft, threshold, sens, screening)
      
      .self$name <- .self$computeNameOfTree("class")
      
      if(.self$sens == ">"){
        .self$outcome  <- ifelse(.self$vt.difft$difft >= .self$threshold, 1, 0)
      } else {
        .self$outcome  <- ifelse(.self$vt.difft$difft <= .self$threshold, 1, 0)
      }  
    },
    
    run = function(...){
      callSuper()
      
      data  <- .self$getData()  
      if(sum(data[,1]) != 0){
        .self$tree <- rpart::rpart(as.formula(paste(.self$name, ".", sep = "~")), data = data, method = "class", ...)
        .self$Ahat <- as.numeric(predict(.self$tree, data, type = "class")) - 1
      }else{
        .self$Ahat <- .self$outcome
      }
      
      return(invisible(tree))
    },
    
    sumup = function(){
      cat("Classification Tree")
      callSuper()
    }
  )
)

VT.tree.class$lock("threshold", "vt.difft")