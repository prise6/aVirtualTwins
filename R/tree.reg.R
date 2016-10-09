# VT.TREE.REG -------------------------------------------------------------

#' Regression tree to find subgroups
#'
#' See \code{\link{VT.tree}}
#'
#' @include tree.R
#' 
#' @export VT.tree.reg
#' 
#' @name VT.tree.reg
#' 
#' @importFrom rpart rpart

VT.tree.reg <- setRefClass(
  Class = "VT.tree.reg",
  
  contains = "VT.tree",
  
  methods = list(
    initialize = function(vt.difft, threshold = 0.05, sens = ">", screening = NULL){
      callSuper(vt.difft, threshold, sens, screening)
      
      .self$name <- .self$computeNameOfTree("reg")
      
      .self$outcome  <- .self$vt.difft$difft
    },
    
    run = function(...){
      callSuper()
      data  <- .self$getData()
      
      .self$tree <- rpart::rpart(as.formula(paste(.self$name, ".", sep = "~")), data = data, ...)
      
      if(.self$sens == ">")
        res <- ifelse(stats::predict(.self$tree) >= (.self$threshold), 1, 0)
      else
        res <- ifelse(stats::predict(.self$tree) <= (.self$threshold), 1, 0)
      
      .self$Ahat <- res
      #       if(sum(res) != 0) .self$vt.forest$addAhatColumn(name, res)
      return(invisible(tree))
    },
    
    sumup = function(){
      cat("Regression Tree")
      callSuper()
    }
  )
)

VT.tree.reg$lock("threshold", "vt.difft")

