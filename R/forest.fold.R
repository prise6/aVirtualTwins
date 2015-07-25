# VT.FOREST.FOLD ----------------------------------------------------------

#' Difft via k random forests
#' 
#' A reference class to compute twins via k random forest
#' 
#' \code{VT.forest.fold} extends \code{VT.forest}
#' 
#' Twins are estimated by k-fold cross validation. A forest is computed on k-1/k
#' of the data and then used to estimate twin1 and twin2 on 1/k of the left 
#' data.
#' 
#' @include forest.R
#'   
#' @field interactions logical set TRUE if model has been computed with 
#'   interactions
#' @field fold numeric, number of fold, i.e. number of forest (k)
#' @field ratio numeric experimental, use to balance sampsize. Defaut to 1.
#' @field groups vector Define which observations belong to which group
#' @field ... field from parent class : \code{\link{VT.forest}}
#'   
#' @name VT.forest.fold
#'   
#' @seealso \code{\link{VT.difft}}, \code{\link{VT.forest}}, 
#'   \code{\link{VT.forest.one}}, \code{\link{VT.forest.double}}
#'   
#' @import methods
#' 
#' @export VT.forest.fold
#' 

VT.forest.fold <- setRefClass(
  Class = "VT.forest.fold",
  
  contains = "VT.forest",
  
  fields = list(
    interactions = "logical",
    fold = "numeric",
    ratio = "numeric",
    groups = "vector"
  ),
  
  methods = list(
    initialize = function(vt.object, fold, ratio, interactions = T, ...){
      
      .self$fold <- fold
      
      .self$ratio <- ratio
      
      .self$interactions <- interactions
      
      callSuper(vt.object, ...)
    },
    
    run = function(parallel = F, ...){
      
      .self$groups <- sample(1:.self$fold, nrow(.self$vt.object$data), replace = T)
      
      for(g in 1:.self$fold){
        .self$runOneForest(g, ...)
      }
      
      .self$computeDifft()
    },
    
    runOneForest = function(group, ...){
      data <- .self$vt.object$getX(interactions = .self$interactions)
      X <- data[.self$groups != group, -1] 
      Y <- .self$vt.object$data[.self$groups != group, 1]
      Yeff <- table(Y) # 1 -> levels(Y)[1] & 2 -> levels(Y)[2]
      sampmin <- min(Yeff[1], Yeff[2])
      
      if(sampmin == Yeff[2]){
        samp2 <- sampmin
        samp1 <- min(Yeff[1], round(.self$ratio*Yeff[1], digits = 0))
      }else{
        samp2 <- Yeff[2]
        samp1 <- sampmin
      }
      if(!requireNamespace("randomForest", quietly = TRUE)) stop("randomForest package must be loaded.")
      rf <- randomForest(x = X, y = Y, sampsize = c(samp1, samp2), keep.forest = T, ...)
      
      .self$computeTwin1(rf, group)
      .self$computeTwin2(rf, group)
    },
    
    computeTwin1 = function(rfor, group){
      
      data <- .self$vt.object$getX(interactions = .self$interactions)
      data <- data[.self$groups == group, -1]
      
      .self$twin1[.self$groups == group] <- VT.predict(rfor = rfor, newdata = data,  type = .self$vt.object$type)
      
      return(invisible(.self$twin1))
    },
    
    computeTwin2 = function(rfor, group){
      
      .self$vt.object$switchTreatment()
      data <- .self$vt.object$getX(interactions = .self$interactions)
      data <- data[.self$groups == group, ]
      
      .self$twin2[.self$groups == group] <- VT.predict(rfor = rfor, newdata = data,  type = .self$vt.object$type)
      
      .self$vt.object$switchTreatment()
      return(invisible(.self$twin2))
    }
  )
)
