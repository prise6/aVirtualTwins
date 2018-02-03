
#' 
#' Create forest to compute difft
#' 
#' \code{vt.forest} is a wrapper of \code{\link{VT.forest.one}}, 
#' \code{\link{VT.forest.double}} and \code{\link{VT.forest.fold}}. With 
#' parameter forest.type, any of these class can be used with its own parameter.
#' 
#' @param forest.type must be a character. "one" to use VT.forest.one class. 
#'   "double" to use VT.forest.double. "fold" to use VT.forest.fold.
#' @param vt.data \code{\link{VT.object}}. Can be return of \code{vt.data()} 
#'   function
#' @param interactions logical. If running VirtualTwins with treatment's 
#'   interactions, set to TRUE (default value)
#' @param method character c("absolute", "relative", "logit"). See 
#'   \code{\link{VT.difft}}.
#' @param model allows to give a model you build outside this function. Can be 
#'   randomForest, train or cforest. Is only used with forest.type = "one". If 
#'   NULL, a randomForest model is grown inside the function. NULL is default.
#' @param model_trt0 works the same as model parameter. Is only used with 
#'   forest.type = "double". If NULL, a randomForest model is grown inside the 
#'   function. NULL is default. See \code{\link{VT.forest.double}} for details.
#' @param model_trt1 see model_trt0 explanation and 
#'   \code{\link{VT.forest.double}} details.
#' @param fold number of fold you want to construct forest with k-fold method. 
#'   Is only used with forest.type = "fold". Default to 5. See 
#'   \code{\link{VT.forest.fold}}
#' @param ratio numeric value that allow sampsize to be a bit controlled.
#'   Default to 1. See \code{\link{VT.forest.fold}}.
#' @param ... randomForest() function parameters. Can be used for any forest.type.
#'   
#' @return \code{VT.difft}
#'  
#' @examples
#' 
#' data(sepsis)
#' vt.o <- vt.data(sepsis, "survival", "THERAPY", T)
#' # inside model :
#' vt.f <- vt.forest("one", vt.o)
#' # ...
#' # your model :
#' # library(randomForest)
#' # rf <- randomForest(y = vt.o$getY(),
#' #                    x = vt.o$getX(int = T),
#' #                    mtry = 3,
#' #                    nodesize = 15)
#' # vt.f <- vt.forest("one", vt.o, model = rf)
#' # ...
#' # Can also use ... parameters
#' vt.f <- vt.forest("one", vt.o, mtry = 3, nodesize = 15)
#' # ...
#'    
#' 
#' @include forest.R difft.R
#'   
#' @name vt.forest
#'   
#' @export vt.forest
vt.forest <- function(forest.type = "one", vt.data, interactions = T, method = "absolute", 
                      model = NULL, model_trt1 = NULL, model_trt0 = NULL, ratio = 1, fold = 10, ...){
  if(!inherits(vt.data, "VT.object"))
    stop("vt.data must be VT.object class")
  
  params <- list(...)
  if (forest.type == "one"){
    if(is.null(model)){
      model <- randomForest::randomForest(x = vt.data$getX(interactions = interactions, trt = NULL),
                         y = vt.data$getY(), 
                         ...)       
    }
    rf <- model
    
    vt.difft <- VT.forest.one(vt.object = vt.data, model = rf, interactions = interactions, method = method)
  } else if (forest.type == "double"){
    if(is.null(model_trt1)){
      model_trt1 <- randomForest::randomForest(x = vt.data$getX(trt = 1),
                                  y = vt.data$getY(1),
                                 ...)
    }
    rf_trt1 <- model_trt1
    
    if(is.null(model_trt0)){
      model_trt0 <- randomForest::randomForest(x = vt.data$getX(trt = 0),
                                  y = vt.data$getY(0),
                                  ...)
    }
    rf_trt0 <- model_trt0
    
    vt.difft  <- VT.forest.double(vt.object = vt.data, model_trt1 = rf_trt1, model_trt0 = rf_trt0, method = method)
  } else if (forest.type == "fold"){
    vt.difft <- VT.forest.fold(vt.object = vt.data, fold = fold, ratio = ratio, 
                                               interactions = interactions, method = method)
    
  } else
    stop("forest.type must be one, double or fold")
  if(forest.type %in% c("one", "double"))
    vt.difft$run()
  else
    vt.difft$run(...)
  return(vt.difft)
} 

