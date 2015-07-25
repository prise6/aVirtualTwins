
#' 
#' Create forest to compute difft
#' 
#' \code{vt.forest} is a wrapper of \code{\link{VT.forest.one}},
#' \code{\link{VT.forest.double}} and \code{\link{VT.forest.fold}}.
#' 
#' @param forest.type character one / double / fold
#' @param vt.data \code{\link{VT.data}} or return of \code{vt.data()} function
#' @param interactions logical. If running VirtualTwins with treatment's 
#'   interactions, set to TRUE (default value)
#' @param method character absolute / relative / logit
#' @param ... parameters of \code{\link{VT.difft}} or \code{\link{VT.forest}}  
#'   
#' @return \code{VT.difft}
#'
#' @include forest.R difft.R
#' 
#' @name vt.forest
#' 
#' @export vt.forest

vt.forest <- function(forest.type = "one", vt.data, interactions = T, method = "absolute", ...){
  if(!inherits(vt.data, "VT.object"))
    stop("vt.data must be VT.object class")
  
  params <- list(...)
  if (forest.type == "one"){
    if(! "rf" %in% names(params) ){
      rf <- randomForest(x = vt.data$getX(interactions = interactions, trt = NULL),
                         y = vt.data$getY(), 
                         ...)       
    } else{
      rf <- params["rf"]
    }
    
    vt.difft <- VT.forest.one(vt.object = vt.data, model = rf, interactions = interactions, method = method)
  } else if (forest.type == "double"){
    if(! "model_trt1" %in% names(params) ){
      rf_trt1 <- randomForest(x = vt.data$getX(trt = 1, interactions = interactions),
                              y = vt.data$getY(1),
                              ...)
    } else
      rf_trt1 <- params["model_trt1"]  
    
    if(! "model_trt0" %in% names(params) ){
      rf_trt0 <- randomForest(x = vt.data$getX(trt = 1, interactions = interactions),
                              y = vt.data$getY(1),
                              ...)
    } else
      rf_trt0 <- params["rf_trt0"]
    
    vt.difft  <- VT.forest.double(vt.object = vt.data, model_trt1 = rf_trt1, model_trt0 = rf_trt0, method = method, ...)
  } else if (forest.type == "fold"){
    fold <- ifelse(! "fold" %in% names(params) , 5, params["fold"])
    fold <- ifelse(! "ratio" %in% names(params) , 1, params["ratio"])
    vt.difft <- aVirtualTwins:::VT.forest.fold(vt.object = vt.data, fold = fold, ratio = ratio, 
                                               interactions = interactions, method = method)
    
  } else
    stop("forest.type must be one, double or fold")
  if(forest.type %in% c("one", "double"))
    vt.difft$run()
  else
    vt.difft$run(...)
  return(vt.difft)
} 

