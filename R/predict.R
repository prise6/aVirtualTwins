# PREDICTION --------------------------------------------------------------

# LES METHODES SUIVANTES PERMETTENT DE PREDIRE LA PROBA D'INTERET POUR 
# LES TROIS CLASSES SUIVANTES : train, randomForest, RandomForest{party}


#' VT.predict generic function
#' 
#' @param rfor random forest model. Can be train, randomForest or RandomForest
#'   class.
#' @param newdata Newdata to predict by the random forest model. If missing, OOB
#'   predictions are returned.
#' @param type Must be binary or continous, depending on the outcome. Only
#'   binary is really available.
#'   
#' @return vector \eqn{E(Y=1)}
#'   
#'
#' @include setClass.R
#' @importClassesFrom party RandomForest
#' 
#' @name VT.predict
#' 
setGeneric("VT.predict",
           function(rfor, newdata, type){standardGeneric("VT.predict")}
)

#' @describeIn VT.predict rfor(RandomForest) newdata (missing) type (character)
setMethod(
  f = "VT.predict",
  signature = c(rfor = "RandomForest", newdata = "missing", type = "character"),
  function(rfor, type = "binary"){
    if(! type %in% c("binary", "continous")) stop("Type must be Binary or continous")
    if(type == "binary"){
      if(!requireNamespace("party", quietly = TRUE)) stop("Party package must be loaded.")
      tmp <- stats::predict(rfor, OOB = T, type = "prob")      
      tmp <- unlist(tmp)
      tmp <- tmp[seq(2, length(tmp), 2)]      
    }else{
      message("continous is not done yet")
      tmp <- NULL
    }
    
    return(tmp)
  }
)

#' @describeIn VT.predict rfor(RandomForest) newdata (data.frame) type (character)
setMethod(
  f = "VT.predict",
  signature = c(rfor = "RandomForest", newdata = "data.frame", type = "character"),
  function(rfor, newdata, type = "binary"){
    if(! type %in% c("binary", "continous")) stop("Type must be Binary or continous")
    if(type == "binary"){
      if(!requireNamespace("party", quietly = TRUE)) stop("Party package must be loaded.")
      tmp <- stats::predict(rfor, newdata = newdata, type = "prob")
      tmp <- unlist(tmp)
      tmp <- tmp[seq(2, length(tmp), 2)]      
    }else{
      message("continous is not done yet")
      tmp <- NULL
    }
    
    return(tmp)
  }
)

#' @describeIn VT.predict rfor(randomForest) newdata (missing) type (character)
setMethod(
  f = "VT.predict",
  signature = c(rfor = "randomForest", newdata = "missing", type = "character"),
  function(rfor, type = "binary"){
    if(! type %in% c("binary", "continous")) stop("Type must be Binary or continous")
    if(type == "binary"){
      # no longer available in all version ?!
      # tmp <- rfor$vote[, 2] # get the "o" prob
      if(!requireNamespace("randomForest", quietly = TRUE)) stop("randomForest package must be loaded.")
      tmp <- stats::predict(rfor, type = "prob")[, 2] # We want to get the "o" prob
    }else{
      message("continous is not done yet")
      tmp <- NULL
    }
    return(tmp)
  }
)

#' @describeIn VT.predict rfor(randomForest) newdata (data.frame) type (character)
setMethod(
  f = "VT.predict",
  signature = c(rfor = "randomForest", newdata = "data.frame", type = "character"),
  function(rfor, newdata, type = "binary"){
    if(! type %in% c("binary", "continous")) stop("Type must be Binary or continous")
    if(type == "binary"){
      if(!requireNamespace("randomForest", quietly = TRUE)) stop("randomForest package must be loaded.")
      tmp <- stats::predict(rfor, newdata = newdata, type = "prob")[, 2] # We want to get the "o" prob
    }else{
      message("continous is not done yet")
      tmp <- NULL
    }
    return(tmp)
  }
)

#' @describeIn VT.predict rfor(train) newdata (ANY) type (character)
setMethod(
  f = "VT.predict",
  signature = c(rfor = "train", newdata = "ANY", type = "character"),
  function(rfor, newdata, type = "binary"){
    if(!requireNamespace("caret", quietly = TRUE)) stop("caret package must be loaded.")
    return(VT.predict(rfor$finalModel, newdata, type))
  }
)

#' @describeIn VT.predict rfor(train) newdata (missing) type (character)
setMethod(
  f = "VT.predict",
  signature = c(rfor = "train", newdata = "missing", type = "character"),
  function(rfor, type = "binary"){
    if(!requireNamespace("caret", quietly = TRUE)) stop("caret package must be loaded.")
    return(VT.predict(rfor=rfor$finalModel, type=type))
  }
)