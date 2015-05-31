# PREDICTION --------------------------------------------------------------

# LES METHODES SUIVANTES PERMETTENT DE PREDIRE LA PROBA D'INTERET POUR 
# LES TROIS CLASSES SUIVANTES : train, randomForest, RandomForest{party}

setGeneric("VT.predict",
           function(rfor, newdata, type){standardGeneric("VT.predict")}
)

setMethod(
  f = "VT.predict",
  signature = c(rfor = "RandomForest", newdata = "missing", type = "character"),
  function(rfor, type = "binary"){
    if(! type %in% c("binary", "continous")) stop("Type must be Binary or continous")
    if(type == "binary"){
      tmp <- predict(rfor, OOB = T, type = "prob")      
      tmp <- unlist(tmp)
      tmp <- tmp[seq(2, length(tmp), 2)]      
    }else{
      message("continous is not done yet")
      tmp <- NULL
    }
    
    return(tmp)
  }
)

setMethod(
  f = "VT.predict",
  signature = c(rfor = "RandomForest", newdata = "data.frame", type = "character"),
  function(rfor, newdata, type = "binary"){
    if(! type %in% c("binary", "continous")) stop("Type must be Binary or continous")
    if(type == "binary"){
      tmp <- predict(rfor, newdata = newdata, type = "prob")
      tmp <- unlist(tmp)
      tmp <- tmp[seq(2, length(tmp), 2)]      
    }else{
      message("continous is not done yet")
      tmp <- NULL
    }
    
    return(tmp)
  }
)

setMethod(
  f = "VT.predict",
  signature = c(rfor = "randomForest", newdata = "missing", type = "character"),
  function(rfor, type = "binary"){
    if(! type %in% c("binary", "continous")) stop("Type must be Binary or continous")
    if(type == "binary"){
      # no longer available in all version ?!
      # tmp <- rfor$vote[, 2] # get the "o" prob
      tmp <- predict(rfor, type = "prob")[, 2] # We want to get the "o" prob
    }else{
      message("continous is not done yet")
      tmp <- NULL
    }
    return(tmp)
  }
)

setMethod(
  f = "VT.predict",
  signature = c(rfor = "randomForest", newdata = "data.frame", type = "character"),
  function(rfor, newdata, type = "binary"){
    if(! type %in% c("binary", "continous")) stop("Type must be Binary or continous")
    if(type == "binary"){
      tmp <- predict(rfor, newdata = newdata, type = "prob")[, 2] # We want to get the "o" prob
    }else{
      message("continous is not done yet")
      tmp <- NULL
    }
    return(tmp)
  }
)

setMethod(
  f = "VT.predict",
  signature = c(rfor = "train", newdata = "ANY", type = "character"),
  function(rfor, newdata, type = "binary"){
    return(VT.predict(rfor$finalModel, newdata, type))
  }
)

setMethod(
  f = "VT.predict",
  signature = c(rfor = "train", newdata = "missing", type = "character"),
  function(rfor, type = "binary"){
    return(VT.predict(rfor=rfor$finalModel, type=type))
  }
)