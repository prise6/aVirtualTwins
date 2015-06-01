# VT.OBJECT ---------------------------------------------------------------

#' A Reference Class to deal with RCT dataset
#' 
#' @field data A data.frame de la forme \eqn{Y,T,X_{1}, \ldots, X_{p}}. Y must
#'   be two levels factor if type is binary. T must be numeric or integer.
#' @field alpha no usefull now, set to 1
#' @field screening logical, set to FALSE. Se TRUE to use varimp in trees
#'   computation
#' @field varimp character vector of important variables to use in trees
#'   computation
#' @field delta numeric representing the difference of incidence between
#'   treatments
#' @field type character : binary or continous. Only binary is possible.
#' 
#' @import methods
VT.object <- setRefClass(
  Class = "VT.object",
  
  fields = list(
    data = "data.frame",
    alpha = "numeric",
    screening = "logical",
    varimp = "character",
    delta = "numeric",
    type = "character"
  ),
  
  methods = list(
    initialize = function(screening = F, alpha = 1, type = "binary", ...){
      
      .self$screening <- screening
      
      .self$type <- type
      
      .self$alpha <- alpha
      
      .self$initFields(...)
    },
    
    getFormula = function(){
      "Return formula : Y~T+X1+...+Xp. Usefull for cforest function."
      return(as.formula(paste(colnames(.self$data)[1], ".", sep = "~")))
    },
    
    getX = function(interactions = T, trt = NULL){
      "Return predictors {T,X,X*T,X*(1-T)}. Or {T,X} if interactions is FALSE.
        If trt is not NULL, return predictors for T=trt"
      # retour les prédicteurs si trt n'est pas null
      if(!is.null(trt)) return(.self$data[.self$data[,2] == trt, -c(1,2)])
      # retourne les predicteurs*traitement peut importe le traitement si interactions est à TRUE
      if(interactions == T) return(.self$getXwithInt())
      # retourne les predicteurs
      return(.self$data[, -1])
    },
    
    getY = function(trt = NULL){
      "Return outcome. If trt is not NULL, return outcome for T=trt."
      if(is.null(trt)) return(.self$data[, 1])
      return(.self$data[.self$data[,2] == trt, 1])
    },
    
    getXwithInt = function(){
      "Return predictors with interactions. Use VT.object::getX(interactions = T) instead."
      tmp <- .self$data[, -c(1,2)]
      return(data.frame(cbind(.self$data[,-1], tmp*.self$data[, 2], tmp*(1 - .self$data[, 2]))))
    },
    
    switchTreatment = function(){
      "Switch treatment value."
      cl <- class(.self$data[, 2])
      # Treatments must be numeric or integer and binary
      .self$data[, 2] <- 1 - .self$data[, 2]
      # keep original class for treatment
      if(cl == "integer"){
        .self$data[, 2] <- as.integer(.self$data[, 2])
      }else{
        .self$data[, 2] <- as.numeric(.self$data[, 2])
      }
      cat("witch \n")
      return(TRUE)
    },
    
    computeDelta = function(){
      "Compute delta value."
      if(.self$type == "binary"){
        .self$delta <- sum((as.numeric(.self$data[, 1]) - 1)*(.self$data[, 2])) / sum(.self$data[, 2]) -
          sum((as.numeric(.self$data[, 1]) - 1)*(1 - .self$data[, 2])) / sum(1 - .self$data[, 2])
        
        return(.self$delta)
      }else{
        stop("Error : type is not Binary")  
      }
    },
    
    getIncidences = function(){
      "Return incidence table of data."
      return(vt.getIncidence(.self$data))
    }
  )
)
