# VT.OBJECT ---------------------------------------------------------------

#' VT.object
#' 
#' A Reference Class to deal with RCT dataset
#' 
#' Currently working with binary response only. Continous will come, one day. 
#' Two-levels treatment only as well.
#' 
#' \code{data} field should be as described, however if virtual twins won't used
#' interactions, there is no need to transform factors. See
#' \link{formatRCTDataset} for more details.
#' 
#' 
#' @field data Data.frame with format: \eqn{Y,T,X_{1}, \ldots, X_{p}}. Y must be
#'   two levels factor if type is binary. T must be numeric or integer.
#' @field screening Logical, set to \code{FALSE} Set to \code{TRUE} to use
#'   \code{varimp} in trees computation.
#' @field varimp Character vector of important variables to use in trees 
#'   computation.
#' @field delta Numeric representing the difference of incidence between 
#'   treatments.
#' @field type Character : binary or continous. Only binary is currently 
#'   available.
#'   
#' @import methods
#'   
#' @name VT.object
#'   
#' @export VT.object
#'   
#' @examples
#' \dontrun{
#' # Default use :
#' vt.o <- VT.object$new(data = my.rct.dataset)
#' 
#' # Getting data
#' head(vt.o$data)
#' 
#' # or getting predictor with interactions
#' vt.o$getX(interactions = T)
#' 
#' # or getting X|T = 1
#' vt.o$getX(trt = 1)
#' 
#' # or getting Y|T = 0
#' vt.o$getY(0)
#' 
#' # Print incidences
#' vt.o$getIncidences()
#' }
#' 
#' @seealso \code{\link{VT.difft}}
#'   
VT.object <- setRefClass(
  Class = "VT.object",
  
  fields = list(
    data = "data.frame",
    screening = "logical",
    varimp = "character",
    delta = "numeric",
    type = "character"
  ),
  
  methods = list(
    initialize = function(screening = F, type = "binary", ...){
      
      .self$screening <- screening
      
      .self$type <- type
            
      .self$initFields(...)
    },
    
    getFormula = function(){
      "Return formula : Y~T+X1+...+Xp. Usefull for cforest function."
      return(as.formula(paste(colnames(.self$data)[1], ".", sep = "~")))
    },
    
    getX = function(interactions = T, trt = NULL){
      "Return predictors (T,X,X*T,X*(1-T)). Or (T,X) if interactions is FALSE.
        If trt is not NULL, return predictors for T = trt"
      # predictors if trt is not null
      if(!is.null(trt)) return(.self$data[.self$data[,2] == trt, -c(1,2)])
      # predictor*treatment no matter trt if interactions is TRUE
      if(interactions == T) return(.self$getXwithInt())
      # predictors
      return(.self$data[, -1])
    },
    
    getY = function(trt = NULL){
      "Return outcome. If trt is not NULL, return outcome for T = trt."
      if(is.null(trt)) return(.self$data[, 1])
      return(.self$data[.self$data[,2] == trt, 1])
    },
    
    getXwithInt = function(){
      "Return predictors with interactions. Use VT.object::getX(interactions = T) instead."
      tmp <- .self$data[, -c(1,2)]
      return(data.frame(cbind(.self$data[,-1], tmp*.self$data[, 2], tmp*(1 - .self$data[, 2]))))
    },
    
    getData = function(interactions = F){
      "Return dataset. If interactions is set to T, return data with treatement interactions"
      if(!isTRUE(interactions))
        return(.self$data)
      else{
        data.int <- cbind(.self$data[, 1], .self$getX(T))
        colnames(data.int)[1] <- colnames(.self$data)[1]
        return(data.int)
      }
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
    
    # Hack of VT.incidences
    getIncidences = function(rule = NULL){
      "Return incidence table of data if rule set to NULL. Otherwise return incidence for the rule."
      hack.difft <- VT.difft$new(.self)
      if(is.null(rule))  
        return(vt.getIncidence(.self$data))
      else
        return(VT.incidences(hack.difft, rule, F))
    }
  )
)
