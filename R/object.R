# VT.OBJECT ---------------------------------------------------------------

# Permet de stocker les données
# alpha - paramètre inutile dans cette version
# screening & varimp permettent de construire des arbres sur les variables
#   définies dans varimp si screening = True
# delta - différence d'incidence entre les deux "bras"
# type - type de réponse - binary ou continous - seul binary est disponible
# interactions - si TRUE getX() retourne (X,X*T,X*(1-T))
#
# $getFormula() - utile pour retourner une formule pour rpart
# $getX(trt = c(0,1,NULL), interactions = c(TRUE, FALSE)) - si trt est non NULL
#   getX() retourne les lignes pour le traitement passé paramètre (utile pour les doubles forests)
# $getY() - retourne la réponse / cible
# ...
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
      return(as.formula(paste(colnames(.self$data)[1], ".", sep = "~")))
    },
    
    getX = function(interactions = T, trt = NULL){
      # retour les prédicteurs si trt n'est pas null
      if(!is.null(trt)) return(.self$data[.self$data[,2] == trt, -c(1,2)])
      # retourne les predicteurs*traitement peut importe le traitement si interactions est à TRUE
      if(interactions == T) return(.self$getXwithInt())
      # retourne les predicteurs
      return(.self$data[, -1])
    },
    
    getY = function(trt = NULL){
      if(is.null(trt)) return(.self$data[, 1])
      return(.self$data[.self$data[,2] == trt, 1])
    },
    
    getXwithInt = function(){
      tmp <- .self$data[, -c(1,2)]
      return(data.frame(cbind(.self$data[,-1], tmp*.self$data[, 2], tmp*(1 - .self$data[, 2]))))
    },
    
    switchTreatment = function(){
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
      if(.self$type == "binary"){
        .self$delta <- sum((as.numeric(.self$data[, 1]) - 1)*(.self$data[, 2])) / sum(.self$data[, 2]) -
          sum((as.numeric(.self$data[, 1]) - 1)*(1 - .self$data[, 2])) / sum(1 - .self$data[, 2])
        
        return(.self$delta)
      }else{
        stop("Error : type is not Binary")  
      }
    },
    
    getIncidences = function(){
      return(vt.getIncidence(.self$data))
    }
  )
)
