# FORESTS -----------------------------------------------------------------

# Objet enfant de VT.difft
# Objet Parent de VT.forest.one & VT.forest.double, Abstract class : ne doit pas etre instanciée
# twin1 - Proba du "oui" de la réponse (modalité d'intéret codé par "o" ou 1 contre "n" ou 0 dans le cas binaire)
#  sachant le vrai traitement
# twin2 - Proba du "oui" [...] sachant le traitement opposé
# difft - différence de twin1 - twin2 SI le vrai traitement == 1 SINON twin2 - twin1
#
# $run() - lance le calcul des probas
VT.forest <- setRefClass(
  Class = "VT.forest",
  
  contains = "VT.difft",
  
  methods = list(  
    run = function(){
      .self$computeTwin1()
      
      if(inherits(.self, "VT.forest.one")) .self$vt.object$switchTreatment() #if one forest
      
      .self$computeTwin2()
      
      if(inherits(.self, "VT.forest.one")) .self$vt.object$switchTreatment() #if one forest
      
      .self$computeDifft()
      
      .self$vt.object$computeDelta() # To see later
      
      return(invisible(.self))
    },
    
    checkModel = function(model){
      if(!(inherits(model, "train") | inherits(model, "RandomForest") | inherits(model, "randomForest"))){
        stop("Model is not recognized. Must be : train, RandomForest, randomForest")
      }
    },
    
    getFullData = function(){
      if(length(.self$twin1) != nrow(.self$vt.object$data)) stop("Twin1 must have same length as data")
      if(length(.self$twin2) != nrow(.self$vt.object$data)) stop("Twin2 must have same length as data")
      
      if(length(.self$difft) != nrow(.self$vt.object$data)) stop("Difft must have same length as data")
      
      tmp <- cbind(.self$vt.object$data, .self$twin1, .self$twin2, .self$difft)
      
      colnames(tmp) <- c(colnames(.self$vt.object$data), "twin1", "twin2", "difft")
      
      return(tmp)
    }
  )
)
