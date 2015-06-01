# TREES COMPUTATIONS ------------------------------------------------------
#' An abstract reference class to compute tree
#' 
#' @include difft.R setClass.R
#' 
#' @field vt.difft VT.difft object
#' @field outcome vector
#' @field threshold numeric Threshold for difft (c)
#' @field screening logical TRUE if using varimp (default is VT.object screening field) 
#' @field sens character Sens can be ">" (default) or "<". Meaning : difft > threshold or difft < threshold
#' @field name character Names of the tree
#' @field tree rpart Rpart object to construct the tree
#' @field Ahat vector Indicator of beglonging to Ahat
#'  
VT.tree <- setRefClass(
  Class = "VT.tree",
  
  fields = list(
    vt.difft = "VT.difft",
    outcome = "vector",
    threshold = "numeric",
    screening = "logical",
    sens = "character",
    name = "character",
    tree = "rpart",
    Ahat = "vector"
  ),
  
  methods = list(
    initialize = function(vt.difft = VT.difft(), threshold = 0.05, sens = ">", screening = NULL){        
      .self$vt.difft <- vt.difft
      
      .self$threshold <- threshold
      
      .self$sens <- sens
      
      .self$screening <- ifelse(is.null(screening), vt.difft$vt.object$screening, screening)
      
    },
    
    getData = function(){
      d <- .self$vt.difft$vt.object$data[, 3:ncol(.self$vt.difft$vt.object$data)]
      
      if(.self$screening == T){
        d.tmp <- d
        d <- d.tmp[, colnames(d.tmp) %in% .self$vt.difft$vt.object$varimp] # To see later
      }
      
      d <- data.frame(.self$outcome, d)
      names(d) <- c(.self$name, colnames(d)[-1])
      
      return(d)
    },
    
    computeNameOfTree = function(type){
      if(.self$threshold < 0 ){
        threshold.chr <- paste0("m", -.self$threshold)
      }else{
        threshold.chr <- as.character(.self$threshold)        
      }
      
      tmp = strsplit(threshold.chr, "[.]")[[1]]
      return(paste(type, tmp[1], tmp[2], sep = ""))
    },
    
    run = function(){
      if(length(.self$vt.difft$difft) == 0) stop("VT.difft::difft is an empty vector")
    },
    
    getInfos = function(){
      cat("\n")
      cat(sprintf("Threshold = %0.4f", .self$threshold))
      cat("\n")
      cat(sprintf("Delta = %0.4f", .self$vt.difft$vt.object$delta))
      cat("\n")
      cat(sprintf("Sens : %s", .self$sens))
      cat("\n")
      #       cat(sprintf("Bounds = %0.4f", (.self$vt.difft$vt.object$delta + .self$threshold)))
      #       cat("\n")
      cat(sprintf("Size of Ahat : %i", (sum(.self$Ahat))))
      
      return(invisible(NULL))
    },
    
    getRules = function(only.leaf = F, only.fav = F, tables = T, verbose = T){
      
      # On supprime le root node, inutile pour les stats d'incidences et autres...
      full.frame <- .self$tree$frame[-1, ]
      
      if (only.fav == T){  
        if(inherits(.self, "VT.tree.reg")){
          if(.self$sens == ">"){
            frm.only.fav <- full.frame[full.frame$yval >= (.self$threshold), ]
          } else {
            frm.only.fav <- full.frame[full.frame$yval <= (.self$threshold), ]
          }
        }else if(inherits(.self, "VT.tree.class")){
          frm.only.fav <- full.frame[full.frame$yval == 2, ]  
        }
        frm <- frm.only.fav
      }
      
      if (only.leaf == T){
        if(inherits(.self, "VT.tree.reg")){
          frm.only.leaf <- full.frame[full.frame$var == "<leaf>", ]
        }else if(inherits(.self, "VT.tree.class")){
          frm.only.leaf <- full.frame[full.frame$var == "<leaf>", ]  
        }
        frm <- frm.only.leaf
      }
      
      if (only.fav == T & only.leaf == T){
        frm  <- frm.only.leaf[ intersect(rownames(frm.only.leaf), rownames(frm.only.fav)) , ]
      }else if (only.fav == F & only.leaf == F){
        frm  <- full.frame
      }
      
      # Le cas où l'arbre est vide ou n'existe pas:
      if (length(frm) == 0) stop("VT.tree : no tree");
      if (ncol(frm)==0) stop("VT.tree : no rules");
      
      pth <- rpart::path.rpart(.self$tree, nodes = row.names(frm), print.it = F)
      # Delete 'root' node des règles
      pth <- lapply(pth, FUN = function(d) return(d[-1]))
      
      depth <- 0
      nodes <- names(pth)
      rules <- data.frame(replicate(6, character(0), simplify = T), replicate(2, numeric(0), simplify = T), stringsAsFactors = F)
      
      colnames(rules) <- c("Subgroup", "Subgroup size", "Treatement event rate", "Control event rate",
                           "Treatment sample size", "Control sample size", "RR (resub)", "RR (snd)")
      for(i in nodes){
        pth.text <- paste(pth[[i]], collapse = " & ")
        incid <- .self$getIncidences(pth.text)
        
        rules[i, 1] <- pth.text
        rules[i, 2] <- incid$table.selected$table[3, 3] #size subgroupg 
        rules[i, 3] <- incid$table.selected$table[4, 2] #treatment event rate
        rules[i, 4] <- incid$table.selected$table[4, 1] #control event rate
        rules[i, 5] <- incid$table.selected$table[3, 2] #treatment sample size
        rules[i, 6] <- incid$table.selected$table[3, 1] #control sample size
        rules[i, 7] <- round(incid$table.selected$rr, digits = 3) # rr (resub)
        rules[i, 8] <- round(incid$table.selected$rr.snd, digits = 3) # rr (snd)
        
        if(isTRUE(verbose)){
          cat("----------------------------\n")
          cat(sprintf("| Rule number %s : ", i))
          
          
          if(inherits(.self, "VT.tree.reg")){
            cat(sprintf("Y val = %0.3f \n", frm[i, ]$yval))
          }else{
            cat(sprintf("Y val = %i \n", frm[i, ]$yval))
          }
          
          cat("----------------------------\n")
          
          cat(sprintf("[n = %i", frm[i, ]$n))
          cat(sprintf(", loss = %s, prob = %0.2f",
                      frm[i, ]$dev,
                      frm[i, ]$yval2[, 5]))
          
          cat("] \n")        
          cat("\t\t")
          cat(pth[[i]], sep="\n\t\t")
          
          if(isTRUE(tables)){
            cat("\n")
            cat(sprintf("Incidence dans la selection \n"))
            print(incid$table.selected$table)
            cat("\n")
            cat(sprintf("Risque relatif (resub) : %0.3f \n", incid$table.selected$rr))
            cat(sprintf("Risque relatif (snd) : %0.3f \n\n", incid$table.selected$rr.snd))
            
            cat(sprintf("Incidence dans le complementaire\n"))
            print(incid$table.not.selected$table)
            cat("\n")
            cat(sprintf("Risque relatif (resub) : %0.3f \n", incid$table.not.selected$rr))
            cat(sprintf("Risque relatif (snd) : %0.3f \n\n", incid$table.not.selected$rr.snd))
          }
          
          cat("\n\n")
        }
      }
      
      return(invisible(rules))
    },
    
    getIncidences = function(rule, rr.snd = T){
      return(VT.incidences(.self$vt.difft, rule, rr.snd))
    },
    
    getAhatIncidence = function(){
      if(sum(.self$Ahat)!=0){
        
        table.inc <- VT.incidences(vt.object = .self$vt.difft$vt.object, select = .self$Ahat)
        
        table.A <- table.inc$table.selected
        table.A.cmpl <- table.inc$table.not.selected
        
        cat(sprintf("Incidence dans le sous groupe A\n"))
        print(table.A$table)
        cat("\n")
        cat(sprintf("Risque relatif : %0.3f \n\n", table.A$risque_relatif))
        
        cat(sprintf("Incidence dans le sous groupe A complementaire\n"))
        print(table.A.cmpl$table)
        cat("\n")
        cat(sprintf("Risque relatif : %0.3f \n\n", table.A.cmpl$risque_relatif))
      }else{
        return("Empty set")
      }
    },
    
    getAhatQuality = function(){
      
      resub <- vt.getQAOriginal(.self$Ahat, response = .self$vt.difft$vt.object$getY(), trt = .self$vt.difft$vt.object$data[, 2])
      
      snd <- vt.getQAOriginal(.self$Ahat, response = .self$vt.difft$twin1, trt = .self$vt.difft$vt.object$data[, 2])
      
      # on ajoute la taille de Ahat
      size <- sum(.self$Ahat)
      
      res <- cbind(size, resub, snd)
      names(res) <- c("size", "resub", "snd")
      
      return(res)
    }
  )
)
