#' Tree to find subgroup
#' 
#' An abstract reference class to compute tree
#' 
#' \code{VT.tree.class} and \code{VT.tree.reg} are children of \code{VT.tree}.
#' \code{VT.tree.class} and \code{VT.tree.reg} try to find a strong association 
#' between \code{difft} (in \code{VT.difft} object) and RCT variables.
#' 
#' In \code{VT.tree.reg}, a regression tree is computed on \code{difft} values. 
#' Then, thanks to the \code{threshold} it flags leafs of the \code{tree} which 
#' are above the \code{threshold} (when \code{sens} is ">"). Or it flags leafs 
#' which are below the \code{threshold} (when \code{sens} = "<").
#' 
#' In \code{VT.tree.class}, it first flags \code{difft} above or below 
#' (depending on the \code{sens}) the given \code{threshold}. Then a 
#' classification tree is computed to find which variables explain flagged 
#' \code{difft}.
#' 
#' To sum up, \code{VT.tree} try to understand which variables are associated 
#' with a big change of \code{difft}.
#' 
#' Results are shown with \code{getRules()} function. \code{only.leaf} parameter
#' allows to obtain only the leaf of the \code{tree}. \code{only.fav} parameter 
#' select only favorable nodes. \code{tables} shows incidence table of the rule.
#' \code{verbose} allow \code{getRules()} to be quiet. And \code{compete} show
#' also rules with \code{maxcompete} competitors from the \code{tree}.
#' 
#' @include difft.R setClass.R
#'   
#' @field vt.difft \code{VT.difft} object
#' @field outcome outcome vector from \code{rpart} function
#' @field threshold numeric Threshold for difft calculation (c)
#' @field screening Logical. TRUE if using varimp. Default is VT.object 
#'   screening field
#' @field sens character Sens can be ">" (default) or "<". Meaning : 
#'   \code{difft} > \code{threshold} or \code{difft} < \code{threshold}
#' @field name character Names of the tree
#' @field tree rpart Rpart object to construct the tree
#' @field Ahat vector Indicator of beglonging to Ahat
#'   
#' @seealso \code{\link{VT.tree.reg}}, \code{\link{VT.tree.class}}
#'   
#' @name VT.tree
#'   
#' @import methods
#' 
#' @rdname VT.tree-abstract
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
    competitors = "data.frame",
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
      "Return data used for tree computation"
      d <- .self$vt.difft$vt.object$data[, 3:ncol(.self$vt.difft$vt.object$data)]
      
      if(.self$screening == T){
        d.tmp <- d
        d <- d.tmp[, colnames(d.tmp) %in% .self$vt.difft$vt.object$varimp]
      }
      
      d <- data.frame(.self$outcome, d)
      names(d) <- c(.self$name, colnames(d)[-1])
      
      return(d)
    },
    
    computeNameOfTree = function(type){
      "return label of response variable of the tree"
      return(type)
      if(.self$threshold < 0 ){
        threshold.chr <- paste0("m", -.self$threshold)
      }else{
        threshold.chr <- as.character(.self$threshold)        
      }
      
      tmp = strsplit(threshold.chr, "[.]")[[1]]
      return(paste(type, tmp[1], tmp[2], sep = ""))
    },
    
    run = function(...){
      "Compute tree with rpart parameters"
      if(length(.self$vt.difft$difft) == 0) stop("VT.difft::difft is an empty vector")
    },
    
    getInfos = function(){
      "Return infos about tree"
      cat("\n")
      cat(sprintf("Threshold = %0.4f", .self$threshold))
      cat("\n")
      cat(sprintf("Delta = %0.4f", .self$vt.difft$vt.object$delta))
      cat("\n")
      cat(sprintf("Sens : %s", .self$sens))
      cat("\n")
      cat(sprintf("Size of Ahat : %i", (sum(.self$Ahat))))
      
      return(invisible(NULL))
    },
    
    getRules = function(only.leaf = F, only.fav = F, tables = T, verbose = T, compete = F){
      "Return subgroups discovered by the tree. See details."
      
      # No tree ?
      if (length(.self$tree) == 0){
        warning("VT.tree : no tree"); return(invisible(NULL));
      }
      
      # No rules ?
      if(nrow(.self$tree$frame) < 2){
        warning("VT.tree : no nodes"); return(invisible(NULL));
      }
      
      # delete root node
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
      
      # in case tree is empty or doesn't exist
      if (ncol(frm) == 0){
        warning("VT.tree : no rules"); return(invisible(NULL));
      }
      
      pth <- rpart::path.rpart(.self$tree, nodes = row.names(frm), print.it = F)
      # Delete 'root' node of rule
      pth <- lapply(pth, FUN = function(d) return(d[-1]))
      
      nodes <- c()
      if(isTRUE(compete)){
        comp.df <- .self$createCompetitors()
        comp <- comp.df$path    
        for(i in names(pth)){
          tmp <- length(comp[comp == i][-1])
          if(tmp>0){
            tmp <- 1:tmp
            tmp <- paste(i, tmp, sep = ".")
            nodes <- c(nodes, i, tmp)
          }else
            nodes <- c(nodes, i)
        }
      }else
        nodes <- names(pth)
      
      rules <- data.frame(replicate(6, character(0), simplify = T), replicate(2, numeric(0), simplify = T), stringsAsFactors = F)
      
      colnames(rules) <- c("Subgroup", "Subgroup size", "Treatement event rate", "Control event rate",
                           "Treatment sample size", "Control sample size", "RR (resub)", "RR (snd)")
      for(i in nodes){
        is.comp <- FALSE
        if (isTRUE(length(grep("^\\d+\\.\\d+$", i)) > 0)){
          tmp.str <- strsplit(x = i, split = ".", fixed = T)[[1]]
          tmp.path <- as.numeric(tmp.str[1])
          tmp.path.str <- tmp.str[1]
          tmp.comp <- as.numeric(tmp.str[2])
          l <- length(pth[[tmp.path.str]])
          pth.text.c <- c(pth[[tmp.path.str]][-l], comp.df[comp.df$path == tmp.path, ][(tmp.comp+1), "string"])
          pth.text  <- paste(pth.text.c, collapse = " & ") 
          is.comp <- TRUE
        }else{
          pth.text <- paste(pth[[i]], collapse = " & ")
        }
        
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
          
          if(isTRUE(!is.comp)){
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
            
          } else {
            cat("\n----------------------------\n")
            cat("\t\t")
            cat(pth.text.c, sep="\n\t\t")
          }
          
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
    
    createCompetitors = function(){
      "Create competitors table"
      
      fr  <- .self$tree$frame
      fr  <- fr[fr$var != "<leaf>",]
      sp  <- .self$tree$splits
      sp <- as.data.frame(sp)
      sp$var <- row.names(sp)
      row.names(sp) <- NULL
      
      sp$path <- rep(as.numeric(row.names(fr)), (fr$ncompete+fr$nsurrogate+1))
      sp$string <- paste(sp$var, ifelse(sp$ncat == -1L, "<", ">="), round(sp$index, digits = 3))
      
      sp <- with(sp, sp[adj==0, ])
      sp <- with(sp, sp[, -5])
      
      sp.2 <- sp.3 <- sp
      sp.2$path <- sp$path*2
      sp.2$string <- paste(sp.2$var, ifelse(sp.2$ncat == -1L, "<", ">="), round(sp.2$index, digits = 3))
      
      sp.3$path <- sp$path*2+1
      sp.3$string <- paste(sp.3$var, ifelse(sp.3$ncat == -1L, ">=", "<"), round(sp.3$index, digits = 3))
      
      .self$competitors <- rbind(sp.2, sp.3)
      
      return(invisible(.self$competitors))
    },
    
    getIncidences = function(rule, rr.snd = T){
      "Return incidence of the rule"
      return(VT.incidences(.self$vt.difft, rule, rr.snd))
    },
    
    getAhatIncidence = function(){
      "Return Ahat incidence"
      if(sum(.self$Ahat)!=0){
        
        table.inc <- VT.incidences(vt.difft = .self$vt.difft, select = .self$Ahat, F)
        
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
      "Return Ahat quality"
      
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
