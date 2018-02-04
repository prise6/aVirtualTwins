
#' Visualize subgroups
#' 
#' Function which uses \code{\link{VT.tree}} intern functions. Package 
#' rpart.plot must be loaded. See \code{\link{VT.tree}} for details.
#' 
#' @param vt.trees \code{\link{VT.tree}} object. Or return of 
#'   \code{\link{vt.tree}} function. Can be a list.
#' @param only.leaf logical to select only leaf of trees. TRUE is default.
#' @param only.fav logical select only favorable subgroups (meaning with 
#'   favorable label of the tree). TRUE is default.
#' @param tables set to TRUE if tables of incidence must be shown. FALSE is 
#'   default.
#' @param verbose print infos during computation. FALSE is default.
#' @param compete print competitors rules thanks to competitors computation of
#'   the tree
#'   
#' @return data.frame of rules
#'   
#' @examples 
#' data(sepsis)
#' vt.o <- vt.data(sepsis, "survival", "THERAPY", TRUE)
#' # inside model :
#' vt.f <- vt.forest("one", vt.o)
#' # use classification tree
#' vt.tr <- vt.tree("class", vt.f, threshold = c(0.01, 0.05))
#' # show subgroups
#' subgroups <- vt.subgroups(vt.tr)
#' # change options you'll be surprised !
#' subgroups <- vt.subgroups(vt.tr, verbose = TRUE, tables = TRUE)
#' 
#' @export vt.subgroups
#'   
#' @name vt.subgroups
#'

vt.subgroups <- function(vt.trees, only.leaf = T, only.fav = T, tables = F, verbose = F, compete = F){
  
  if(is.list(vt.trees)){
    subgroups <- lapply(vt.trees, function(x)x$getRules(only.leaf = only.leaf, only.fav = only.fav, tables = tables, verbose = verbose, compete = F))
    unique(do.call(rbind, subgroups))
  }
  else{
    subgroups <- vt.trees$getRules(only.leaf = only.leaf, only.fav = only.fav, tables = tables, verbose = verbose, compete = compete)
  }
}

vt.getQAOriginal <- function(response, trt, ahat){
  if(is.factor(response)) response = as.numeric(response) - 1
  
  if(sum(ahat) == 0){
    tmp <- 0
  }else{
    tmp <- sum(response*ahat*trt)/sum(ahat*trt) - 
      sum(response*ahat*(1-trt))/sum(ahat*(1-trt)) - 
      (sum(response*trt)/sum(trt) - 
         sum(response*(1-trt))/sum(1-trt))  
 } 
  return(tmp)
}

vt.getTable <- function(table){
  if(is.list(table)) table <- table[[1]]
  Incidence <- function(X) round(X[2] / X[3], digits = 3)
  t <- stats::addmargins(table, margin = c(1,2), FUN = sum, quiet = T)
  t <- stats::addmargins(t, FUN = Incidence, margin = 1, quiet = T)
  rr <- NA_real_
  if(nrow(t) == 4) rr <- t[4, 2] / t[4, 1]
  return(list(table = t, rr = rr))
}

vt.getIncidence <- function(df){
  if (nrow(df) == 0) table.res <- NULL
  if (ncol(df) != 2) table.res <- NULL
  else{
    table.res <- vt.getTable(
      table(
        factor(df[, 1], levels = c(0, 1)),
        factor(df[, 2], levels = c(0, 1)),
        deparse.level = 2,
        dnn = c("resp", "trt")
      )
    )
  }
  return(table.res)
}

vt.rr.snd <- function(vt.difft, selected){
  if(sum(selected) == 0){
    return(0)
  }else{
    return((sum(vt.difft$twin1*selected*vt.difft$vt.object$data[, 2])/sum(selected*vt.difft$vt.object$data[, 2]))
           /(sum(vt.difft$twin1*selected*(1-vt.difft$vt.object$data[, 2]))/sum(selected*(1-vt.difft$vt.object$data[, 2]))))
  }
}



