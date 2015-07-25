
#' Trees to find Subgroups
#' 
#' A wrapper of class VT.tree.xxx
#'
#' 
#' See \code{\link{VT.tree}}
#' 
#' @param tree.type character "class" for classification tree, "reg" for regression tree
#' @param vt.difft \code{\link{VT.difft}} object
#' @param sens character c(">","<"). See details.
#' @param threshold numeric It can be a unique value or a vector
#' 
#' @return \code{VT.tree} or a list of \code{VT.tree} depending on threshold dimension
#' 
#' @include tree.R
#' 
#' @name vt.tree
#' 
#' @export vt.tree

vt.tree <- function(tree.type = "class", vt.difft, sens = ">", threshold = seq(.5, .8, .1), screening = NULL, ...){
  if(!inherits(vt.difft, "VT.difft"))
    stop("vt.difft parameter must be aVirtualTwins::VT.difft class")
  if(is.numeric(threshold)){
    if(length(threshold)>1){
      res.name <- paste0("tree", 1:length(threshold))
      res.list <- lapply(X = threshold, FUN = vt.tree, tree.type = tree.type, vt.difft = vt.difft, sens = sens, screening = screening, ...)
      names(res.list) <- res.name
      return(res.list)
    }else{
      if(tree.type == "class")
        tree <- aVirtualTwins:::VT.tree.class(vt.difft = vt.difft, sens = sens, threshold = threshold, screening = screening)
      else
        tree <- aVirtualTwins:::VT.tree.reg(vt.difft = vt.difft, sens = sens, threshold = threshold, screening = screening)
      
      tree$run(...)
      
      return(tree)
    }
  }else
    stop("threshold must be numeric")
}
