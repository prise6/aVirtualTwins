
#' Trees to find Subgroups
#' 
#' \code{vt.tree} is a wrapper of \code{\link{VT.tree.class}} and 
#' \code{\link{VT.tree.reg}}. With parameter tree.type, any of these two class 
#' can be used with its own parameter.
#' 
#' See \code{\link{VT.tree}}, \code{\link{VT.tree.class}} and 
#' \code{\link{VT.tree.reg}} classes.
#' 
#' @param tree.type must be a character. "class" for classification tree, "reg" 
#'   for regression tree.
#' @param vt.difft \code{\link{VT.difft}} object. Or return of 
#'   \code{\link{vt.forest}} function.
#' @param sens must be a character c(">","<"). See \code{\link{VT.tree}} for 
#'   details.
#' @param threshold must be numeric. It can be a unique value or a vector. If 
#'   numeric vector, a list is returned. See \code{\link{VT.tree}} for details.
#' @param screening must be logical. If TRUE, only varimp variables of VT.object
#'   is used to create the tree.
#' @param ... rpart() function parameters. Can be used for any tree.type.
#'   
#' @return \code{VT.tree} or a list of \code{VT.tree} depending on threshold 
#'   dimension. See examples.
#'   
#' @examples
#' \dontrun{
#'  # data(sepsis)
#'  vt.o <- vt.data(sepsis, "survival", "THERAPY", T)
#'  # inside model : 
#'  vt.f <- vt.forest("one", vt.o)
#'  # use classification tree
#'  vt.tr <- vt.tree("class", vt.f, threshold = c(0.01, 0.05))
#'  # return a list
#'  class(vt.tr)
#'  # access one of the tree
#'  vt.tr$tree1
#'  # return infos
#'  vt.tr$tree1$getInfos()
#'  # ...
#' }
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
        tree <- VT.tree.class(vt.difft = vt.difft, sens = sens, threshold = threshold, screening = screening)
      else
        tree <- VT.tree.reg(vt.difft = vt.difft, sens = sens, threshold = threshold, screening = screening)
      
      tree$run(...)
      
      return(tree)
    }
  }else
    stop("threshold must be numeric")
}
