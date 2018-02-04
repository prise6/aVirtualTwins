
#' aVirtualTwins : An adapation of VirtualTwins method created by Jared Foster.
#' 
#' aVirtualTwins is written mainly with reference classes. Briefly, there is three kinds of class :
#' \itemize{
#'  \item \code{\link{VT.object}} class to represent RCT dataset used by aVirtualTwins. To format correctly RCT dataset, use \code{\link{formatRCTDataset}}.
#'  \item \code{\link{VT.difft}} class to compute difference between twins. Family \code{\link{VT.forest}} extends it to compute twins by random forest.
#'   \code{\link{vt.forest}} is users function.
#'  \item \code{\link{VT.tree}} class to find subgroups from \code{difft} by CART trees. \code{\link{VT.tree.class}} and \code{\link{VT.tree.reg}} extend it. 
#'  \code{\link{vt.tree}} is users function.
#' }
#'  
#' 
#' See http://github.com/prise6/aVirtualTwins for last updates.
#'
#' @docType package
#' @name aVirtualTwins
#' @aliases aVirtualTwins-package
#'
NULL