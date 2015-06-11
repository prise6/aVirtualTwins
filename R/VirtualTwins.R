
#' VirtualTwins : An adapation of VirtualTwins method created by Jared Foster.
#' 
#' VirtualTwins is written mainly with reference classes. Briefly, there is three kinds of class :
#' \itemize{
#'  \item \code{\link{VT.object}} class to represent RCT dataset used by VirtualTwins. To format correctly RCT dataset, use \code{\link{formatRCTDataset}}.
#'  \item \code{\link{VT.difft}} class to compute difference between twins. Family \code{\link{VT.forest}} extends it to compute twins by random forest.
#'  \item \code{\link{VT.tree}} class to find subgroups from \code{\link{difft}} by CART trees. \code{\link{VT.tree.class}} and \code{\link{VT.tree.reg}} extend it.
#' }
#'  
#' @section TODO LIST:
#' \emph{last update : 11.06.2015}
#' \itemize{
#'  \item More detailed documentation and vignettes 
#'  \item Write wrappers for classes
#'  \item Write examples
#'  \item ...
#' }
#'
#' @docType package
#' @name VirtualTwins
#' @aliases VirtualTwins-package
#'
NULL