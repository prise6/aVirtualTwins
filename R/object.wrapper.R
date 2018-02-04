#' 
#' Initialize virtual twins data
#' 
#' \code{vt.data} is a wrapper of \code{\link{formatRCTDataset}} and 
#' \code{\link{VT.object}}. Allows to format your data.frame in order to create
#' a VT.object object.
#' 
#' @param dataset data.frame representing RCT's
#' @param outcome.field name of the outcome's field in \code{dataset}
#' @param treatment.field name of the treatment's field in \code{dataset}
#' @param interactions logical. If running VirtualTwins with treatment's 
#'   interactions, set to TRUE (default value)
#' @param ... parameters of \code{\link{VT.object}}
#'   
#' @examples
#' 
#' data(sepsis)
#' vt.o <- vt.data(sepsis, "survival", "THERAPY", T)
#'   
#' @return \code{VT.object}
#'   
#' @include object.R 
#'   
#' @name vt.data
#'   
#' @export vt.data
#' 
#' @seealso \code{\link{formatRCTDataset}}

vt.data <- function(dataset, outcome.field, treatment.field, interactions = TRUE, ...){
  data <- formatRCTDataset(dataset, outcome.field, treatment.field, interactions = TRUE)
  VT.object(data = data, ...)
}