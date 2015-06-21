#' 
#' Initialize virtual twins data
#' 
#' \code{vt.data} is a wrapper of \code{\link{formatRCTDataset}} and
#' \code{\link{VT.object}}.
#' 
#' @param dataset data.frame representing RCT's
#' @param outcome.field name of the outcome's field in \code{dataset}
#' @param treatment.field name of the treatment's field in \code{dataset}
#' @param interactions logical. If running VirtualTwins with treatment's
#'   interactions, set to TRUE (default value)
#' @param ... parameters of \code{\link{VT.object}}
#' 
#' @return \code{VT.object}
#' 
#' @export

vt.data <- function(dataset, outcome.field, treatment.field, interactions = TRUE, ...){
  data <- formatRCTDataset(dataset, outcome.field, treatment.field, interactions = TRUE)
  VT.object(data = data, ...)
}