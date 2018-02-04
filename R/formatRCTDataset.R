#' 
#' RCT format for Virtual Twins
#' 
#' \code{formatRCTDataset} returns dataset that Virtual Twins is able to 
#' analyze.
#' 
#' This function check these differents topic: Outcome must be binary and a 
#' factor. If numeric with two distincts values, outcome becomes a factor where 
#' the favorable reponse is the second level. Also, outcome is moved on the 
#' first column of \code{dataset}.
#' 
#' Treatment must have two distinct numeric values, 0 : no treatment, 1 : 
#' treatment. Treatment is moved to the second column.
#' 
#' Qualitatives variables must be factor. If it has more than two levels, if 
#' running VirtualTwins with interaction, it creates dummy variables.
#' 
#' @param dataset data.frame representing RCT's
#' @param outcome.field name of the outcome's field in \code{dataset}
#' @param treatment.field name of the treatment's field in \code{dataset}
#' @param interactions logical. If running VirtualTwins with treatment's
#'   interactions, set to TRUE (default value)
#' 
#' @return return data.frame with good format (explained in details section) to run VirtualTwins
#' 
#' @examples
#' \dontrun{
#'     data.format <- formatRCTDataset(data, "outcome", "treatment", TRUE)
#' }
#' data(sepsis)
#' data.format <- formatRCTDataset(sepsis, "survival", "THERAPY", T)
#'  
#'
#'  
#'   
#' @export
formatRCTDataset <- function(dataset, outcome.field, treatment.field, interactions = TRUE){
  
  if(!is.data.frame(dataset)) stop("Dataset parameter must be data.frame")
  if(!is.character(outcome.field)) stop(sprintf("%s, outcome.field parameter must be a string", outcome.field))
  if(!is.character(treatment.field)) stop(sprintf("%s, treatment.field parameter must be a string", treatment.field))
  
  if(!outcome.field %in% colnames(dataset)) stop(sprintf("%s must be in data.frame colnames", outcome.field))
  outcome.field.which <- which(outcome.field == colnames(dataset))
  if(!treatment.field %in% colnames(dataset)) stop(sprintf("%s must be in data.frame colnames", treatment.field))
  treatment.field.which <- which(treatment.field == colnames(dataset))
  
  d <- dataset
  
  outcome <- d[, outcome.field.which]
  if(!is.factor(outcome)) outcome <- as.factor(outcome)
  if(!length(levels(outcome)) == 2L) stop(sprintf("outcome %s must be binary", outcome.field))
  cat(sprintf("\"%s\" will be the favorable outcome \n", levels(outcome)[2]))
  d[, outcome.field.which] <- outcome
  
  treatment <- d[, treatment.field.which]
  if(!is.numeric(treatment) & !is.integer(treatment)) treatment <- as.numeric(treatment)
  if(!( length(unique(treatment)) == 2L & all(c(0,1) %in% unique(treatment)) ))
    stop(sprintf("%s, response must be numeric:\n 0 = no treatment \n 1 = treatment \n", treatment.field))
  d[, treatment.field.which] <- treatment
  
  predictors <- colnames(dataset)[-c(outcome.field.which, treatment.field.which)]
  
  predictors.next <- vector()
  iter = 1
  for(i in predictors){
    iter <- length(predictors.next)+1
    
    var <- d[, i]
    
    if(is.numeric(var) | is.integer(var)){
      predictors.next[iter] <- i
    }
    
    if(is.character(var)){
      var <- as.factor(var)
    }
    
    if(is.factor(var)){
      if(length(levels(var))>2){
        if(isTRUE(interactions)){
          cat(sprintf("Creation of dummy variables for %s \n", i))
          for(l in levels(var)){
            n <- paste(i, l, sep = "_")
            d[, n] <- ifelse(var == l, 1, 0)
            predictors.next[iter] <- n
            cat(sprintf("Dummy variable %s created \n", n))
            iter <- iter + 1
          }
          #           d <- d[, predictors != i]
        } else {
          if(length(levels(var))>32){
            stop(cat(sprintf("%s has too many levels (superior to 32)", i)))
          }else{
            warning(sprintf("%s has more than 2 levels. Virtual Twins won't be able to run with interactions.", i))
            predictors.next[iter] <- i
          }
        }
      }else if(length(levels(var)) == 2){
        cat(sprintf("%s is two-level factor. It has to be transformed into numeric value : \n", i))
        cat(sprintf("%s becomes 0 \n", levels(var)[1]))
        cat(sprintf("%s becomes 1 \n", levels(var)[2]))
        d[, i] <- ifelse(var == levels(var)[1], 0, 1)
        predictors.next[iter] <- i
      }else{
        cat(sprintf("%s is deleted because only one level", i))
      }
    }
  }
  
  colnames.order <- c(outcome.field, treatment.field, predictors.next)
  d <- d[, colnames.order]
  
  return(invisible(d))
}
