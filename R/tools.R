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
  if(is.list(table)) table <-  table[[1]]
  Incidence <- function(X) as.character(round(X[2] / X[3], digits = 3))
  t <- addmargins(table, margin = c(1,2), FUN = sum, quiet = T)
  t <- addmargins(t, FUN = Incidence, margin = 1, quiet = T)
  rr <- as.numeric(t["Incidence", "1"]) / as.numeric(t["Incidence", "0"])
  return(list(table = t, rr = rr))
}

vt.getIncidence <- function(df){
  if (nrow(df) == 0) table.res <- NULL
  else{
    table.res <- vt.getTable(table(df[, 1],
                                   df[, 2],
                                   deparse.level = 2,
                                   dnn = c("resp", "trt")))
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




