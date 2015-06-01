# INCIDENCE TABLES FUNCTION /!\ important


setGeneric("VT.incidences",
           function(vt.difft, select, rr.snd){ standardGeneric("VT.incidences") }
)

setMethod(
  f = "VT.incidences",
  signature = c(vt.difft = "VT.difft", select = "character", rr.snd = "logical"),
  function(vt.difft, select, rr.snd = F){
    vector.selected <- with(vt.difft$vt.object$data, ifelse(eval(parse(text = select)), 1, 0))
    
    return(VT.incidences(vt.difft, select = vector.selected, rr.snd))
  }
)

setMethod(
  f = "VT.incidences",
  signature = c(vt.difft = "VT.difft", select = "vector", rr.snd = "logical"),
  function(vt.difft, select, rr.snd = F){
    
    selected <- with(vt.difft$vt.object$data, vt.difft$vt.object$data[as.logical(select), c(1,2)])
    not.selected <- with(vt.difft$vt.object$data, vt.difft$vt.object$data[!as.logical(select), c(1,2)])
    
    list.tmp <- list(vt.getIncidence(selected), vt.getIncidence(not.selected))
    names(list.tmp) <- c("table.selected", "table.not.selected")
    
    if(isTRUE(rr.snd)){
      list.tmp$table.selected$rr.snd <- vt.rr.snd(vt.difft, select)
      list.tmp$table.not.selected$rr.snd <- vt.rr.snd(vt.difft, (1-select))  
    }
    
    return(list.tmp)
  }
)
