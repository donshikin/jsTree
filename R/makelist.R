#' Create list data structure for jsTree input from a data frame
#' @param obj Data frame (first column is supposed to have parent category names; the second - child category names; the rest are ignored)
#' @param tooltips Named character vector of tooltips
#' @param nodestates Named logical vector of node states (selected / unselected)
#' @return List encoding all parent-child relations from input data frame
#' @details This function makes use of alternative JSON format accepted by jsTree (see \url{https://www.jstree.com/docs/json/})
makeAdjList <- function(l, tooltips = NULL, nodestates = NULL){
  if(!is.data.frame(l)) stop("Data frame expected")
  if(ncol(l) < 2) stop("Two columns expected")
  if(is.null(tooltips)) tooltips <- character(0)
  if(is.null(nodestates)) nodestates <- logical(0)
  
  l <- l[,1:2]
  roots <- setdiff(l[,1], l[,2])
  leaves <- setdiff(l[,2],l[,1])
  l$is_leaf <- l[,2] %in% leaves
  
  rootjson <- lapply(roots, function(x){
    out = list(id = x, 
               parent = "#",
               text = x,
               ##will produce TRUE if and only if there is there is such a name in nodestates 
               ##and the corresponding element is TRUE
               state = list(selected = isTRUE(nodestates[x])) 
               )
    return(out)
  })
 
  restjson <- mapply(function(parent, child, is_leaf){
    out = list(id = child, 
               parent = parent,
               text = child,
               state = list(selected = isTRUE(nodestates[child]))
    )
    if(is_leaf){
      ##This node is a leaf, use file icon for it
      out$icon <- "glyphicon glyphicon-file text-info"
    }
    if(!is.na(tooltips[child])){
      out$a_href <- list(title = tooltips[child])
    }
    return(out)
    
  }, l[,1], l[,2], l$is_leaf, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  
  return(c(rootjson, restjson))
}



makeList <- function(x, tooltips, nodestates){
  
  idx <- is.na(x[,2])
  
  if( ncol(x) > 2 && sum(idx) != nrow(x) ){
    
    list_split <- split(x[-1], x[1], drop=TRUE)
    
    lapply(names(list_split), function(y){
      
      l2 <- list_split[[y]]
      
      if( all(is.na(l2[!names(l2)%in%c('nodestate')])) ){
        
        lout <- list(text = y,icon='glyphicon glyphicon-file text-info',state=list(selected=l2$nodestate))
        
        if( any(y%in%names(tooltips)) ) 
          lout$a_attr <- list(title=tooltips[grep(y,names(tooltips),value=TRUE)])
        
        lout
        
      }else{
        
        lout <- list(text = y,children = makeList(l2,tooltips))
        
        if( any(y%in%names(tooltips)) ) 
          lout$a_attr <- list(title=tooltips[grep(y,names(tooltips),value=TRUE)])
        
        lout
        
      }
    })
  }else{
    
    if( !all(is.na(x[,1])) ){
      
      nms <- x[,1]
      
      lapply(seq_along(nms), function(y){
        
      lout <- list(text = nms[y],icon='glyphicon glyphicon-file text-info',state=list(selected=x$nodestate[y]))
      
      nm <- nms[y]
      
      if( any(nm%in%names(tooltips)) ) 
        lout$a_attr <- list(title=tooltips[grep(nm,names(tooltips),value=TRUE)])
      
      lout
    })
    }
  }
}

#' @importFrom data.table rbindlist
nest <- function(l, root='root', nodestate=NULL, tooltips=NA){
  
  df <- data.frame(V0=root,
                   data.table::rbindlist(lapply(strsplit(l,'/'),
                                     function(x) as.data.frame(t(x),stringsAsFactors = FALSE)),
                                    fill=TRUE),
             stringsAsFactors = FALSE)
  
  df$value <- NA
  
  if( is.null(nodestate) ){
    
    df$nodestate <- FALSE  
    
  }else{
    
    df$nodestate <- nodestate
    
  }
  
  if(all(df[,1]==df[,2])) df[,2] <- NULL
  
  makeList(df,tooltips)
} 