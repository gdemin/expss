#' Convert table to huxtable
#' 
#' @export
#' @rdname as_huxtable
#' @param x etable. Table to convert to a huxtable.
as_huxtable <- function (x, ...) UseMethod("as_huxtable")

#' @export
#' @rdname as_huxtable
as_hux <- as_huxtable

#' @export
#' @rdname as_huxtable
as_huxtable.etable <- function(x, ...) {
  
  if (!requireNamespace("huxtable", quietly = TRUE)) {
    stop("Package \"huxtable\" needed for this function to work. Please install it first.",
         call. = FALSE)
  }
  
  # is table empty?
  if(ncol(x)==0) {
    ht <- huxtable::hux()
    #huxtable::set_caption(ht, "Table is empty")
    return(ht)
  }
  
  # start with default huxtable
  ht <- huxtable:::as_huxtable.default(x)
  
  
  
  # Split row_labels of merged cells and save matrix
  rown <- do.call("rbind", strsplit(ht[[1]], "\\|"))
  
  # Delete old row_labels
  ht[,1] <- NULL 
  
  # Atttach splitted row_labels
  if(!is.null(rown)) {
     if(ncol(ht) > 0) { 
      ht <- cbind(rown, ht)
     } else {
     ht <- huxtable:::as_huxtable.default(rown)
     }
    }

  # Split column labels of merged cells
  coln <- t(do.call("rbind", strsplit(names(ht), "\\|")))
  
  # Remove default data.frame names (V1, V2, ...)
  coln[coln %in% paste0("V", seq_len(ncol(ht)))] <- ""
  
  # Attach splitted column names
  if(!is.null(coln)) {
  if(nrow(ht)!=0) {
    ht <- rbind(coln, ht)
  } else {
    ht <- huxtable:::as_huxtable.default(coln)
  }
  }
  
  ## Merge columns
  # Iterate over rows
  for(j in seq_len(nrow(ht))) {
    
    # Add new labels and merge cells
    # Restriction on row and column labels
    if (j <= nrow(coln)) { # if in column with row labels
      colnlab <- unique(ht[j, ])
    } else { # if else, stay in rows with column labels
      colnlab <- unique(ht[j, 1:ncol(rown)])  
    }
    
    for(r in colnlab) {
      mergel <- which(ht[j, ] == r)
      
      # Dom't merge comeplety, if row above is splitted
      if (j > 1) {
        rowabove <- attr(ht, "colspan")[j-1,]
      } else {
        rowabove <- rep(0, max(mergel))
      }
      
      # If row before is merged
      if(any(rowabove[mergel] > 1)) {
        
        # Get merged cells
        limit <- rowabove[mergel]
        limits <- limit[limit > 1]
        
        # Merge cells only within merged cells above
        start <- 1
        for(l in limits) {
          # If alternating labels exist, do nothing
          if(isTRUE(all.equal(min(mergel):max(mergel), mergel))) {
            ht <- huxtable:::merge_cells(ht, j, na.omit(mergel[start:(start+l-1)]))
            start <- start + l
          }
        }
      } else {
        # If alternating labels exist, do nothing
        if(isTRUE(all.equal(min(mergel):max(mergel), mergel))) {
          ht <- huxtable:::merge_cells(ht, j, mergel)
        }
      }
    }
  }
  
  # Merge rows
  # Check which cells are already merged
  
  tmp <- attr(ht, "colspan")
  mergemat <- NULL # keep results here
  
  for(j in seq_len(nrow(tmp))){
    row <- rep(FALSE, length(tmp[j,]))
    
    start <- 1
    while(start < ncol(tmp)) {
      m <- tmp[j,][start]
      if(m > 1) {
        row[start:(start + m -1)] <- T
        start <- start + m
      } else {
        start <- start + 1
      }
      
    }
    mergemat <- rbind(mergemat, row)
  }
  
  #, merge columns
  # Iterate over columns
  for(j in seq_len(ncol(ht))) {
    
    # Add new labels and merge cells
    # Restriction on row and column labels
    if (j <= ncol(ht)) { #  if in column with column labels
      rownlab <- na.omit(unique(ht[[j]]))
    } else { # if in column without column labels
      rownlab <- na.omit(unique(ht[[j]][1:nrow(coln)]))
    }
    
    
    for(r in rownlab) {
      mergel <- which(ht[[j]] == r)
      
      if (j > 1) {
        colbefore <- attr(ht, "rowspan")[,j-1]
      } else {
        colbefore <- rep(0, max(mergel))
      }
      
      # If col before is merged
      if(any(colbefore[mergel] > 1)) {
        # Get merged cells
        limit <- colbefore[mergel]
        limits <- limit[limit > 1]
        start <- 1
        
        for(l in limits) {
          # If alternating labels exist and col is not merged  , do nothing
          if(isTRUE(all.equal(min(mergel):max(mergel), mergel)) & !any(mergemat[,j][mergel])) {
            ht <- huxtable:::merge_cells(ht, na.omit(mergel[start:(start+l-1)]), j)
            start <- start + l
          }
        }
      } else {
        # If alternating labels exist and col is not merged  , do nothing
        if(isTRUE(all.equal(min(mergel):max(mergel), mergel)) & !any(mergemat[,j][mergel])) {
          
          ht <- huxtable:::merge_cells(ht, mergel, j)
        }
      }
    }
  }
 
  # Check for caption
  if(!is.null(attr(x, "caption")))
    huxtable::set_caption(ht, attr(x, "caption"))
  
  return(ht)
  
}
