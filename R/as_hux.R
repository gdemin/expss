#' Convert table to huxtable
#' 
#' This function converts a \code{etable} object to a \code{huxtable}.
#' The \code{\link[huxtable]{huxtable-package}} needs to be installed to use this function.
#' 
#' \code{huxtable} allows to export formated tables to LaTeX, HTML, Microsoft Word, 
#' Microsoft Excel, Microsoft Powerpoint, RTF and Markdown.
#' 
#' Tables in knitr or rmarkdown documents of type LaTeX or Word 
#' are converted by default. 
#' 
#' 
#' @export
#' @rdname as_huxtable
#' @param x etable. Table to convert to a huxtable.
#' @param ... arguments passed on to \link[huxtable]{huxtable}.
#' @examples 
#' \dontrun{ 
#' library(huxtable)
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)",
#'                       hp = "Gross horsepower",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (1000 lbs)",
#'                       qsec = "1/4 mile time",
#'                       vs = "Engine",
#'                       vs = c("V-engine" = 0,
#'                              "Straight engine" = 1),
#'                       am = "Transmission",
#'                       am = c("Automatic" = 0,
#'                              "Manual"=1),
#'                       gear = "Number of forward gears",
#'                       carb = "Number of carburetors"
#' )
#' 
#' tab = mtcars %>% 
#'     tab_cols(total(), am %nest% vs) %>% 
#'     tab_cells(mpg, hp) %>% 
#'     tab_stat_mean() %>% 
#'     tab_cells(cyl) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_pivot() %>% 
#'     set_caption("Table 1. Some variables from mtcars dataset.")
#' 
#' ht = as_huxtable(tab)
#' ht
#' }

#' @rawNamespace if(getRversion() >= "3.6.0") {
#' S3method(huxtable::as_huxtable, etable)
#' S3method(huxtable::as_hux, etable)
#' } else {
#' export(as_huxtable.etable)
#' export(as_hux.etable)
#' }
as_huxtable.etable = function(x, ...) {

  
  # is table empty?
  if(ncol(x)==0) {
    ht <- huxtable::hux()
    #huxtable::set_caption(ht, "Table is empty")
    return(ht)
  }
  
  
  # Assign correct type to data columns (counts to integer)
  xt <- as.data.frame(lapply(x, type.convert, as.is = TRUE),
                      optional = FALSE, 
                      check.names = FALSE, 
                      cut.names = FALSE,
                      col.names = names(x),
                      fix.empty.names = FALSE,
                      stringsAsFactors = FALSE
                      )
  
  # start with default huxtable
  ht <- huxtable::as_huxtable(xt, ...)

  # Split row_labels of merged cells and save matrix
  if(length(ht[[1]])>0) {
    rown = do.call("rbind", strsplit(ht[[1]], "\\|"))
  } else {
    rown = character(0)
  }
  
  # Delete old row_labels
  ht[,1] <- NULL 
  
  # Atttach splitted row_labels
  if(!is.null(rown)) {
     if(ncol(ht) > 0) { 
      ht <- cbind(rown, ht)
     } else {
     ht <- huxtable::as_huxtable(rown, ...)
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
    ht <- huxtable::as_huxtable(coln, ...)
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
            ht <- huxtable::merge_cells(ht, j, na.omit(mergel[start:(start+l-1)]))
            start <- start + l
          }
        }
      } else {
        # If alternating labels exist, do nothing
        if(isTRUE(all.equal(min(mergel):max(mergel), mergel))) {
          ht <- huxtable::merge_cells(ht, j, mergel)
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
            ht <- huxtable::merge_cells(ht, na.omit(mergel[start:(start+l-1)]), j)
            start <- start + l
          }
        }
      } else {
        # If alternating labels exist and col is not merged  , do nothing
        if(isTRUE(all.equal(min(mergel):max(mergel), mergel)) & !any(mergemat[,j][mergel])) {
          
          ht <- huxtable::merge_cells(ht, mergel, j)
        }
      }
    }
  }
  # top-left corner
  # corner_width = ncol(split_labels(x[[1]])) 
  # corner_height = ncol(split_labels(colnames(x))) 
  # ht = huxtable::merge_cells(ht, c(1, corner_height), 1)
  
  # Check for caption
  if(!is.null(attr(x, "caption")))
    ht <- huxtable::set_caption(ht, attr(x, "caption"))
  
  # set width to 100%
  huxtable::width(ht) <- 1
  
  return(ht)
  
}

#' @rdname as_huxtable
as_hux.etable = as_huxtable.etable
