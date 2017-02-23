#' Outputting HTML tables
#' 
#' This is method for rendering results of \link{fre}/\link{cro} in Shiny.
#' For detailed description of function and its arguments see \link[DT]{datatable}.
#'
#' @param x a data object (result of \link{fre}/\link{cro} and etc)
#' @param digits integer If it is not NULL than all numeric columns will be
#'   rounded to specified number of digits.
#' @param ... further parameters for \link[htmlTable]{htmlTable}
#'
#' @return Returns a string of class htmlTable
#' @export
#'
#' @examples
#' a = 1
htmlTable.simple_table = function(x, digits = getOption("expss.digits"), ...){
    x = round_dataframe(x, digits = digits)
    first_lab = colnames(x)[1]
    if(first_lab == "row_labels") first_lab = ""
    first_lab = htmltools::htmlEscape(first_lab)
    row_labels = htmltools::htmlEscape(x[[1]])
   

    x[[1]] = NULL
    # x[[1]] = NULL # remove first column. This method is needed to prevent column names damaging
    header = t(split_labels(colnames(x), split = "|", remove_repeated = FALSE))
    header[] = htmltools::htmlEscape(header)
    crgoup_list = matrix_to_cgroup(header)
    cgroup = crgoup_list[["cgroup"]]
    n.cgroup = crgoup_list[["n.cgroup"]]
    html_header = cgroup[nrow(cgroup), ]
    align = rep("r", ncol(x))
    row_labels = split_labels(row_labels)
    if(ncol(row_labels) == 1){
        rnames = row_labels[,1] 
        rgroup = NULL
        n.rgroup = NULL
    } else {
        if(ncol(row_labels) > 2){
            x = dtfrm(row_labels[, -(1:2)], x)
            html_header = c(rep("", ncol(row_labels) - 2), html_header)
            align = c(rep("l", ncol(row_labels) - 2), align)
            cgroup = cbind("", cgroup)
            n.cgroup = cbind(ncol(row_labels) - 2, n.cgroup)
        }
        rnames = row_labels[,2]
        temp = row_labels[,1]
        for(each in seq_along(temp)[-1]) {
            if(temp[each]=="") temp[each] = temp[each-1]
        }
        temp = rle(temp)
        rgroup = temp$values
        n.rgroup = temp$lengths
    }
    cgroup = cgroup[-nrow(cgroup), ]
    n.cgroup = n.cgroup[-nrow(n.cgroup), ]

    if(nrow(cgroup)>0){
        if(is.null(rgroup)){
            htmlTable(as.dtfrm(x), 
                      header = html_header,
                      cgroup = cgroup, 
                      align = align,
                      n.cgroup = n.cgroup, 
                      rnames = rnames, 
                      rowlabel = first_lab,
                      ...)   
        } else {
            htmlTable(as.dtfrm(x), 
                      header = html_header,
                      cgroup = cgroup, 
                      align = align,
                      n.cgroup = n.cgroup, 
                      rnames = rnames,  
                      rgroup = rgroup,
                      n.rgroup = n.rgroup,
                      rowlabel = first_lab,
                      ...)     
        }
    } else {
        if(is.null(rgroup)){
            htmlTable(as.dtfrm(x), 
                      header = html_header,
                      align = align,
                      rnames = rnames, 
                      rowlabel = first_lab,
                      ...)   
        } else {
            htmlTable(as.dtfrm(x), 
                      header = html_header,
                      align = align,
                      rnames = rnames, 
                      rgroup = rgroup,
                      n.rgroup = n.rgroup,
                      rowlabel = first_lab,
                      ...)     
        }
    }
    
    
}

matrix_to_cgroup = function(header){
    
    rle_list = list()
    for(i in seq_len(nrow(header))){
        y = colSums((header[1:i,-1L, drop = FALSE] != header[1:i, -ncol(header), drop = FALSE]))>0
        changes = c(which(y | is.na(y)), ncol(header))
        rle_list[[i]] = structure(list(lengths = diff(c(0L, changes)), values = header[i, changes]))
    }
    cgroup = lapply(rle_list, "[[", "values")
    n.cgroup = lapply(rle_list, "[[", "lengths")
    max_cgroup_length = max(lengths(cgroup))
    for(each in seq_along(cgroup)){
        if(length(cgroup[[each]])<max_cgroup_length){
            cgroup[[each]][max_cgroup_length] = NA
            n.cgroup[[each]][max_cgroup_length] = NA
        }
    }   
    list(cgroup = do.call(rbind, cgroup), n.cgroup = do.call(rbind, n.cgroup))
}


#' @rdname htmlTable.simple_table
#' @export
knit_print.simple_table = function(x, digits = NULL, ...){
    knitr::asis_output(htmlTable(x, ...))
    
}

#' @rdname htmlTable.simple_table
#' @export
knit_print.etable = function(x, digits = NULL, ...){
    knitr::asis_output(htmlTable(x, ...))    
}


