#' Title
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
htmlTable.simple_table = function(x, digits = NULL, ...){
    if(!is.null(digits)){
        for (i in seq_len(NCOL(x))){
            if(is.numeric(x[[i]])){
                x[[i]] = round(x[[i]], digits)
            }
        }
    }
    first_lab = colnames(x)[1]
    row_labels = x[[1]]
    # x[[1]] = NULL # remove first column. This method is needed to prevent column names damaging
    header = t(split_labels(colnames(x), split = "|", remove_repeated = FALSE))
    # row_labels = split_labels(row_labels, split = "|", remove_repeated = !repeat_row_labels)
    # if(length(row_labels)){
    #     row_labels = dtfrm(row_labels)    
    # } else {
    #     row_labels = dtfrm(matrix("", nrow = nrow(x), ncol = 1))
    # }
    # 
    # colnames(row_labels) = rep("", ncol(row_labels))
    # empty_corner = matrix("", nrow = nrow(header), ncol = ncol(row_labels))
    # if(is.na(first_lab) || first_lab=="row_labels") first_lab = ""
    # empty_corner[1, 1] = first_lab    
    crgoup_list = matrix_to_cgroup(header)
    cgroup = crgoup_list[["cgroup"]]
    n.cgroup = crgoup_list[["n.cgroup"]]
    colnames(x) = cgroup[nrow(cgroup), ]
    cgroup = cgroup[-nrow(cgroup), ]
    n.cgroup = n.cgroup[-nrow(n.cgroup), ]
    if(nrow(cgroup)>0){
        htmlTable(as.dtfrm(x), cgroup = cgroup, n.cgroup = n.cgroup, ...)   
    } else {
        htmlTable(as.dtfrm(x), ..)
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