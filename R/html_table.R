#' Outputting HTML tables in RStudio viewer/R Notebooks
#' 
#' This is method for rendering results of \link{fre}/\link{cro}/\link{tables} in
#' Shiny/RMarkdown and etc. For detailed description of function and its
#' arguments see \link[htmlTable]{htmlTable}. You may be interested in
#' \code{expss_output_viewer()} for automatical rendering tables
#' in the RStudio viewer or  \code{expss_output_rnotebook()} for
#' rendering in the R notebooks. See \link{expss.options}.
#'
#' @param x a data object (result of \link{fre}/\link{cro} and etc)
#' @param digits integer By default, all numeric columns are rounded to one digit after
#'   decimal separator. Also you can set this argument by setting option 'expss.digits'
#'   - for example, \code{expss_digits(2)}. If it is NA than all
#'   numeric columns remain unrounded.
#' @param ... further parameters for \link[htmlTable]{htmlTable}.
#' @param row_groups logical Should we create row groups? TRUE by default.
#'
#' @return Returns a string of class htmlTable
#' @export
#'
#' @examples
#' \dontrun{ 
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
#' expss_output_viewer()
#' mtcars %>% 
#'      tab_cols(total(), am %nest% vs) %>% 
#'      tab_cells(mpg, hp) %>% 
#'      tab_stat_mean() %>% 
#'      tab_cells(cyl) %>% 
#'      tab_stat_cpct() %>% 
#'      tab_pivot()
#'      
#' expss_output_default()   
#'  
#' }
htmlTable.etable = function(x, digits = get_expss_digits(), ..., row_groups = TRUE){
    if(!row_groups){
        return(html_table_no_row_groups(x = x, digits = digits, ...))
    }
    x = round_dataframe(x, digits = digits)
    if(NCOL(x) == 0){
        return(htmlTable(setNames(dtfrm("Table is empty"), " "), ...))
    }
    # escape <NA>
    colnames(x) = gsub("<NA>", "&lt;NA&gt;", colnames(x), fixed = TRUE)
    if(is.character(x[[1]]) || is.factor(x[[1]])){
        x[[1]] = gsub("<NA>", "&lt;NA&gt;", x[[1]], fixed = TRUE)
    }
    ## for significance marks
    for(i in seq_along(x)[-1]){
        if(is.character(x[[i]]) || is.factor(x[[i]])){
            x[[i]] = gsub("\\s", "&nbsp;", x[[i]], perl = TRUE)
        }
    }
    first_lab = colnames(x)[1]
    if(first_lab == "row_labels") first_lab = ""
    # first_lab = htmltools::htmlEscape(first_lab)
    row_labels = x[[1]]  # htmltools::htmlEscape(x[[1]])
    x[[1]] = NULL # remove first column. This method is needed to prevent column names damaging
    header = t(split_labels(colnames(x), split = "|", fixed = TRUE, remove_repeated = FALSE))
    header_last_row = t(split_labels(colnames(x),
                                     split = "|", 
                                     fixed = TRUE,
                                     remove_repeated = TRUE))[NROW(header), , drop = FALSE]
    # header[] = htmltools::htmlEscape(header)
    # header_last_row[] = htmltools::htmlEscape(header_last_row)
    for(each in seq_len(NCOL(header))){
        curr_col = header[, each]
        ok = !is.na(curr_col) & curr_col!=""
        header[ok, each] = 
            paste0("&nbsp;", curr_col[ok], "&nbsp;")
    }
    for(each in seq_len(NCOL(header_last_row))){
        curr_col = header_last_row[, each]
        ok = !is.na(curr_col) & curr_col!=""
        header_last_row[ok, each] = 
            paste0("&nbsp;", curr_col[ok], "&nbsp;")
    }
    if(NCOL(header)>0){
       
        html_header = header_last_row
        if(NROW(header)>1){
            cgroup_list = matrix_to_cgroup(header[-NROW(header), ,drop = FALSE])
            cgroup = cgroup_list[["cgroup"]]
            n.cgroup = cgroup_list[["n.cgroup"]]
        } else {
            cgroup = matrix(character(0), 0, 0)
            n.cgroup = matrix(0, 1, 1)    
        }
    } else {
        html_header = character(0)
        cgroup = matrix(character(0), 0, 0)
        n.cgroup = matrix(0, 1, 1)
        
    }
    align = rep("r", NCOL(x))
    row_labels = split_labels(row_labels, split = "|", fixed = TRUE)
    for(each in seq_len(NCOL(row_labels))){
        curr_col = row_labels[, each]
        ok = !is.na(curr_col) & curr_col!=""
        row_labels[ok, each] = 
            paste0("&nbsp;", curr_col[ok], "&nbsp;")
    }
    if(NCOL(row_labels)==0) row_labels = matrix("", 1, 1)
    if(NCOL(row_labels) == 1){
        rnames = row_labels[,1] 
        rgroup = NULL
        n.rgroup = NULL
    } else {
        if(NCOL(row_labels) > 2){
            x = dtfrm(row_labels[, -(1:2)], x)
            html_header = c(rep("", NCOL(row_labels) - 2), html_header)
            align = c(rep("l", NCOL(row_labels) - 2), align)
            if(NCOL(header)>0){
                cgroup = cbind("", cgroup)
                n.cgroup = cbind(NCOL(row_labels) - 2, n.cgroup)
            } else {
                cgroup = matrix("", 1, 1) 
                n.cgroup = matrix(NCOL(row_labels) - 2, 1, 1)
            }
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
    # cgroup = cgroup[-NROW(cgroup), ,drop = FALSE]
    # n.cgroup = n.cgroup[-NROW(n.cgroup), , drop = FALSE]
    if(NCOL(x)>0){
    if(NROW(cgroup)>0){
        cgroup = cgroup[,colSums(!is.na(cgroup))>0, drop = FALSE]
        n.cgroup = n.cgroup[,colSums(!is.na(n.cgroup))>0, drop = FALSE]
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
    } else {
        x = rep("", NROW(x))
        htmlTable(dtfrm(x), 
                  header = "",
                  rnames = rnames, 
                  rgroup = rgroup,
                  n.rgroup = n.rgroup,
                  rowlabel = first_lab,
                  ...) 
        
    }
 
}

matrix_to_cgroup = function(header){
    
    rle_list = list()
    if(NCOL(header)>1){
        for(i in seq_len(nrow(header))){
            y = colSums((header[1:i,-1L, drop = FALSE] != header[1:i, -ncol(header), drop = FALSE]))>0
            changes = c(which(y | is.na(y)), ncol(header))
            rle_list[[i]] = structure(list(lengths = diff(c(0L, changes)), 
                                           values = header[i, changes]))
        }
    } else {
        for(i in seq_len(nrow(header))){
            rle_list[[i]] = structure(list(lengths = 1, 
                                           values = header[i, ])
                                      )
        }
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

#' @export
#' @rdname htmlTable.etable
knit_print.etable = function(x, digits = get_expss_digits(), ...){
    res = htmlTable(x, digits = digits, ...)
    # res = fix_cyrillic_for_rstudio(res)
    knitr::asis_output(res)
}





## for Jupyter notebooks where row headings not rendered correctly
html_table_no_row_groups = function(x, digits = get_expss_digits(), ...){
    x = round_dataframe(x, digits = digits)
    if(NCOL(x) == 0){
        return(htmlTable(setNames(dtfrm("Table is empty"), " "), ...))
    }
    # escape <NA>
    colnames(x) = gsub("<NA>", "&lt;NA&gt;", colnames(x), fixed = TRUE)
    if(is.character(x[[1]]) || is.factor(x[[1]])){
        x[[1]] = gsub("<NA>", "&lt;NA&gt;", x[[1]], fixed = TRUE)
    }
    ## for significance marks
    for(i in seq_along(x)[-1]){
        if(is.character(x[[i]]) || is.factor(x[[i]])){
            x[[i]] = gsub("\\s", "&nbsp;", x[[i]], perl = TRUE)
        }
    }
    first_lab = colnames(x)[1]
    if(first_lab == "row_labels") first_lab = ""
    # first_lab = htmltools::htmlEscape(first_lab)
    row_labels = x[[1]]  # htmltools::htmlEscape(x[[1]])
    x[[1]] = NULL # remove first column. This method is needed to prevent column names damaging
    header = t(split_labels(colnames(x), split = "|", fixed = TRUE, remove_repeated = FALSE))
    header_last_row = t(split_labels(colnames(x),
                                     split = "|", 
                                     fixed = TRUE,
                                     remove_repeated = TRUE))[NROW(header), , drop = FALSE]
    # header[] = htmltools::htmlEscape(header)
    # header_last_row[] = htmltools::htmlEscape(header_last_row)
    for(each in seq_len(NCOL(header))){
        curr_col = header[, each]
        ok = !is.na(curr_col) & curr_col!=""
        header[ok, each] = 
            paste0("&nbsp;", curr_col[ok], "&nbsp;")
    }
    for(each in seq_len(NCOL(header_last_row))){
        curr_col = header_last_row[, each]
        ok = !is.na(curr_col) & curr_col!=""
        header_last_row[ok, each] = 
            paste0("&nbsp;", curr_col[ok], "&nbsp;")
    }
    if(NCOL(header)>0){
        
        html_header = header_last_row
        if(NROW(header)>1){
            cgroup_list = matrix_to_cgroup(header[-NROW(header), ,drop = FALSE])
            cgroup = cgroup_list[["cgroup"]]
            n.cgroup = cgroup_list[["n.cgroup"]]
        } else {
            cgroup = matrix(character(0), 0, 0)
            n.cgroup = matrix(0, 1, 1)    
        }
    } else {
        html_header = character(0)
        cgroup = matrix(character(0), 0, 0)
        n.cgroup = matrix(0, 1, 1)
        
    }
    align = rep("r", NCOL(x))
    row_labels = split_labels(row_labels, split = "|", fixed = TRUE)
    for(each in seq_len(NCOL(row_labels))){
        curr_col = row_labels[, each]
        ok = !is.na(curr_col) & curr_col!=""
        row_labels[ok, each] = 
            paste0("&nbsp;", curr_col[ok], "&nbsp;")
    }
    if(NCOL(row_labels)==0) row_labels = matrix("", 1, 1)
    if(NCOL(row_labels) == 1){
        rnames = row_labels[,1] 
    } else {
        if(NCOL(row_labels) > 1){
            x = dtfrm(row_labels[, -1], x)
            html_header = c(rep("", NCOL(row_labels) - 1), html_header)
            align = c(rep("l", NCOL(row_labels) - 1), align)
            if(NCOL(header)>0){
                cgroup = cbind("", cgroup)
                n.cgroup = cbind(NCOL(row_labels) - 1, n.cgroup)
            } else {
                cgroup = matrix("", 1, 1) 
                n.cgroup = matrix(NCOL(row_labels) - 1, 1, 1)
            }
        }
        rnames = row_labels[,1]
    }
    # cgroup = cgroup[-NROW(cgroup), ,drop = FALSE]
    # n.cgroup = n.cgroup[-NROW(n.cgroup), , drop = FALSE]
    if(NCOL(x)>0){
        if(NROW(cgroup)>0){
            cgroup = cgroup[,colSums(!is.na(cgroup))>0, drop = FALSE]
            n.cgroup = n.cgroup[,colSums(!is.na(n.cgroup))>0, drop = FALSE]
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
                          align = align,
                          rnames = rnames, 
                          rowlabel = first_lab,
                          ...)   
            
        }
    } else {
        x = rep("", NROW(x))
        htmlTable(dtfrm(x), 
                  header = "",
                  rnames = rnames, 
                  rowlabel = first_lab,
                  ...) 
        
    }
    
}
