#' Outputting HTML tables in RStudio viewer/R Notebooks
#' 
#' This is method for rendering results of \link{fre}/\link{cro}/\link{tables}
#' in Shiny/RMarkdown/Jupyter notebooks and etc. For detailed description of
#' function and its arguments see \link[htmlTable]{htmlTable}. You can pack your
#' tables in the list and render them all simultaneously. See examples. You may
#' be interested in \code{expss_output_viewer()} for automatical rendering
#' tables in the RStudio viewer or  \code{expss_output_rnotebook()} for
#' rendering in the R notebooks. See \link{expss.options}. \code{repr_html} is
#' method for rendering table in the Jupyter notebooks and \code{knit_print} is
#' method for rendering table in the \code{knitr} HTML-documents. Jupyter
#' notebooks and \code{knitr} documents are supported automatically but in the R
#' notebooks it is needed to set output to notebook via
#' \code{expss_output_rnotebook()}.
#'
#' @param x a data object of class 'etable' - result of \link{fre}/\link{cro} and etc.
#' @param obj a data object of class 'etable' - result of \link{fre}/\link{cro} and etc.
#' @param digits integer By default, all numeric columns are rounded to one digit after
#'   decimal separator. Also you can set this argument by setting option 'expss.digits'
#'   - for example, \code{expss_digits(2)}. If it is NA than all
#'   numeric columns remain unrounded.
#' @param escape.html logical: should HTML characters be escaped? Defaults to FALSE. 
#' @param ... further parameters for \link[htmlTable]{htmlTable}.
#' @param row_groups logical Should we create row groups? TRUE by default.
#' @param gap character Separator between tables if we output list of
#'   tables. By default it is line break '<br>'.
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
#'      tab_pivot() %>% 
#'      set_caption("Table 1. Some variables from mtcars dataset.")
#'      
#' # several tables in a list
#' mtcars %>% 
#'     calc(list(
#'         cro_cpct(list(am, vs, cyl), list(total(), am)) %>% set_caption("Table 1. Percent."),   
#'         cro_mean_sd_n(list(mpg, hp, qsec), list(total(), am)) %>% set_caption("Table 2. Means.")
#'     )) %>% 
#'     htmlTable()
#'      
#' expss_output_default()   
#'  
#' }
htmlTable.etable = function(x, digits = get_expss_digits(), escape.html = FALSE, ..., row_groups = TRUE){
    if(NCOL(x) == 0){
        return(htmlTable(setNames(sheet("Table is empty"), " "), escape.html = escape.html, ...))
    }
    # because rowlabels and column names never escaped
    dollar = "&#36;"
    na_str = "&lt;NA&gt;"
    nb_space = "&nbsp;"
    # escape <NA>
    colnames(x) = gsub("<NA>", na_str, colnames(x), fixed = TRUE)
    colnames(x) = gsub("$", dollar, colnames(x), fixed = TRUE)
    if(is.character(x[[1]]) || is.factor(x[[1]])){
        x[[1]] = gsub("<NA>", na_str, x[[1]], fixed = TRUE)
        x[[1]] = gsub("$", dollar, x[[1]], fixed = TRUE)
    }
    if(escape.html){
        na_str = "<NA>"
        dollar = "$"
        nb_space =  ""     
    }
    
    digits = if_null(digits, 1)
    if(!is.na(digits)){
        x = round_dataframe(x, digits = digits)
        not_total = !get_total_rows_indicator(x, total_marker = "#")
        
        # no first column
        for(i in seq_len(NCOL(x))[-1]){
            curr_col = x[[i]][not_total]
            if(is.numeric(curr_col) && any(grepl("\\.|,", curr_col, perl = TRUE))){
                    x[[i]][not_total] = trimws(format(curr_col, nsmall = digits, justify =  "right"))
                    x[[i]][not_total][is.na(curr_col)] = ""
            }
            ## for significance marks
            if(is.character(curr_col) || is.factor(curr_col)){
                x[[i]] = gsub("^[\\s\\t]+$", " ", x[[i]], perl = TRUE )
                x[[i]] = gsub("$", dollar, x[[i]], fixed = TRUE )
                x[[i]] = gsub("<NA>", na_str, x[[i]], fixed = TRUE)
                has_symbols = grepl("[^\\s^\\t]", x[[i]], perl = TRUE)
                x[[i]][has_symbols] = gsub("\\s$", nb_space,
                                           x[[i]][has_symbols], perl = TRUE)
                x[[i]] = gsub("([\\d])$", paste0("\\1",nb_space), x[[i]], perl = TRUE)
            }
        }
    }
   

    if(!row_groups){
        return(html_table_no_row_groups(x = x, escape.html = escape.html, ...))
    }
    first_lab = colnames(x)[1]
    if(first_lab == "row_labels") first_lab = ""
    row_labels = x[[1]] 
    x[[1]] = NULL # remove first column. This method is needed to prevent column names damaging
    header = t(split_labels(colnames(x), split = "|", fixed = TRUE, remove_repeated = FALSE))
    header_last_row = t(split_labels(colnames(x),
                                     split = "|", 
                                     fixed = TRUE,
                                     remove_repeated = TRUE))[NROW(header), , drop = FALSE]
   
    for(each in seq_len(NCOL(header))){
        curr_col = header[, each]
        ok = !is.na(curr_col) & curr_col!=""
        header[ok, each] =
            paste0(nb_space, curr_col[ok], nb_space)
    }
    for(each in seq_len(NCOL(header_last_row))){
        curr_col = header_last_row[, each]
        ok = !is.na(curr_col) & curr_col!=""
        header_last_row[ok, each] =
            paste0(nb_space, curr_col[ok], nb_space)
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
            paste0(nb_space, curr_col[ok], nb_space)
    }
    if(NCOL(row_labels)==0) row_labels = matrix("", 1, 1)
    if(NCOL(row_labels) == 1){
        rnames = row_labels[,1] 
        rgroup = NULL
        n.rgroup = NULL
    } else {
        if(NCOL(row_labels) > 2){
            x = sheet(row_labels[, -(1:2)], x)
            html_header = c(rep("", NCOL(row_labels) - 2), html_header)
            align = c(rep("l", NCOL(row_labels) - 2), align)
            if(NROW(cgroup)>0){
                if(NCOL(header)>0){
                    cgroup = cbind("", cgroup)
                    n.cgroup = cbind(NCOL(row_labels) - 2, n.cgroup)
                } else {
                    cgroup = matrix("", 1, 1) 
                    n.cgroup = matrix(NCOL(row_labels) - 2, 1, 1)
                }
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
    html_table_args = list()
    if(NCOL(x)>0){
        html_table_args$x = as.sheet(x)
        html_table_args$header = html_header
        html_table_args$align = align
        if(NROW(cgroup)>0){
            html_table_args$cgroup = cgroup[,colSums(!is.na(cgroup))>0, drop = FALSE]
            html_table_args$n.cgroup = n.cgroup[,colSums(!is.na(n.cgroup))>0, drop = FALSE]
        }
    } else {
        x = rep("", NROW(x))
        html_table_args$x = as.sheet(x)
        html_table_args$header = ""
    }
    html_table_args$rnames = rnames  
    html_table_args$rgroup = rgroup
    html_table_args$n.rgroup = n.rgroup
    html_table_args$rowlabel = first_lab
    html_table_args$escape.html = escape.html
    supplied_args = list(...)
    html_table_args = html_table_args %n_d% names(supplied_args)
    html_table_args = c(html_table_args, supplied_args)
    do.call(htmlTable, html_table_args)
}

#' @export
#' @rdname htmlTable.etable
htmlTable.with_caption = function(x, digits = get_expss_digits(), escape.html = FALSE, ..., row_groups = TRUE){
    caption = get_caption(x)
    x = set_caption(x, NULL)
    htmlTable(x, caption = caption, digits = digits, escape.html = escape.html, ..., row_groups = row_groups)
}


#' @export
#' @rdname htmlTable.etable
htmlTable.list = function(x, gap = "<br>", ...){
    res = lapply(x, htmlTable, ...)
    res = do.call(paste, c(res, list(sep = gap)))
    class(res) = c("htmlTable", "character")
    res
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

#' @rawNamespace if(getRversion() >= "3.6.0") {
#' S3method(knitr::knit_print, etable)
#' S3method(knitr::knit_print, with_caption)
#' S3method(repr::repr_html, etable)
#' S3method(repr::repr_html, with_caption)
#' S3method(repr::repr_text, etable)
#' S3method(repr::repr_text, with_caption)
#' } else {
#' export(knit_print.etable)
#' export(knit_print.with_caption)
#' export(repr_html.etable)
#' export(repr_html.with_caption)
#' export(repr_text.etable)
#' export(repr_text.with_caption)
#' }
#' @rdname htmlTable.etable
knit_print.etable = function(x, digits = get_expss_digits(), escape.html = FALSE, ...){
    knitr::knit_print(htmlTable.etable(x, digits = digits, 
                                       escape.html = escape.html,
                                       ..., row_groups = TRUE))
}


#' @rdname htmlTable.etable
knit_print.with_caption = function(x, digits = get_expss_digits(), escape.html = FALSE, ...){
    knitr::knit_print(htmlTable.with_caption(x, digits = digits, 
                                             escape.html = escape.html,
                                             ..., row_groups = TRUE)
    )
}



#' @rdname htmlTable.etable
repr_html.etable = function(obj, digits = get_expss_digits(), escape.html = FALSE, ...){
    htmlTable(obj, digits = digits, escape.html = escape.html, ..., row_groups = FALSE)
    
}


#' @rdname htmlTable.etable
repr_html.with_caption = function(obj, digits = get_expss_digits(), escape.html = FALSE, ...){
    htmlTable(obj, digits = digits, escape.html = escape.html, ..., row_groups = FALSE)
}


#' @rdname htmlTable.etable
repr_text.etable = function(obj, digits = get_expss_digits(), ...){
    curr_output = getOption("expss.output")
    if(!("raw" %in% curr_output)){
        obj = split_all_in_etable_for_print(obj,
                                          digits = digits, 
                                          remove_repeated = TRUE)
    }
    if("commented" %in% curr_output){
        if(NROW(obj)>0 && NCOL(obj)>0){
            obj = cbind("#" = "#", obj)
            colnames(obj) = rep("", length(obj))
        } 
        
    }
    width = getOption("width")
    on.exit(options(width = width))
    options(width = 1000)
    paste(capture.output(print.data.frame(obj, ...,  right = TRUE, row.names = FALSE)),
          collapse = "\n")
    
}

#' @rdname htmlTable.etable
repr_text.with_caption = function(obj, digits = get_expss_digits(), ...){
    width = getOption("width")
    on.exit(options(width = width))
    options(width = 1000)
    paste(capture.output(print(obj, digits = digits, ...)),
          collapse = "\n")
    
}

## for Jupyter notebooks where row headings are not rendered correctly
html_table_no_row_groups = function(x, escape.html = FALSE, ...){
    if(!escape.html){
        nb_space = "&nbsp;"
    } else {
        nb_space =  ""     
    }
    first_lab = colnames(x)[1]
    if(first_lab == "row_labels") first_lab = ""
    row_labels = x[[1]]  
    x[[1]] = NULL # remove first column. This method is needed to prevent column names damaging
    header = t(split_labels(colnames(x), split = "|", fixed = TRUE, remove_repeated = FALSE))
    header_last_row = t(split_labels(colnames(x),
                                     split = "|", 
                                     fixed = TRUE,
                                     remove_repeated = TRUE))[NROW(header), , drop = FALSE]
    for(each in seq_len(NCOL(header))){
        curr_col = header[, each]
        ok = !is.na(curr_col) & curr_col!=""
        header[ok, each] = 
            paste0(nb_space, curr_col[ok], nb_space)
    }
    for(each in seq_len(NCOL(header_last_row))){
        curr_col = header_last_row[, each]
        ok = !is.na(curr_col) & curr_col!=""
        header_last_row[ok, each] = 
            paste0(nb_space, curr_col[ok], nb_space)
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
            paste0(nb_space, curr_col[ok], nb_space)
    }
    if(NCOL(row_labels)==0) row_labels = matrix("", 1, 1)
    if(NCOL(row_labels) == 1){
        rnames = row_labels[,1] 
    } else {
        if(NCOL(row_labels) > 1){
            x = sheet(row_labels[, -1], x)
            html_header = c(rep("", NCOL(row_labels) - 1), html_header)
            align = c(rep("l", NCOL(row_labels) - 1), align)
            if(NROW(cgroup)>0){
                if(NCOL(header)>0){
                    cgroup = cbind("", cgroup)
                    n.cgroup = cbind(NCOL(row_labels) - 1, n.cgroup)
                } else {
                    cgroup = matrix("", 1, 1) 
                    n.cgroup = matrix(NCOL(row_labels) - 1, 1, 1)
                }
            }
        }
        rnames = row_labels[,1]
    }
    # cgroup = cgroup[-NROW(cgroup), ,drop = FALSE]
    # n.cgroup = n.cgroup[-NROW(n.cgroup), , drop = FALSE]
    html_table_args = list()
    if(NCOL(x)>0){
        html_table_args$x = as.sheet(x)
        html_table_args$header = html_header
        html_table_args$align = align
        if(NROW(cgroup)>0){
            html_table_args$cgroup = cgroup[,colSums(!is.na(cgroup))>0, drop = FALSE]
            html_table_args$n.cgroup = n.cgroup[,colSums(!is.na(n.cgroup))>0, drop = FALSE]
        }
    } else {
        x = rep("", NROW(x))
        html_table_args$x = as.sheet(x)
        html_table_args$header = ""
    }
    html_table_args$rnames = rnames  
    html_table_args$rowlabel = first_lab
    html_table_args$escape.html = escape.html
    supplied_args = list(...)
    html_table_args = html_table_args %n_d% names(supplied_args)
    html_table_args = c(html_table_args, supplied_args)
    do.call(htmlTable, html_table_args)
    
}





