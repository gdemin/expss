
#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return x
#' @examples
#' list()
#' @export
if_val = function(x, ..., from = NULL, to = NULL){
    UseMethod("if_val")
    
}

#' @export
"if_val<-" = function(x, value, from = NULL){
    if (is.null(from)){
        if_val(x, value)
    } else {
        if_val(x, from = from, to = value)
    }
}


#' @export
if_val.default = function(x, ..., from = NULL, to = NULL){
    if (is.null(from) && is.null(to)){
        recoding_list = lapply(unlist(list(...)), parse_formula)
    } else {
        stopif(is.null(from) || is.null(to), "Both 'from' and 'to' arguments should be not NULL.")
        stopif(length(from)!=length(to), 
               "length(to) should be equal to length(from) but length(from)=", length(from),
               " and length(to)=", length(to))

        recoding_list = mapply(function(x,y) list(from = x, to = y), from, to, SIMPLIFY = FALSE)
    }
    recoded = rep(FALSE, length(x))
    dfs_x = as.data.frame(x)
    
    for (from_to in recoding_list){
        if (all(recoded)) break # if all values were recoded
        from = from_to$from
        if (all_other(from)){
            # dot is considered as all other non-recoded values ("else" from SPSS)
            cond = !recoded
        } else {
            if (identical(from, NA)) from = as.numeric(NA)
            cond = build_criterion(from, dfs_x)
            if_na(cond) = FALSE
            cond = cond & !recoded # we don't recode already recoded value
        }
        to = from_to$to
        check_conformance(cond, to)
        # dot in `to` means copy (simply doesn't change values that meet condition - "copy" from SPSS ) 
        if(!is.list(to) || is.data.frame(to)){
            for (each_col in seq_len(NCOL(x))){
                curr_cond = column(cond, each_col)
                if (any(curr_cond)) column(x, each_col, curr_cond) = column(to, each_col, curr_cond)
            }
        } else {
            for (each_col in seq_len(NCOL(x))){
                curr_cond = column(cond, each_col)
                if (any(curr_cond))  if_val(column(x, each_col), from = list(curr_cond)) = list(column(to, each_col))
            }            
        }
        if(is.atomic(to) && length(to)==1 && !is.null(names(to))){
            add_val_lab(x) = to
        } 
        recoded = recoded | cond # we don't recode already recoded value
    }

    x
}




#' @export
if_val.list = function(x, value){
    for(each in seq_along(x)){
        if_val(x[[each]]) = value
    }
    x
}

all_other = function(cond){
    identical(cond, as.symbol(".")) || identical(cond, ".")
}



parse_formula = function(elementary_recoding){
    # strange behavior with parse_formula.formula - it doesn't work with formulas so we use default method and check argument type
    stopif(!inherits(elementary_recoding, what = "formula"),"All recodings should be formula but:",elementary_recoding)
    formula_envir = environment(elementary_recoding)
    formula_list = as.list(elementary_recoding)
    from = formula_list[[2]]
    if (!all_other(from)) from = eval(from, envir = formula_envir)
    to = eval(formula_list[[3]], envir = formula_envir)
    list(from = from, to = to)
}




#### TODO
### replace "<5" notation with functions - 
### lt(5), and as in Fortran le, eq, ne, ge, gt()
### more managebale - for example we can skip non-numeric values

