DOT_CONSTANT = as.list(.~.)[[2]]

#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return x
#' @examples
#' list()
#' @export
if_val = function(x, ...){
    UseMethod("if_val")
    
}

"if_val<-" = function(x, value){
    
    
}


#' @export
if_val.default = function(x, ...){
    recoding_list = unlist(list(...))
    recoded = rep(FALSE, length(x))
    res = x
    dfs_x = as.data.frame(x)
    for (each_recode in recoding_list){
        if (all(recoded)) break # if all values were recoded
        from_to = parse_formula(each_recode)
        from = from_to$from
        if (identical(from, DOT_CONSTANT)){
            # dot is considered as all other non-recoded values ("else" from SPSS)
            cond = !recoded
        } else {
            cond = build_criterion(from, dfs_x)
            if_na(cond) = FALSE
            cond = cond & !recoded # we don't recode already recoded value
        }
        to = from_to$to
        check_conformance(x, to)
        if (NROW(to)>1) {
            x[cond] = to[cond]
        } else {
            x[cond] = to
        }
        recoded = recoded | cond # we don't recode already recoded value
    }

    x
}

parse_formula = function(elementary_recoding){
    UseMethod("parse_formula")
}

parse_formula.formula = function(elementary_recoding){
    formula_envir = environment(elementary_recoding)
    formula_list = as.list(elementary_recoding)
    from = formula_list[[2]]
    if (!identical(from, DOT_CONSTANT)) from = eval(from, envir = formula_envir)
    to = eval(formula_list[[3]], envir = formula_envir)
    list(from = from, to = to)
}