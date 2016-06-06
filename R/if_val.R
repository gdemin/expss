#' Title
#' 
#' 
#' 
#' @details 
#' Left hand side (LHS) of formula or element of \code{from} list:
#' \itemize{
#' \item{vector/single value}{ All values in \code{x} which equal to elements of vector in LHS will be replaced with RHS.}
#' \item{function}{ Values for which function gives TRUE will be replaced
#' with RHS. There are some special functions for convenience - see
#' \link{criteria}.}
#' \item{logical vector/matrix/data.frame}{ Values for which LHS equals to TRUE 
#' will be recoded. Logical vector will be recycled across all columns of 
#' \code{x}. If LHS is matrix/data.frame then column from this matrix/data.frame
#' will be used for correpsonding column/element of \code{x}.}
#' \item{.}{ Dot in LHS/\code{from} means all other unrecoded values (ELSE in SPSS RECODE). So all
#' other unrecoded values will be changed to RHS of formula or appropriate
#' element of \code{to}.}
#' }
#' Right hand side (RHS) of formula or element of \code{to} list:
#' \itemize{
#' \item{value}{ value which replace elements of \code{x}. This value will be
#' recycled across rows and columns of \code{x}.}
#' \item{vector}{ values of this vector will be replace values in corresponding
#' position in rows of \code{x}. Vector will be recycled across columns of
#' \code{x}.}
#' \item{list/matrix/data.frame}{ Element of list/column of matrix/data.frame
#' will be used as a replacement value for corresponding column/element of
#' \code{x}.}
#' \item{.}{ Dot in RHS/\code{to} means copy old value (COPY in SPSS RECODE).
#' In most cases there is no need for this option because by default
#' \code{if_val} doesn't modify values which don't satisfy any of conditions.}
#' }
#' 
#'
#' @param x vector/matrix/data.frame/list
#' @param ... sequence of formulas which describe recodings. Only used when \code{from}/\code{to} arguments are not provided. 
#' @param value list with formulas which describe recodings in assignment form of function. 
#' @param from list of conditions for values which should be recoded (in the same format as LHS's of formulas). 
#' @param to list of values into which old values should be recoded (in the same format as RHS's of formulas). 
#'
#' @return object of same form as \code{x} with recoded values
#' @examples
#' # RECODE V1 TO V3 (0=1) (1=0) (2,3=-1) (9=9) (ELSE=SYSMIS).
#' # RECODE STRNGVAR (’A’,’B’,’C’=’A’)(’D’,’E’,’F’=’B’)(ELSE=’ ’).
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
    recoded = matrix(FALSE, nrow = NROW(x), ncol = NCOL(x))
    dfs_x = as.data.frame(x)
    
    for (from_to in recoding_list){
        if (all(recoded)) break # if all values were recoded
        from = from_to$from
        if (all_other(from)){
            # dot is considered as all other non-recoded values ("else" from SPSS)
            cond = !recoded
        } else {
            #if (identical(from, NA)) from = as.numeric(NA)
            cond = build_criterion(from, dfs_x)
            cond = cond & !recoded # we don't recode already recoded value
        }
        to = from_to$to
        check_conformance(cond, to)
        # dot in `to` means copy (simply doesn't change values that meet condition - "copy" from SPSS ) 
        if(!is.list(to) || is.data.frame(to)){
            # to: matrix, data.frame, vector
            for (each_col in seq_len(NCOL(x))){
                curr_cond = column(cond, each_col)
                if (any(curr_cond)) column(x, each_col, curr_cond) = column(to, each_col, curr_cond)
            }
        } else {
            # to: list
            for (each_col in seq_len(NCOL(x))){
                curr_cond = column(cond, each_col)
                if (any(curr_cond))  if_val(column(x, each_col), from = list(curr_cond)) = list(column(to, each_col))
            }            
        }
        recoded = recoded | cond # we don't recode already recoded value
    }

    x
}




#' @export
if_val.list = function(x, ..., from = NULL, to = NULL){
    if (is.null(from) && is.null(to)){
        for(each in seq_along(x)){
            if_val(x[[each]]) = list(...)
        }
        
    } else {
        for(each in seq_along(x)){
            if_val(x[[each]], from = from) = to
        }
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


    




