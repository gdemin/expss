#' Change, rearrange or consolidate the values of an existing variable. Inspired by RECODE command from SPSS.
#' 
#' \code{if_val} change, rearrange or consolidate the values of an existing 
#' variable based on conditions. Design of this function inspired by RECODE from
#' SPSS. Sequence of recodings provided in the form of formulas. For example,
#' 1:2 ~ 1 means that all 1 and 2 will be replaced with 1. Each value recoded
#' only once. Values which doesn't meet any condition remain unchanged. As a
#' condition one can use just values or more sophisticated logical values and
#' functions. There are several special functions for usage as criteria - for
#' details see \link{criteria}. Simple common usage looks like: \code{if_val(x,
#' 1:2 ~ -1, 3 ~ 0, 1:2 ~ 1, 99 ~ NA)}. For more information, see details and
#' examples.
#' 
#' @details 
#' Input conditions: possible values for left hand side (LHS) of formula or element of \code{from} list:
#' \itemize{
#' \item{vector/single value}{ All values in \code{x} which equal to elements of vector in LHS will be replaced with RHS.}
#' \item{function}{ Values for which function gives TRUE will be replaced
#' with RHS. There are some special functions for convenience - see
#' \link{criteria}.}
#' \item{logical vector/matrix/data.frame}{ Values for which LHS equals to TRUE 
#' will be recoded. Logical vector will be recycled across all columns of 
#' \code{x}. If LHS is matrix/data.frame then column from this matrix/data.frame
#' will be used for corresponding column/element of \code{x}.}
#' \item{.}{ Dot in LHS/\code{from} means all other unrecoded values (ELSE in SPSS RECODE). So all
#' other unrecoded values will be changed to RHS of formula or appropriate
#' element of \code{to}.}
#' }
#' Output values: possible values for right hand side (RHS) of formula or element of \code{to} list:
#' \itemize{
#' \item{value}{ replace elements of \code{x}. This value will be
#' recycled across rows and columns of \code{x}.}
#' \item{vector}{ values of this vector will be replace values in corresponding
#' position in rows of \code{x}. Vector will be recycled across columns of
#' \code{x}.}
#' \item{list/matrix/data.frame}{ Element of list/column of matrix/data.frame
#' will be used as a replacement value for corresponding column/element of
#' \code{x}.}
#' \item{function}{ This function will be applied to values of \code{x} which satisfy recoding condition.}
#' \item{.}{ Dot in RHS/\code{to} means copy old value (COPY in SPSS RECODE).
#' In most cases there is no need for this option because by default
#' \code{if_val} doesn't modify values which don't satisfy any of conditions.}
#' }
#' 
#' @param x vector/matrix/data.frame/list
#' @param ... sequence of formulas which describe recodings. Only used when \code{from}/\code{to} arguments are not provided. 
#' @param value list with formulas which describe recodings in assignment form
#'   of function/\code{to} list if \code{from}/\code{to} notation is used.
#' @param from list of conditions for values which should be recoded (in the same format as LHS of formulas). 
#' @param to list of values into which old values should be recoded (in the same format as RHS of formulas). 
#'
#' @return object of same form as \code{x} with recoded values
#' @examples
#' # some examples from SPSS manual
#' # RECODE V1 TO V3 (0=1) (1=0) (2, 3=-1) (9=9) (ELSE=SYSMIS)
#' set.seed(123)
#' v1  = sample(c(0:3, 9, 10), 20, replace = TRUE)
#' if_val(v1) = c(0 ~ 1, 1 ~ 0, 2:3 ~ -1, 9 ~ 9, . ~ NA)
#' v1
#' 
#' # RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
#' set.seed(123)
#' qvar = sample((-5):20, 50, replace = TRUE)
#' if_val(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% Inf ~ 3, . ~ 0)
#' # the same result
#' if_val(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, gte(11) ~ 3, . ~ 0)
#'
#' # RECODE STRNGVAR ('A', 'B', 'C'='A')('D', 'E', 'F'='B')(ELSE=' '). 
#' strngvar = LETTERS
#' if_val(strngvar, c('A', 'B', 'C') ~ 'A', c('D', 'E', 'F') ~ 'B', . ~ ' ')
#'
#' # RECODE AGE (MISSING=9) (18 THRU HI=1) (0 THRU 18=0) INTO VOTER. 
#' set.seed(123)
#' age = sample(c(sample(5:30, 40, replace = TRUE), rep(9, 10)))
#' voter = if_val(age, 9 ~ NA, 18 %thru% Inf ~ 1, 0 %thru% 18 ~ 0)
#' voter
#' 
#' # example with function in RHS
#' set.seed(123)
#' a = rnorm(20)
#' # if a<(-0.5) we change it to absolute value of a (abs function)
#' if_val(a, lt(-0.5) ~ abs) 
#' 
#' # the same example with logical criteria
#' if_val(a, a<(-.5) ~ abs) 
#' 
#' # replace with specific value for each column
#' # we replace values greater than 0.75 with column max and values less than 0.25 with column mean
#' # and NA with column means
#' # make data.frame
#' set.seed(123)
#' x1 = runif(30)
#' x2 = runif(30)
#' x3 = runif(30)
#' x1[sample(30, 10)] = NA # place 10 NA's
#' x2[sample(30, 10)] = NA # place 10 NA's
#' x3[sample(30, 10)] = NA # place 10 NA's
#' dfs = data.frame(x1, x2, x3)
#' 
#' #replacement. Note the necessary transpose operation
#' if_val(dfs, lt(0.25) ~ t(min_col(dfs)), gt(0.75) ~ t(max_col(dfs)), NA ~ t(mean_col(dfs)))
#' 
#' # replace NA with row means
#' # some row which contain all NaN remain unchanged because mean_row for them also is NaN
#' if_val(dfs, NA ~ mean_row(dfs)) 
#' 
#' # some of the above examples with from/to notation
#' 
#' set.seed(123)
#' v1  = sample(c(0:3,9,10), 20, replace = TRUE)
#' # RECODE V1 TO V3 (0=1) (1=0) (2,3=-1) (9=9) (ELSE=SYSMIS)
#' fr = list(0, 1, 2:3, 9, ".")
#' to = list(1, 0, -1, 9, NA)
#' if_val(v1, from = fr) = to
#' v1
#' 
#' # RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
#' fr = list(1 %thru% 5, 6 %thru% 10, gte(11), ".")
#' to = list(1, 2, 3, 0)
#' if_val(qvar, from = fr, to = to)
#' 
#' # RECODE STRNGVAR ('A','B','C'='A')('D','E','F'='B')(ELSE=' ').
#' fr = list(c('A','B','C'), c('D','E','F') , ".")
#' to = list("A", "B", " ")
#' if_val(strngvar, from = fr, to = to)
#' 
#' # RECODE AGE (MISSING=9) (18 THRU HI=1) (0 THRU 18=0) INTO VOTER.
#' fr = list(9, 18 %thru% Inf, 0 %thru% 18)
#' to = list(NA, 1, 0)
#' voter = if_val(age, from = fr, to = to)
#' voter
#' 
#' @export
if_val = function(x, ..., from = NULL, to = NULL){
    UseMethod("if_val")
    
}

#' @export
#' @rdname if_val
"if_val<-" = function(x, from = NULL, value){
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
        if (!is.function(to)) check_conformance(cond, to)
        # dot in `to` means copy (simply doesn't change values that meet condition - "copy" from SPSS ) 
        if(!is.list(to) || is.data.frame(to) || is.function(to)){
            if(is.function(to)){
                # to: function
                for (each_col in seq_len(NCOL(x))){
                    curr_cond = column(cond, each_col)
                    if (any(curr_cond)) column(x, each_col, curr_cond) = to(column(x, each_col, curr_cond))
                }
                
                
            } else {
                # to: matrix, data.frame, vector
                for (each_col in seq_len(NCOL(x))){
                    curr_cond = column(cond, each_col)
                    if (any(curr_cond)) column(x, each_col, curr_cond) = column(to, each_col, curr_cond)
                }
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


    




