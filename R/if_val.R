#' Change, rearrange or consolidate the values of an existing/new variable. Inspired by RECODE command from SPSS.
#' 
#' \code{recode} change, rearrange or consolidate the values of an existing 
#' variable based on conditions. Design of this function inspired by RECODE from
#' SPSS. Sequence of recodings provided in the form of formulas. For example, 
#' 1:2 ~ 1 means that all 1's and 2's will be replaced with 1. Each value will 
#' recoded only once. In the assignment form \code{recode(...) = ...} of this 
#' function values which doesn't meet any condition remain unchanged. In case of
#' the usual form \code{... = recode(...)} values which doesn't meet any 
#' condition will be replaced with NA. One can use values or more sophisticated
#' logical conditions and functions as a condition. There are several special 
#' functions for usage as criteria - for details see \link{criteria}. Simple 
#' common usage looks like: \code{recode(x, 1:2 ~ -1, 3 ~ 0, 1:2 ~ 1, 99 ~ NA)}.
#' For more information, see details and examples.
#' The \code{ifs} function checks whether one or more conditions are met and
#' returns a value that corresponds to the first TRUE condition. \code{ifs} can
#' take the place of multiple nested \code{ifelse} statements and is much
#' easier to read with multiple conditions. \code{ifs} works in the same manner
#' as \code{recode} - e. g. with formulas or with from/to notation. But conditions
#' should be only logical and it doesn't operate on multicolumn objects.
#' 
#' @details 
#' Input conditions - possible values for left-hand side (LHS) of formula or
#' element of \code{from} list:
#' \itemize{
#' \item{vector/single value}{ All values in \code{x} which equal to elements of
#' vector in LHS will be replaced with RHS.}
#' \item{function}{ Values for which function gives TRUE will be replaced with 
#' RHS. There are some special functions for the convenience - see \link{criteria}.
#' One of special functions is \code{other}. It means all other unrecoded values
#' (ELSE in SPSS RECODE). All other unrecoded values will be changed to RHS
#' of formula or appropriate element of \code{to}.}
#' \item{logical vector/matrix/data.frame}{ Values for which LHS equals to TRUE 
#' will be recoded. Logical vector will be recycled across all columns of 
#' \code{x}. If LHS is matrix/data.frame then column from this matrix/data.frame
#' will be used for corresponding column/element of \code{x}.}
#' }
#' Output values - possible values for right-hand side (RHS) of formula or
#' element of \code{to} list:
#' \itemize{
#' \item{value}{ replace elements of \code{x}. This value will be
#' recycled across rows and columns of \code{x}.}
#' \item{vector}{ values of this vector will be replace values in corresponding
#' position in rows of \code{x}. Vector will be recycled across columns of
#' \code{x}.}
#' \item{list/matrix/data.frame}{ Element of list/column of matrix/data.frame
#' will be used as a replacement value for corresponding column/element of
#' \code{x}.}
#' \item{function}{ This function will be applied to values of \code{x} which 
#' satisfy recoding condition.There is special auxiliary function \code{copy} 
#' which just returns its argument. So in the \code{recode} it just copies old 
#' value (COPY in SPSS RECODE).  See examples. \code{copy} is useful in the
#' usual form of \code{recode} and doesn't do anything in the case of the
#' assignment form \code{recode() = ...} because this form don't modify values
#' which are not satisfying any of the conditions.}}
#' \code{\%into\%} tries to mimic SPSS 'INTO'. Values from left-hand side will 
#' be assigned to right-hand side. You can use \code{\%to\%} expression in the 
#' RHS of \code{\%into\%}. See examples. 
#' \code{lo} and \code{hi} are shortcuts for \code{-Inf} and \code{Inf}. They
#' can be useful in expressions with \code{\%thru\%}, e. g. \code{1 \%thru\%
#' hi}. \code{if_val} is an alias for \code{recode}.
#' 
#' @param x vector/matrix/data.frame/list
#' @param ... sequence of formulas which describe recodings. They are used when
#'   \code{from}/\code{to} arguments are not provided.
#' @param value list with formulas which describe recodings in assignment form
#'   of function/\code{to} list if \code{from}/\code{to} notation is used.
#' @param from list of conditions for values which should be recoded (in the
#'   same format as LHS of formulas).
#' @param to list of values into which old values should be recoded (in the same
#'   format as RHS of formulas).
#' @param values object(-s) which will be assigned to \code{names} for 
#'   \code{\%into\%} operation. \code{\%into\%} supports multivalue assignments.
#'   See examples.
#' @param names name(-s) which will be given to \code{values} expression. For 
#'   \code{\%into\%}.
#'
#' @return object of same form as \code{x} with recoded values
#' @examples
#' # `ifs` examples
#' a = 1:5
#' b = 5:1
#' ifs(b>3 ~ 1)                       # c(1, 1, NA, NA, NA)
#' ifs(b>3 ~ 1, TRUE ~ 3)             # c(1, 1, 3, 3, 3)
#' ifs(b>3 ~ 1, a>4 ~ 7, TRUE ~ 3)    # c(1, 1, 3, 3, 7)
#' ifs(b>3 ~ a, TRUE ~ 42)            # c(1, 2, 42, 42, 42)
#' # some examples from SPSS manual
#' # RECODE V1 TO V3 (0=1) (1=0) (2, 3=-1) (9=9) (ELSE=SYSMIS)
#' set.seed(123)
#' v1  = sample(c(0:3, 9, 10), 20, replace = TRUE)
#' recode(v1) = c(0 ~ 1, 1 ~ 0, 2:3 ~ -1, 9 ~ 9, other ~ NA)
#' v1
#' 
#' # RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
#' set.seed(123)
#' qvar = sample((-5):20, 50, replace = TRUE)
#' recode(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% hi ~ 3, other ~ 0)
#' # the same result
#' recode(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, ge(11) ~ 3, other ~ 0)
#'
#' # RECODE STRNGVAR ('A', 'B', 'C'='A')('D', 'E', 'F'='B')(ELSE=' '). 
#' strngvar = LETTERS
#' recode(strngvar, c('A', 'B', 'C') ~ 'A', c('D', 'E', 'F') ~ 'B', other ~ ' ')
#'
#' # RECODE AGE (MISSING=9) (18 THRU HI=1) (0 THRU 18=0) INTO VOTER. 
#' set.seed(123)
#' age = sample(c(sample(5:30, 40, replace = TRUE), rep(9, 10)))
#' voter = recode(age, NA ~ 9, 18 %thru% hi ~ 1, 0 %thru% 18 ~ 0)
#' voter
#' # the same result with '%into%'
#' recode(age, NA ~ 9, 18 %thru% hi ~ 1, 0 %thru% 18 ~ 0) %into% voter2
#' voter2
#' 
#' # multiple assignment with '%into%'
#' #' set.seed(123)
#' x1 = runif(30)
#' x2 = runif(30)
#' x3 = runif(30)
#' # note nessesary brackets around RHS of '%into%'
#' recode(x1 %to% x3, gt(0.5) ~ 1, other ~ 0) %into% (x_rec_1 %to% x_rec_3)
#' fre(x_rec_1)
#' # the same operation with characters expansion
#' i = 1:3
#' recode(x1 %to% x3, gt(0.5) ~ 1, other ~ 0) %into% subst('x_rec2_`i`')
#' fre(x_rec2_1)
#' 
#' # example with function in RHS
#' set.seed(123)
#' a = rnorm(20)
#' # if a<(-0.5) we change it to absolute value of a (abs function)
#' recode(a, lt(-0.5) ~ abs, other ~ copy) 
#' 
#' # the same example with logical criteria
#' recode(a, a<(-.5) ~ abs, other ~ copy) 
#' 
#' # replace with specific value for each column
#' # we replace values greater than 0.75 with column max and values less than 0.25 with column min
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
#' recode(dfs, 
#'         lt(0.25) ~ t(min_col(dfs)), 
#'         gt(0.75) ~ t(max_col(dfs)), 
#'         NA ~ t(mean_col(dfs)), 
#'         other ~ copy
#'       )
#' 
#' # replace NA with row means
#' # some rows which contain only NaN's remain unchanged because mean_row for them also is NaN
#' recode(dfs, NA ~ mean_row(dfs), other ~ copy) 
#' 
#' # some of the above examples with from/to notation
#' 
#' set.seed(123)
#' v1  = sample(c(0:3,9,10), 20, replace = TRUE)
#' # RECODE V1 TO V3 (0=1) (1=0) (2,3=-1) (9=9) (ELSE=SYSMIS)
#' fr = list(0, 1, 2:3, 9, other)
#' to = list(1, 0, -1, 9, NA)
#' recode(v1, from = fr) = to
#' v1
#' 
#' # RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
#' fr = list(1 %thru% 5, 6 %thru% 10, ge(11), other)
#' to = list(1, 2, 3, 0)
#' recode(qvar, from = fr, to = to)
#' 
#' # RECODE STRNGVAR ('A','B','C'='A')('D','E','F'='B')(ELSE=' ').
#' fr = list(c('A','B','C'), c('D','E','F') , other)
#' to = list("A", "B", " ")
#' recode(strngvar, from = fr, to = to)
#' 
#' # RECODE AGE (MISSING=9) (18 THRU HI=1) (0 THRU 18=0) INTO VOTER.
#' fr = list(NA, 18 %thru% hi, 0 %thru% 18)
#' to = list(9, 1, 0)
#' voter = recode(age, from = fr, to = to)
#' voter
#' 
#' @export
if_val = function(x, ..., from = NULL, to = NULL){
    UseMethod("if_val")
    
}

#' @export
#' @rdname if_val
"if_val<-" = function(x, from = NULL, value){
    UseMethod("if_val<-")
}

#' @export
#' @rdname if_val
"recode<-" = `if_val<-`


#' @export
#' @rdname if_val
recode = if_val

#' @export
"if_val<-.default" = function(x, from = NULL, value){
    if (!is.null(from)){
        if(is.function(from)) from = list(from)
        if(is.function(value)) value = list(value)
        stopif(length(from)!=length(value), 
               "length(value) should be equal to length(from) but length(from) = ", length(from),
               " and length(value) = ", length(value))
        recoding_list = mapply(function(x,y) list(from = x, to = y), from, value, SIMPLIFY = FALSE)
    } else {
        if(inherits(value, what = "formula")) value = list(value)
        recoding_list = lapply(value, parse_formula)
    }
    
    recoded = matrix(FALSE, nrow = NROW(x), ncol = NCOL(x))
    dfs_x = as.data.frame(x,
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
    
    for (from_to in recoding_list){
        if (all(recoded)) break # if all values were recoded
        from = from_to$from
        
        #if (identical(from, NA)) from = as.numeric(NA)
        cond = build_criterion(from, dfs_x)
        cond = cond & !recoded # we don't recode already recoded value
        
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
        
        recoded = recoded | (cond & !is.na(cond)) # we don't recode already recoded value
    }
    
    x
}

#' @export
"if_val<-.list" = function(x, from = NULL, value){
    
    for(each in seq_along(x)){
        if_val(x[[each]], from = from) = value
    }
    x
}

#' @export
if_val.default = function(x, ..., from = NULL, to = NULL){
    if (is.null(from) && is.null(to)){
        stopif(!length(list(...)), "Formulas or `from`/`to` arguments should be provided.")
        recoding_list = lapply(unlist(list(...)), parse_formula)
    } else {
        stopif(is.null(from) || is.null(to), "Both 'from' and 'to' arguments should be not NULL.")
        stopif(length(from)!=length(to), 
               "length(to) should be equal to length(from) but length(from)=", length(from),
               " and length(to)=", length(to))
        
        recoding_list = mapply(function(x,y) list(from = x, to = y), from, to, SIMPLIFY = FALSE)
    }
    recoded = matrix(FALSE, nrow = NROW(x), ncol = NCOL(x))
    res = make_empty_object(x)
    x = as.data.frame(x,
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
    
    for (from_to in recoding_list){
        if (all(recoded)) break # if all values were recoded
        from = from_to$from
        #if (identical(from, NA)) from = as.numeric(NA)
        cond = build_criterion(from, x)
        cond = cond & !recoded # we don't recode already recoded value
        
        to = from_to$to
        
        if (!is.function(to)) check_conformance(cond, to)
        # dot in `to` means copy (simply doesn't change values that meet condition - "copy" from SPSS ) 
        if(!is.list(to) || is.data.frame(to) || is.function(to)){
            if(is.function(to)){
                # to: function
                for (each_col in seq_len(NCOL(x))){
                    curr_cond = column(cond, each_col)
                    if (any(curr_cond)) column(res, each_col, curr_cond) = to(column(x, each_col, curr_cond))
                }
                
                
            } else {
                # to: matrix, data.frame, vector
                for (each_col in seq_len(NCOL(x))){
                    curr_cond = column(cond, each_col)
                    if (any(curr_cond)) column(res, each_col, curr_cond) = column(to, each_col, curr_cond)
                }
            }
        } else {
            # to: list
            for (each_col in seq_len(NCOL(x))){
                curr_cond = column(cond, each_col)
                if (any(curr_cond))  if_val(column(res, each_col), from = list(curr_cond)) = list(column(to, each_col))
            }     
            
        }
        
        recoded = recoded | (cond & !is.na(cond)) # we don't recode already recoded value
    }
    
    res
}


#' @export
if_val.list = function(x, ..., from = NULL, to = NULL){
    lapply(x, if_val, ..., from = from, to = to)
}



parse_formula = function(elementary_recoding){
    # strange behavior with parse_formula.formula - it doesn't work with formulas so we use default method and check argument type
    stopif(!inherits(elementary_recoding, what = "formula"),"All recodings should be formula but: ", elementary_recoding)
    stopif(length(elementary_recoding)!=3,"All formulas should have left and right parts but: ",
           paste(elementary_recoding, collapse = " "))
    formula_envir = environment(elementary_recoding)
    from = elementary_recoding[[2]]
    to = elementary_recoding[[3]]
    from = eval(from, envir = formula_envir)
    to = eval(to, envir = formula_envir)
    list(from = from, to = to)
}

#' @export
#' @rdname if_val
ifs = function(... , from = NULL, to = NULL){
    if (is.null(from) && is.null(to)){
        recoding_list = lapply(unlist(list(...)), parse_formula)
        from = lapply(recoding_list, "[[", "from")
        to = lapply(recoding_list, "[[", "to")
    } else {
        stopif(is.null(from) || is.null(to), "Both 'from' and 'to' arguments should be not NULL.")
        stopif(length(from)!=length(to), 
               "length(to) should be equal to length(from) but length(from)=", length(from),
               " and length(to)=", length(to))
        
        
    } 
    from = lapply(from, as.matrix)
    test = vapply(from, is.logical, logical(1))
    stopif(!all(test), "All conditions should be logical")
    from_rows = unique(vapply(from, nrow, numeric(1)))
    from_cols = unique(vapply(from, ncol, numeric(1)))
    stopif(!all(from_cols %in% 1), "All conditions should be single column objects.")
    max_rows = max(from_rows, na.rm = TRUE)
    stopif(!all(from_rows %in% c(1, max_rows)), "All values should have the same number of rows or have length 1.")
    res = rep(NA, max_rows)
    if_na(from) = FALSE
    if_val(res, from = from, to = to)
}

#' @export
#' @rdname if_val
lo = -Inf

#' @export
#' @rdname if_val
hi = Inf

#' @export
#' @rdname if_val
copy = function(x) {
    if(missing(x)){
        copy
    } else {
        x
    }
}    

#' @export
#' @rdname if_val
'%into%' = function(values, names){
    variables_names = substitute(names)
    if(length(variables_names)==1){
        variables_names = substitute(list(names))
    }
    into_internal(values, variables_names, parent.frame())
}





into_internal = function(values, variables_names, envir){
    variables_names = substitute_symbols(variables_names,
                       list("%to%" = expr_into_helper,
                            ".." = expr_internal_parameter)
                       )
    existing_vars = get_current_variables(envir)
    variables_names = as.list(variables_names)
    variables_names[-1] = convert_top_level_symbols_to_characters(variables_names[-1])
    variables_names = as.call(variables_names)
    variables_names = eval(variables_names, envir = envir,
                       enclos = baseenv())
    variables_names = flat_list(variables_names)
    # strange condition because of missing(...) doesn't work
    if(length(variables_names)==1 && is.character(variables_names[[1]]) && variables_names[[1]]==""){
        stopif(!is.list(values), "Unboxing can be applied only to list/data.frame.")
        variables_names = names(values)
        stopif(is.null(variables_names), "There are no names in 'x'.")
        stopif(any(c("", NA) %in% variables_names), "There are empty names in 'x'.")
    }
    for(i in seq_along(variables_names)){
        each_name = variables_names[[i]]
        if(is.function(each_name)){
            variables_names[[i]] = v_intersect(existing_vars, each_name)
            existing_vars = v_diff(existing_vars, each_name)
        } 
    }
    variables_names = unlist(variables_names, use.names = FALSE)
    if(is.list(values)){
        n_elements = length(values)
    } else {
        n_elements = NCOL(values)
    }
    stopif(!((n_elements==length(variables_names)) || (n_elements==1)),
           "You provide ", length(variables_names), " names and ", n_elements,
           " items for them in assignment. Number of items should be equal to number of the names or equal to one."
    )
    for(each in seq_along(variables_names)){
        assign(variables_names[[each]], column(values, each), envir = envir)
    }
    invisible(NULL)
}


# version of %to% for usage inside %into%'
.into_helper_ = function(e1, e2){
    var_names = get_current_variables(parent.frame())
    e1 = substitute(list(e1))
    e2 = substitute(list(e2))
    e1 = evaluate_variable_names(e1, envir = parent.frame(), symbols_to_characters = TRUE)
    e2 = evaluate_variable_names(e2, envir = parent.frame(), symbols_to_characters = TRUE)
    stopif(length(e1)>1, "'%to%' - length of name of first variable is greater than one.")
    stopif(length(e2)>1, "'%to%' - length of name of second variable is greater than one.")
    e1 = e1[[1]]
    e2 = e2[[1]]
    first = match(e1, var_names)[1]
    last = match(e2, var_names)[1]
    if(is.na(first) && is.na(last)){
        patt1 = gsub("^(.+?)([\\d]+)$", "\\1", e1, perl = TRUE)
        patt2 = gsub("^(.+?)([\\d]+)$", "\\1", e2, perl = TRUE)
        stopif(patt1!=patt2, "Start and end variables begin from different patterns: '", patt1, "', '", patt2,"'.")
        digits1 = gsub("^(.+?)([\\d]+)$", "\\2", e1, perl = TRUE)
        digits2 = gsub("^(.+?)([\\d]+)$", "\\2", e2, perl = TRUE)
        padding = 0
        if((substr(digits1,1,1)=="0" || substr(digits2,1,1)==0) &&
           !(substr(digits1,1,1)=="0" && nchar(digits1)==1 && substr(digits2,1,1)!=0)){
            stopif(nchar(digits1)!=nchar(digits2), 
                   "Invalid use of the %to% convention. For zero-padded numbers numeric part of the names must be the same length but: '", 
                   digits1, ", '", digits2, "'.")
            padding = nchar(digits1)
        }
        digits1 = as.numeric(digits1)
        digits2 = as.numeric(digits2)
        stopif(digits1>digits2, "Name of start variables greater than name of end variables: '", e1,"' > '",e2,"'.")
        all_digits = digits1:digits2
        if(padding>0) all_digits = formatC(all_digits, width = padding, format = "d", flag = "0")
        return(paste0(patt1, all_digits))
    } else { 
        stopif(is.na(first), "'", e2, "' is found but '", e1, "' is absent.")
        stopif(is.na(last), "'", e1, "' is found but '", e2, "' is absent.")
        stopif(last<first, "'",e2, "' located before '",e1,"'. Did you mean '",e2," %to% ",e1,"'?")
        return(var_names[first:last])         
    } 
}


expr_into_helper = as.call(list(as.name(":::"), as.name("expss"), as.name(".into_helper_")))


# make object with the same shape as its argument but filled with NA and logical type
make_empty_object = function(x){
    UseMethod("make_empty_object")    
}

#' @export
make_empty_object.data.frame = function(x){
    res = as.sheet(lapply(x, make_empty_object))
    row.names(res) = row.names(x)
    colnames(res) = colnames(x)
    res
} 

#' @export
make_empty_object.list = function(x){
    res = lapply(x, make_empty_object)
    names(res) = names(x)
    res
} 

#' @export
make_empty_object.matrix = function(x){
    res = matrix(NA, nrow = nrow(x), ncol = ncol(x))
    rownames(res) = rownames(x)
    colnames(res) = colnames(x)
    res
}   

# #' @export
# make_empty_object.POSIXct = function(x){
#     res = as.POSIXct(rep(NA, length(x)))
#     names(res) = names(x)
#     res
# } 

# #' @export
# make_empty_object.factor = function(x){
#     res = x
#     res[] = NA
#     res
# } 

#' @export
make_empty_object.default = function(x){
    res = rep(NA, length(x))
    names(res) = names(x)
    res
}
                