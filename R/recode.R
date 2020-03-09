#' Change, rearrange or consolidate the values of an existing or new variable. Inspired by the RECODE command from SPSS.
#' 
#' \code{recode} change, rearrange or consolidate the values of an existing 
#' variable based on conditions. Design of this function inspired by RECODE from
#' SPSS. Sequence of recodings provided in the form of formulas. For example, 
#' 1:2 ~ 1 means that all 1's and 2's will be replaced with 1. Each value will be
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
#' as \code{recode} - e. g. with formulas. But conditions
#' should be only logical and it doesn't operate on multicolumn objects.
#' 
#' @details 
#' Input conditions - possible values for left-hand side (LHS) of formula or
#' element of \code{from} list:
#' \itemize{
#' \item{vector/single value}{ All values in \code{x} which equal to elements of the
#' vector in LHS will be replaced with RHS.}
#' \item{function}{ Values for which function gives TRUE will be replaced with 
#' RHS. There are some special functions for the convenience - see \link{criteria}}.
#' \item{single logical value \code{TRUE}}{ It means all other unrecoded values
#' (ELSE in SPSS RECODE). All other unrecoded values will be changed to RHS of the
#' formula or appropriate element of \code{to}.} 
#' }
#' Output values - possible values for right-hand side (RHS) of formula or
#' element of \code{to} list:
#' \itemize{
#' \item{value}{ replace elements of \code{x}. This value will be
#' recycled across rows and columns of \code{x}.}
#' \item{vector}{ values of this vector will replace values in the corresponding
#' position in rows of \code{x}. Vector will be recycled across columns of
#' \code{x}.}
#' \item{function}{ This function will be applied to values of \code{x} which 
#' satisfy recoding condition. There is a special auxiliary function \code{copy} 
#' which just returns its argument. So, in the \code{recode} it just copies old 
#' value (COPY in SPSS RECODE).  See examples.}}
#' \code{\%into\%} tries to mimic SPSS 'INTO'. Values from left-hand side will 
#' be assigned to right-hand side. You can use \code{\%to\%} expression in the 
#' RHS of \code{\%into\%}. See examples. 
#' \code{lo} and \code{hi} are shortcuts for \code{-Inf} and \code{Inf}. They
#' can be useful in expressions with \code{\%thru\%}, e. g. \code{1 \%thru\%
#' hi}.
#' 
#' @param x vector/matrix/data.frame/list
#' @param ... sequence of formulas which describe recodings. They are used when
#'   \code{from}/\code{to} arguments are not provided.
#' @param with_labels logical. FALSE by default for 'recode' and TRUE for 'rec'.
#'   Should we also recode value labels with the same recodings as variable?
#' @param new_label one of "all", "range", "first", or "last". If we recode
#'   value labels ('with_labels = TRUE') how we will combine labels for
#'   duplicated values? "all" will use all labels, "range" will use first and
#'   last labels. See examples.
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
#' @return object of the same form as \code{x} with recoded values
#' @examples
#' # examples from SPSS manual
#' # RECODE V1 TO V3 (0=1) (1=0) (2, 3=-1) (9=9) (ELSE=SYSMIS)
#' v1  = c(0, 1, 2, 3, 9, 10)
#' recode(v1) = c(0 ~ 1, 1 ~ 0, 2:3 ~ -1, 9 ~ 9, TRUE ~ NA)
#' v1
#' 
#' # RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
#' qvar = c(1:20, 97, NA, NA)
#' recode(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% hi ~ 3, TRUE ~ 0)
#' # the same result
#' recode(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, ge(11) ~ 3, TRUE ~ 0)
#'
#' # RECODE STRNGVAR ('A', 'B', 'C'='A')('D', 'E', 'F'='B')(ELSE=' '). 
#' strngvar = LETTERS
#' recode(strngvar, c('A', 'B', 'C') ~ 'A', c('D', 'E', 'F') ~ 'B', TRUE ~ ' ')
#' 
#' # recode in place. Note that we recode only first six letters
#' recode(strngvar) = c(c('A', 'B', 'C') ~ 'A', c('D', 'E', 'F') ~ 'B')
#' strngvar
#'
#' # RECODE AGE (MISSING=9) (18 THRU HI=1) (0 THRU 18=0) INTO VOTER. 
#' age = c(NA, 2:40, NA)
#' voter = recode(age, NA ~ 9, 18 %thru% hi ~ 1, 0 %thru% 18 ~ 0)
#' voter
#' # the same result with '%into%'
#' recode(age, NA ~ 9, 18 %thru% hi ~ 1, 0 %thru% 18 ~ 0) %into% voter2
#' voter2
#' # recode with adding labels
#' voter = recode(age, "Refuse to answer" = NA ~ 9, 
#'                     "Vote" = 18 %thru% hi ~ 1, 
#'                     "Don't vote" = 0 %thru% 18 ~ 0)
#' voter
#' 
#' # recoding with labels
#' ol = c(1:7, 99)
#' var_lab(ol) = "Liking"
#' val_lab(ol)  = num_lab("
#'                      1 Disgusting
#'                      2 Very Poor
#'                      3 Poor
#'                      4 So-so
#'                      5 Good
#'                      6 Very good
#'                      7 Excellent
#'                      99 Hard to say
#'                      ")
#' 
#' recode(ol, 1:3 ~ 1, 5:7 ~ 7, TRUE ~ copy, with_labels = TRUE)
#' # 'rec' is a shortcut for recoding with labels. Same result: 
#' rec(ol, 1:3 ~ 1, 5:7 ~ 7, TRUE ~ copy)
#' # another method of combining labels
#' recode(ol, 1:3 ~ 1, 5:7 ~ 7, TRUE ~ copy, with_labels = TRUE, new_label = "range")
#' # example with from/to notation
#' # RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
#' list_from = list(1 %thru% 5, 6 %thru% 10, ge(11), TRUE)
#' list_to = list(1, 2, 3, 0)
#' recode(qvar, from_to(list_from, list_to))
#' 
#' 
#' list_from = list(NA, 18 %thru% hi, 0 %thru% 18)
#' list_to = list("Refuse to answer" = 9, "Vote" = 1, "Don't vote" = 0)
#' voter = recode(age, from_to(list_from, list_to))
#' voter
#' 
#' # 'ifs' examples
#' a = 1:5
#' b = 5:1
#' ifs(b>3 ~ 1)                       # c(1, 1, NA, NA, NA)
#' ifs(b>3 ~ 1, TRUE ~ 3)             # c(1, 1, 3, 3, 3)
#' ifs(b>3 ~ 1, a>4 ~ 7, TRUE ~ 3)    # c(1, 1, 3, 3, 7)
#' ifs(b>3 ~ a, TRUE ~ 42)            # c(1, 2, 42, 42, 42)
#' 
#' # advanced usage
#' #' # multiple assignment with '%into%'
#' set.seed(123)
#' x1 = runif(30)
#' x2 = runif(30)
#' x3 = runif(30)
#' # note nessesary brackets around RHS of '%into%'
#' recode(x1 %to% x3, gt(0.5) ~ 1, other ~ 0) %into% (x_rec_1 %to% x_rec_3)
#' fre(x_rec_1)
#' # the same operation with characters expansion
#' i = 1:3
#' recode(x1 %to% x3, gt(0.5) ~ 1, other ~ 0) %into% text_expand('x_rec2_{i}')
#' fre(x_rec2_1)
#' 
#' # factor recoding
#' a = factor(letters[1:4])
#' recode(a, "a" ~ "z", TRUE ~ copy) # we get factor
#' 
#' # example with function in RHS
#' data(iris)
#' new_iris = recode(iris, is.numeric ~ scale, other ~ copy)
#' str(new_iris)
#' 
#' set.seed(123)
#' a = rnorm(20)
#' # if a<(-0.5) we change it to absolute value of a (abs function)
#' recode(a, lt(-0.5) ~ abs, other ~ copy) 
#' 
#' # the same example with logical criteria
#' recode(a, when(a<(-.5)) ~ abs, other ~ copy) 
#' @export
recode = function(x, ..., with_labels = FALSE, new_label = c("all", "range", "first", "last")){
    UseMethod("recode")    
}

#' @export
#' @rdname recode
rec = function(x, ..., with_labels = TRUE, new_label = c("all", "range", "first", "last")){
  recode(x, ..., with_labels = with_labels, new_label = new_label)    
}


#' @export
#' @rdname recode
#' @usage NULL
if_val = recode


#' @export
recode.default = function(x, ..., with_labels = FALSE, new_label = c("all", "range", "first", "last")){
    stopif(!length(list(...)), "'recode': recoding formula should be provided.")
    process_recodings(x, unlist(list(...), recursive = TRUE), make_empty_vec(x),
                      with_labels = with_labels, new_label = new_label)
    
}


process_recodings = function(x, recoding_formulas, res, 
                             with_labels, 
                             new_label = c("all", "range", "first", "last")){
    recoding_list = lapply(recoding_formulas, parse_formula)
    recoded = logical(length(x))
    labels = names(recoding_list)
    value_labels = c()
    for (i in seq_along(recoding_list)){
        each_recoding = recoding_list[[i]]
        target = each_recoding[["to"]]
         (length(target)>1 && length(target)!=length(res) ) &&
            stop("'recode' - length of 'RHS' should be
               1 or equals to length of 'x' but we have: ", expr_to_character(recoding_formulas[[i]]))
        
        curr_label = labels[[i]]
        if(length(curr_label)>0 && !is.na(curr_label) && curr_label!=""){
            if(is.function(target) || length(target)!=1 ||  is.na(target)){
                stop("'recode' - labelled recodings should recode into single not-NA value but we have: ", 
                     expr_to_character(recoding_formulas[[i]]))
            }
            value_labels[curr_label] = target 
        }
        if (all(recoded)) break # if all values were recoded
        crit = each_recoding[["from"]]
        if(isTRUE(crit)) crit = other
        if(!inherits(crit, "criterion")) {
          crit = as.criterion(crit)
        }
        cond = crit(x)
        cond = cond & !recoded # we don't recode already recoded value
        if(!any(cond)) next

        if(is.function(target)){
            # target: function
            res  = modify_vec(res, cond, target(x[cond]))
        } else {
            # target: vector
            if(length(target)==1){
                res  = modify_vec(res, cond, target)
            } else {
                res  = modify_vec(res, cond, target[cond])
            }
        }
        recoded = recoded | cond # we don't recode already recoded value
    }
    if(with_labels){
        vallab = val_lab(x)
        if(!is.null(vallab)){
            vallab = process_recodings(vallab, 
                                       # if recodings based on logical...!!??
                                       unname(recoding_formulas), 
                                       if(is.null(val_lab(res))) make_empty_vec(vallab) else vallab,
                                       with_labels = FALSE, 
                                       new_label = new_label)
            
            val_lab(res) = fix_new_labels(vallab, new_label = new_label)
        }
        var_lab(res) = var_lab(x)
        
    }
    if(length(value_labels)>0){
        add_val_lab(res) = value_labels
    }
    res
}

fix_new_labels = function(vallab, new_label = c("all", "range", "first", "last")){
    vallab = vallab[!is.na(vallab)]
    if(!anyDuplicated(vallab)) return(vallab)
    vallab = sort(vallab)
    labels = split(names(vallab), vallab)
    if(is.function(new_label)){
        labels = 
            vapply(labels, function(items){
                if(length(items)==1) return(items)
                as.character(new_label(items))
            }, FUN.VALUE = character(1))
    } else {
        new_label = match.arg(new_label[[1]], c("all", "range", "first", "last"))
        labels = unlist(
            lapply(labels, function(items){
                if(length(items)==1) return(items)
                switch(new_label,
                       all = paste(items, collapse = "/"),
                       range = paste0(items[1], " - ", items[length(items)]),
                       first = first(items),
                       last = last(items)
                )
            })
        )
    }
    vallab = unique(vallab)
    names(vallab) = labels
    vallab
}

modify_vec = function(x, condition, value){
    #### obvious and non-obvious fixes based on practice for smooth working
    if(is.factor(x)){
        fac_levels = levels(x)
        uniq_values = unique(value)
        if(!all(uniq_values %in% fac_levels)){
            fac_levels = union(fac_levels, uniq_values)
            levels(x) = fac_levels
        }
        x[condition] = as.character(value)
        return(x)
    } 
    ###########
    if(is.factor(value)){
        x = as.factor(x)
        return(modify_vec(x, condition, value))
    } 
    ###############
    if(inherits(value, "POSIXct") && !inherits(x, "POSIXct") & all(is.na(x))){
        # first assignment - we expect that x with all NA and set its class to POSIXct
        x = as.POSIXct(x)
        return(modify_vec(x, condition, value))
    } 
    if(inherits(value, "Date") && !inherits(x, "Date") && all(is.na(x))){
        # first assignment - we expect that x with all NA and set its class to Date
        x = as.Date(x)
        return(modify_vec(x, condition, value))
    } 
    x[condition] = value
    x
}  

  

#' @export
recode.list = function(x, ..., with_labels = FALSE, new_label = c("all", "range", "first", "last")){
    lapply(x, recode, ..., with_labels = with_labels, new_label = new_label)
}

#' @export
recode.data.frame = function(x, ..., with_labels = FALSE, new_label = c("all", "range", "first", "last")){
    for(i in seq_along(x)){
        x[[i]] = recode(x[[i]], ..., with_labels = with_labels, new_label = new_label)
    }
    x
}

#' @export
recode.matrix = function(x, ..., with_labels = FALSE, new_label = c("all", "range", "first", "last")){
    if(isTRUE(with_labels) || !is.null(names(list(...)))){
        warning("'recode' - trying to recode matrix with labels. Matrix can not have labels. All operations with labels will be ignored.")  
    } 
    res = recode(c(x, use.names = FALSE), ..., with_labels = FALSE, new_label = new_label)
    res = matrix(res, nrow = nrow(x), ncol = ncol(x))
    dimnames(res) = dimnames(x)
    res
}

################# recode<- ###############

#' @export
#' @rdname recode
"recode<-" = function(x, with_labels = FALSE, new_label = c("all", "range", "first", "last"), value){
    UseMethod("recode<-")
}

#' @export
#' @rdname recode
#' @usage NULL
"if_val<-" = `recode<-`

#' @export
#' @rdname recode
"rec<-" = function(x, with_labels = TRUE, new_label = c("all", "range", "first", "last"), value){
  recode(x, with_labels = with_labels, new_label = new_label) = value
  x
}


#' @export
"recode<-.default" = function(x, with_labels = FALSE, new_label = c("all", "range", "first", "last"), value){
    value = unlist(value, recursive = TRUE) # for case when we have nested lists in 'value'
    if(!is.list(value)) value = list(value) # for case when 'value' is single formula
    process_recodings(x, value, x, with_labels = with_labels, new_label = new_label)
}

#' @export
"recode<-.list" = function(x, with_labels = FALSE, new_label = c("all", "range", "first", "last"), value){
    for(each in seq_along(x)){
        recode(x[[each]], with_labels = with_labels, new_label = new_label) = value
    }
    x
}

#' @export
"recode<-.data.frame" = function(x, with_labels = FALSE, new_label = c("all", "range", "first", "last"), value){
  for(each in seq_along(x)){
    recode(x[[each]], with_labels = with_labels, new_label = new_label) = value
  }
  fix_datatable(x)
}

#' @export
"recode<-.matrix" = function(x, with_labels = FALSE, new_label = c("all", "range", "first", "last"), value){
    if(isTRUE(with_labels) || !is.null(names(unlist(value, recursive = TRUE)))){
        warning("'recode' - trying to recode matrix with labels. Matrix can not have labels. All operations with labels will be ignored.")  
    } 
    res = c(x, use.names = FALSE)
    recode(res, with_labels = FALSE, new_label = new_label) = value
    res = matrix(res, nrow = nrow(x), ncol = ncol(x))
    dimnames(res) = dimnames(x)
    res
}

parse_formula = function(elementary_recoding){
    # strange behavior with parse_formula.formula - it doesn't work with formulas so we use default method and check argument type
    if(!inherits(elementary_recoding, what = "formula")) {
        stop(paste0("'recode': all recodings should be formula but: ",  expr_to_character(elementary_recoding)))
    }
    if(length(elementary_recoding)!=3) {
        stop(paste0("'recode': all formulas should have left and right parts but: ",
                    expr_to_character(elementary_recoding)))
    }
    formula_envir = environment(elementary_recoding)
    from = elementary_recoding[[2]]
    # from = substitute_symbols(from, list(":" = `%thru%`))
    to = elementary_recoding[[3]]
    from = eval(from, envir = formula_envir)
    to = eval(to, envir = formula_envir)
    if(is.list(to) || is.data.frame(to)) {
        stop("'recode': formula RHS should be vector or function, but we have list or data.frame - ", 
             expr_to_character(elementary_recoding))
    }
    list(from = from, to = to)
}



############ ifs ####################

#' @export
#' @rdname recode
ifs = function(...){

    recoding_list = lapply(unlist(list(...)), parse_formula)
    from = lapply(recoding_list, "[[", "from")
    to = lapply(recoding_list, "[[", "to")
    
    from_test = lapply(from, as.matrix)
    test = vapply(from_test, is.logical, logical(1))
    stopif(!all(test), "'ifs': all conditions should be logical")
    from_rows = unique(vapply(from_test, nrow, numeric(1)))
    from_cols = unique(vapply(from_test, ncol, numeric(1)))
    stopif(!all(from_cols %in% 1), "'ifs': all conditions should be single column objects.")
    max_rows = max(from_rows, na.rm = TRUE)
    stopif(!all(from_rows %in% c(1, max_rows)), "'ifs': all values should have the same number of rows or have length 1.")
    res = rep(NA, max_rows)
    from = lapply(from, when)
    recode(res, from_to(from, to))
}

#' @export
#' @rdname recode
lo = -Inf

#' @export
#' @rdname recode
hi = Inf

#' @export
#' @rdname recode
copy = function(x) {
    if(missing(x)){
        copy
    } else {
        if(is.data.table(x)){
            data.table::copy(x)
        } else {
            x    
        }
    }
}    


#' @export
#' @rdname recode
from_to = function(from, to){
  stopifnot(
    length(from)>0,
    length(to)>0,
    length(from) == length(to)
  )
  if(is.function(from)) from = list(from)
  if(is.function(to)) to = list(to)
  res = lapply(seq_along(from), function(i) {
    force(i)
    from[[i]] ~ to[[i]]
  })
  names(res) = names(to)
  res
}


#' @export
#' @rdname recode
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
    if(length(variables_names)==1 && identical(variables_names[[1]], "")){
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



make_empty_vec = function(x){
  if(is.factor(x)){
    res = x
    res[] = NA
  } else {
    res = rep(NA, length(x))
    names(res) = names(x)
  }
  res
}