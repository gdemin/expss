#' Get variables/range of variables by name/by pattern.
#' 
#' \itemize{
#' \item{\code{vars}}{ returns data.frame with all variables by their names or
#' by criteria (see \link{criteria}). There is no non-standard evaluation in
#' this function by design so use quotes for names of your variables. This
#' function is intended to get variables by parameter/criteria. The only
#' exception with non-standard evaluation is \code{\%to\%}. You can use
#' \code{\%to\%} inside \code{vars} or independently.}
#' \item{\code{..p}}{ returns data.frame with all variables which names satisfy
#' supplied perl-style regular expression. Arguments for this function is quoted
#' characters. It is a shortcut for \code{vars(perl(pattern))}.}
#' \item{\code{..f}}{ returns data.frame with all variables which names contain
#' supplied pattern. Arguments for this function can be unquoted. It is a
#' shortcut for \code{vars(fixed(pattern))}.}
#' \item{\code{..t}}{ returns data.frame with variables which names are stored
#' in the supplied arguments. Expressions in characters in curly brackets are
#' expanded. See \link{text_expand}.}
#' \item{\code{..[]}}{ returns data.frame with all variables by their names or 
#' by criteria (see \link{criteria}).  Names at the top-level can be unquoted 
#' (non-standard evaluation). For standard evaluation of parameters you can 
#' surround them by round brackets. You can assign to this expression. If there 
#' are several names inside square brackets then each element of list/data.frame
#' from right side will be assigned to appropriate name from left side. You can 
#' use \code{item1 \%to\% item2} notation to get/create sequence of variables. 
#' If there are no arguments inside square brackets than from each item of RHS 
#' will be created separate variable in the parent frame. In this case RHS
#' should be named list or data.frame. }
#' \item{\code{..$name}}{ sets/returns object which name is stored in the
#' variable \code{name}. It is convenient wrapper around 
#' \link[base]{get}/\link[base]{assign} functions.}
#' \item{\code{\%to\%}}{ returns range of variables between \code{e1} and 
#' \code{e2} (similar to SPSS 'to'). \link{modify}, \link{modify_if}, 
#' \link{calculate}, \link{keep}, \link{except} and \link{where} support 
#' \code{\%to\%}.}
#' \item{\code{indirect}/\code{indirect_list}}{ are aliases for
#' \code{vars}/\code{vars_list}.}
#' }
#' Functions with word 'list' in name return lists of variables instead of 
#' dataframes.
#' @seealso \link{keep}, \link{except}, \link{do_repeat}, \link{compute},
#'   \link{calculate}, \link{where}
#' @param ... characters names of variables or criteria/logical functions
#' @param e1 unquoted name of start variable (e. g. a_1)
#' @param e2 unquoted name of start variable (e. g. a_5) 
#' @return  data.frame/list with variables
#' @examples
#' # In data.frame
#' dfs = data.frame(
#'     a = rep(1, 5),
#'     b_1 = rep(11, 5),
#'     b_2 = rep(12, 5),
#'     b_3 = rep(13, 5),
#'     b_4 = rep(14, 5),
#'     b_5 = rep(15, 5) 
#' )
#' 
#' # calculate sum of b_* variables
#' compute(dfs, {
#'     b_total = sum_row(b_1 %to% b_5)
#' })
#' 
#' # identical result
#' compute(dfs, {
#'     b_total = sum_row(..f(b_))
#' })
#' 
#' compute(dfs, {
#'     b_total = sum_row(..t("b_{1:5}"))
#' })
#' 
#' # In global environement
#' a = rep(10, 5)
#' a1 = rep(1, 5)
#' a2 = rep(2, 5)
#' a3 = rep(3, 5)
#' a4 = rep(4, 5)
#' a5 = rep(5, 5)
#' 
#' # identical results
#' a1 %to% a5
#' vars(perl("^a[0-9]$"))
#' ..[perl("^a[0-9]$")]
#' ..p("^a[0-9]$")
#' ..t("a{1:5}")
#' 
#' # sum each row
#' sum_row(a1 %to% a5)
#' 
#' # variable substitution
#' name1 = "a"
#' name2 = "new_var"
#' 
#' # in global environment
#' ..$name1 # give as variable 'a'
#' 
#' ..$name2 = ..$name1 * 2 # create variable 'new_var' which is equal to 'a' times 2
#' new_var
#' 
#' # inside data.frame
#' compute(dfs, {
#'      ..$name2 = ..$name1*2    
#' })
#' 
#' compute(dfs, {
#'      for(name1 in paste0("b_", 1:5)){
#'          name2 = paste0("new_", name1) 
#'          ..$name2 = ..$name1*2 
#'      }
#'      rm(name1, name2) # we don't need this variables as columns in 'dfs'
#' })
#' 
#' # square brackets notation - multi-assignment
#' name1 = paste0("b_", 1:5)
#' compute(dfs, {
#'           # round brackets about 'name1' is needed to avoid using it 'as is'
#'          ..[paste0("new_", name1)] = ..[(name1)]*2  
#' })
#' 
#' # the same result
#' # note the automatic creation of sequence of variables
#' compute(dfs, {
#'          ..[new_b_1 %to% new_b_5] = ..[b_1 %to% b_5]*2  
#' })
#' 
#' # assignment form of 'recode' on multiple variables
#' compute(dfs, {
#'          recode(..[b_1 %to% b_5]) = 13 %thru% hi ~ 20   
#' })
#' 
#' # empty brackets - unboxing of dichotomy.
#' compute(dfs, {
#'          ..[] =  as.dichotomy(b_1 %to% b_5, prefix = "v_")   
#' })
#' @export
vars = function(...){
    variables_names = substitute(list(...))
    res = internal_vars_list(variables_names, parent.frame())
    as.sheet(res)
}



#' @export
#' @rdname vars
vars_list = function(...){
    variables_names = substitute(list(...))
    internal_vars_list(variables_names, parent.frame())
}

internal_vars_list = function(variables_names, envir, symbols_to_characters = FALSE){
    stopif(length(variables_names)<2, 
           "'vars'/'vars_list' - you should provide at least one argument.")
    curr_names = get_current_variables(envir)
    new_vars = variables_names_to_indexes(curr_names, 
                                          variables_names, 
                                          envir = envir, 
                                          symbols_to_characters = symbols_to_characters)
    mget(curr_names[new_vars], envir = envir, inherits = TRUE)
}

#' @export
#' @rdname vars
indirect = vars

#' @export
#' @rdname vars
indirect_list = vars_list


#' @export
#' @rdname vars
'%to%' = function(e1, e2){
    # e1 = substitute(e1)
    # e2 = substitute(e2)
    eval(substitute(expss::vars(e1 %to% e2)),
         envir = parent.frame(),
         enclos = baseenv()
    )
}

#' @export
#' @rdname vars
'%to_list%' = function(e1, e2){
    # e1 = substitute(e1)
    # e2 = substitute(e2)
    eval(substitute(expss::vars_list(e1 %to% e2)),
         envir = parent.frame(),
         enclos = baseenv()
    )
}

###################################


# version of %to% for usage inside 'keep'/'except'/'vars'
# \code{.internal_to_} is for internal usage and not documented.


.internal_to_ =function(e1, e2){
    e1 = substitute(list(e1))
    e2 = substitute(list(e2))
    e1 = evaluate_variable_names(e1, envir = parent.frame(), symbols_to_characters = TRUE)
    e2 = evaluate_variable_names(e2, envir = parent.frame(), symbols_to_characters = TRUE)
    stopif(length(e1)>1, "'%to%' - length of name of first variable is greater than one.")
    stopif(length(e2)>1, "'%to%' - length of name of second variable is greater than one.")
    e1 = e1[[1]]
    e2 = e2[[1]]
    res = function(y){
        first = match(e1, y)[1]
        stopif(is.na(first), "'",e1, "' not found." )
        last = match(e2, y)[1]
        stopif(is.na(last), "'",e2, "' not found." )
        stopif(last<first, "'",e2, "' located before '",e1,"'. Did you mean '",e2," %to% ",e1,"'?")
        positions = seq_along(y)
        (positions>=first) & (positions<=last)         
    } 
    class(res) = union("criterion",class(res))
    res
    
}

# expr of .internal_to_

expr_internal_to = as.call(list(as.name(":::"), as.name("expss"), as.name(".internal_to_")))


###################################
internal_parameter_set = function(name, value, envir){
    
    if(is.character(name)){
        expr = parse(text = name)
        if(length(expr)!=1){
            stop(paste0("'..$': incorrect expression '", name, "'."))
        }
        expr = expr[[1]]
    } else if(inherits(name, "formula")){
        expr = name[[2]]    
    } else if(is.language(name)){
        expr = name
    } else {
        stop("'..' - variable name should be character, formula or language.")    
    }
    expr = bquote(.(expr)<-.(value))
    eval(expr, envir = envir, enclos = baseenv())
    invisible(NULL)
}

internal_parameter_get = function(name, envir){
    
    if(is.character(name)){
        expr = parse(text = name)
        if(length(expr)!=1){
            stop(paste0("'..$': incorrect expression '", name, "'."))
        }
        expr = expr[[1]]
    } else if(inherits(name, "formula")){
        expr = name[[2]]    
    } else if(is.language(name)){
        expr = name
    } else {
        stop("'..' - variable name should be character, formula or language.")    
    }
    eval(expr, envir = envir, enclos = baseenv())
}

get_parameter = function(name, envir){
    stopif(length(name)!=1, "'..' - variable name should be a vector of length 1.")
    name = as.character(name)
    get(name, pos = envir, inherits = TRUE)
}

#' @export
#' @rdname vars
'..' = 'Object for variable substitution. Usage: `..$varname` or `..["varname"]`.'


class(..) = "parameter"


#' @export
print.parameter = function(x, ...){
    cat(x, '\n')
    invisible(x)
}

#' @export
'$.parameter' = function(x, name){
    name = get_parameter(name, envir = parent.frame())  
    internal_parameter_get(name, envir = parent.frame())  
}

#' @export
'$<-.parameter' = function(x, name, value){
    # value = substitute(value)
    name = get_parameter(name, envir = parent.frame())
    internal_parameter_set(name, value, envir = parent.frame())
    x
}


#' @export
'[.parameter' = function(x, ...){
    envir = parent.frame()
    variables_names = substitute(list(...))
    res = internal_vars_list(variables_names, envir, symbols_to_characters = TRUE)
    as.sheet(res)
}

#' @export
'[<-.parameter' = function(x, ..., value){
    variables_names = substitute(list(...))
    into_internal(value, variables_names, parent.frame())
    x
}

############################################


############################################

'.internal_parameter_' = "Object for internal variable substitution. Don't use it."


class(.internal_parameter_) = "internal_parameter"

expr_internal_parameter = as.call(list(as.name(":::"), as.name("expss"), as.name(".internal_parameter_")))



#' @export
'$.internal_parameter' = function(x, name){
    get_parameter(name, envir = parent.frame())  
    
}

#' @export
'$<-.internal_parameter' = function(x, name, value){
    .NotYetImplemented()
}


#' @export
'[.internal_parameter' = function(x, ...){
    envir = parent.frame()
    variables_names = substitute(list(...))
    evaluate_variable_names(variables_names, 
                            envir, 
                            symbols_to_characters = TRUE)
}

#' @export
'[<-.internal_parameter' = function(x, ..., value){
    .NotYetImplemented()
}

#' @export
#' @rdname vars
..f = function(...){
    patterns = as.list(substitute(list(...)))[-1]
    patterns = lapply(patterns, deparse)
    res = vector(length(patterns), mode = "list")
    for(i in seq_along(patterns)){
        patt  = patterns[[i]]
        res[[i]] = eval.parent(substitute(vars_list(fixed(patt))))    
    }
    
    res = do.call(sheet, flat_list(res))
    if(anyDuplicated(names(res))) {
        warning(paste(c("..f - duplicated names:", names(res)), collapse = " "))
    }
    res    
}

#' @export
#' @rdname vars
..p = function(...){
    patterns = list(...)
    res = vector(length(patterns), mode = "list")
    for(i in seq_along(patterns)){
        patt  = patterns[[i]]
        res[[i]] = eval.parent(substitute(vars_list(perl(patt))))    
    }
    
    res = do.call(sheet, flat_list(res))
    if(anyDuplicated(names(res))) {
        warning(paste(c("..f - duplicated names:", names(res)), collapse = " "))
    }
    res   
}


#' @export
#' @rdname vars
..t = function(...){
    all_names = eval.parent(substitute(text_expand(...)))
    res = eval.parent(substitute(vars_list(all_names)))    
    res = do.call(sheet, flat_list(res))
    if(anyDuplicated(names(res))) {
        warning(paste(c("..t - duplicated names:", names(res)), collapse = " "))
    }
    res   
}