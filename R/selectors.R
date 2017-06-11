#' Get variables/range of variables by name/by pattern.
#' 
#' \itemize{
#' \item{\code{vars}}{ returns all variables by their names or by criteria (see 
#' \link{criteria}). There is no non-standard evaluation in this function by
#' design so use quotes for names of your variables or use \link{qc}. The only
#' exception with non-standard evaluation is \code{\%to\%}. You can use
#' \code{\%to\%} inside \code{vars} or independently.}
#' \item{\code{\%to\%}}{ returns range of variables between \code{e1} and 
#' \code{e2} (similar to SPSS 'to'). \link{modify}, \link{modify_if}, 
#' \link{calculate}, \link{keep}, \link{except} and \link{where} support 
#' \code{\%to\%}. Inside global environment \link[base]{with},
#' \link[base]{within} \code{\%to\%} will take range from names of variables
#' sorted in the alphabetic order.}
#' \item{\code{indirect}}{ are aliases for
#' \code{vars}/\code{vars_list}.}
#' }
#' Functions with word 'list' in name return lists of variables instead of 
#' dataframes.
#' \code{.internal_to_} is for internal usage and not documented.
#' @seealso \link{keep}, \link{except}, \link{do_repeat}, \link{compute}
#' @param ... characters names of variables or criteria/logical functions
#' @param e1 unquoted name of start variable (e. g. a_1)
#' @param e2 unquoted name of start variable (e. g. a_5) 
#' @return  data.frame/list with variables
#' @examples
#' # In data.frame
#' dfs = data.frame(
#'     a = rep(10, 5),
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
#' 
#' # sum each row
#' sum_row(a1 %to% a5)
#' 
#' # variable substitution
#' name1 = "a"
#' name2 = "new_var"
#' 
#' # example with short notation but it can be applied only for simple cases - 
#' # when 'name' is vector of length 1
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
#' # square brackets notation
#' compute(dfs, {
#'      ..[name2] = ..[name1]*2  
#' })
#' 
#' compute(dfs, {
#'      for(name1 in paste0("b_", 1:5)){
#'          ..[paste0("new_", name1)] = ..$name1*2 
#'      }
#'      rm(name1) # we don't need this variable as column in 'dfs'
#' })
#' 
#' # '..$' doesn't work for case below so we need to use square brackets form
#' name1 = paste0("b_", 1:5)
#' name2 = paste0("new_", name1)
#' compute(dfs, {
#'      for(i in 1:5){
#'          ..[name2[i]] = ..[name1[i]]*3
#'      }
#'      rm(i) # we don't need this variable as column in 'dfs'
#' })
#' 
#' @export
vars = function(...){
    variables_names = substitute(list(...))
    res = internal_vars_list(variables_names, parent.frame())
    as.dtfrm(res)
}



#' @export
#' @rdname vars
vars_list = function(...){
    variables_names = substitute(list(...))
    internal_vars_list(variables_names, parent.frame())
}

internal_vars_list = function(variables_names, envir){
    if(exists(".internal_column_names0", envir = envir)){
        curr_names = internal_ls(envir[[".internal_column_names0"]], env = envir)
    } else {
        curr_names = ls(envir = envir)
    }
    new_vars = keep_helper(curr_names, variables_names, envir = envir)
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
#' @export
#' @rdname vars
.internal_to_ = function(e1, e2){
    e1 = expr_to_character(substitute(e1))
    e2 = expr_to_character(substitute(e2))
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

###################################
internal_parameter_set = function(name, value, envir){
    stopif(length(name)!=1, "'..' - variable name should be a vector of length 1.")
    name = as.character(name)
    assign(name, value = value, pos = envir, inherits = FALSE)
    name
}

internal_parameter_get = function(name, envir){
    stopif(length(name)!=1, "'..' - variable name should be a vector of length 1.")
    name = as.character(name)
    get(name, pos = envir, inherits = TRUE)
}

#' @export
#' @rdname vars
'..' = 'Object for variable substitution. Usage: `..$varname` or `..["varname"]`'

class(..) = "parameter"


#' @export
'$.parameter' = function(x, name){
    name = internal_parameter_get(name, envir = parent.frame())  
    internal_parameter_get(name, envir = parent.frame())  
}

#' @export
'$<-.parameter' = function(x, name, value){
    name = internal_parameter_get(name, envir = parent.frame())
    internal_parameter_set(name, value, envir = parent.frame())
    x
}


#' @export
'[.parameter' = function(x, name){
    internal_parameter_get(name, envir = parent.frame())  
}

#' @export
'[<-.parameter' = function(x, name, value){
    internal_parameter_set(name, value, envir = parent.frame())
    x
}

