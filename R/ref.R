#' Auxiliary functions to pass arguments to function by reference
#' 
#' These two functions aimed to simplify build functions with side-effects 
#' (e. g. for modifying variables in place). Of cause it is not the R way of 
#' doing things but sometimes it can save several keystrokes.
#'  
#' @param x Reference to variable, it is formula, ~var_name. 
#' @param value Value that should be assigned to modified variable.
#' 
#' @return 
#' \code{ref} returns value of referenced variable. 
#' \code{ref<-} modifies referenced variable.
#' 
#' @details To create reference to variable one can use formula: \code{b = ~a}.
#'   \code{b} is reference to \code{a}. So \code{ref(b)} returns value of
#'   \code{a} and \code{ref(b) = new_val} will modify \code{a}. If argument
#'   \code{x} of these functions is not formula then these functions have no
#'   effect e. g. \code{ref(a)} is identical to \code{a} and after \code{ref(a)
#'   = value} \code{a} is identical to \code{value}. It is not possible to use
#'   function as argument \code{x} in assignment form. For example, 
#'   \code{ref(some_function(x)) = some_value} will rise error. Use \code{y =
#'   some_function(x); ref(y) = some_value} instead.
#' 
#' @export
#' @examples
#' # Simple example
#' a = 1:3
#' b = ~a  # b is reference to 'a'
#' identical(ref(b),a) # TRUE
#' 
#' ref(b)[2] = 4 # here we modify 'a'
#' identical(a, c(1,4,3)) # TRUE
#' 
#' # usage inside function
#' 
#' # top 10 rows 
#' head10 = function(x){
#'  ds = head(ref(x), 10)
#'  ref(x) = ds
#'  invisible(ds) # for usage without references
#' }
#' 
#' data(iris)
#' ref_to_iris = ~iris
#' head10(ref_to_iris) # side-effect
#' nrow(iris) # 10
#' 
#' # argument is not formula - no side-effect 
#' data(mtcars)
#' mtcars10 = head10(mtcars)
#' 
#' nrow(mtcars10) # 10
#' nrow(mtcars) # 32
#' 
ref = function(x){
    UseMethod("ref")
}

#' @export
ref.default = function(x){
    x
}

#' @export
ref.formula = function(x){
    varname = all.vars(x)
    stopif(length(varname)!=1,"Reference should have only one variable name, e. g. ref_var = ~a")
    envir = environment(x)
    envir[[varname]]
}


#' @export
#' @rdname ref
'ref<-' = function(x,value){
    UseMethod("ref<-")
}

#' @export
'ref<-.default' = function(x,value){
    value
}

#' @export
'ref<-.formula' = function(x,value){
    varname = all.vars(x)
    stopif(length(varname)!=1,"Reference should have only one variable name, e. g. ref_var = ~a")
    envir = environment(x)
    envir[[varname]] = value
    x
}
