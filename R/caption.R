#' Add caption to the table
#' 
#' To drop caption use \code{set_caption} with \code{caption = NULL}. Captions
#' are supported by \link{htmlTable.etable}, \link{xl_write} and
#' \link{as.datatable_widget} functions.
#' @param obj object of class \code{etable} - result of \code{cro_cpct} and etc.
#' @param caption character caption for the table.
#'
#' @return object of class \code{with_caption}.
#' @examples 
#' 
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       vs = "Engine",
#'                       vs = num_lab("
#'                              0 V-engine 
#'                              1 Straight engine
#'                              "),
#'                       am = "Transmission",
#'                       am = num_lab("
#'                              0 Automatic 
#'                              1 Manual
#'                              ")
#' )
#' tbl_with_caption = calc_cro(mtcars, am, vs) %>% 
#'     set_caption("Table 1. Type of transimission.")
#'     
#' tbl_with_caption
#' 
#' @export
set_caption <- function (obj, caption) UseMethod("set_caption")


#' @export
set_caption.etable = function(obj, caption){
    if(length(caption)==0) {
        attr(obj, "caption") = NULL
        obj = remove_class(obj, "with_caption")
    } else {
        attr(obj, "caption") = caption
        obj = add_class(obj, "with_caption")
    }
    obj    
}

#' @export
set_caption.huxtable = function(obj, caption){
    huxtable::set_caption(obj, caption) 
}
#####################


#' @rawNamespace if(getRversion() >= "3.6.0") {
#' S3method(huxtable::"caption<-", etable)
#' } else {
#' export("caption<-.etable")
#' }
'caption<-.etable' <- function (obj, value) {
    expss::set_caption(obj, value)
}





#######################
#' @export
#' @rdname set_caption
get_caption <- function (obj) UseMethod("get_caption")

#' @export
get_caption.etable = function(obj){
    attr(obj, "caption", exact = TRUE)     
}

#' @rawNamespace if(getRversion() >= "3.6.0") {
#' S3method(huxtable::caption, etable)
#' } else {
#' export(caption.etable)
#' }
caption.etable = get_caption

############

#' @export
#' @rdname set_caption
is.with_caption = function(obj){
    inherits(obj, "with_caption")     
}