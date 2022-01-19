#' Add caption to the table
#' 
#' To drop caption use \code{set_caption} with \code{caption = NULL}. Captions
#' are supported by \link{htmlTable.etable}, \link{xl_write} and
#' \link{as.datatable_widget} functions.
#' @param obj object of class \code{etable} - result of \code{cross_cpct} and etc.
#' @param caption character caption for the table.
#' @param footer character footer for the table.
#'
#' @return object of class \code{with_caption} for captions and original class
#'   for footer.
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
#' tbl_with_caption = cross_cases(mtcars, am, vs) %>% 
#'     set_caption("Table 1. Type of transimission.") %>% 
#'     set_footer("Source: Motor Trend US magazine, 1974")
#'     
#' tbl_with_caption
#' 
#' @export
set_caption <- function (obj, caption) UseMethod("set_caption")


#' @export
#' @rdname set_caption
set_footer <- function (obj, footer) UseMethod("set_footer")


#' @export
set_caption.default = function(obj, caption){
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
set_footer.default = function(obj, footer){
    if(length(footer)==0) {
        attr(obj, "footer") = NULL
    } else {
        attr(obj, "footer") = footer
    }
    obj    
}


#' @export
set_caption.etable = function(obj, caption){
   set_caption.default(obj, caption)  
}


#' @export
set_footer.etable = function(obj, footer){
    set_footer.default(obj, footer)  
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



########## get_caption #############
#' @export
#' @rdname set_caption
get_caption <- function (obj) UseMethod("get_caption")

#' @export
get_caption.default = function(obj){
    attr(obj, "caption", exact = TRUE)     
}

#' @export
get_caption.etable = function(obj){
    get_caption.default(obj)     
}

#' @rawNamespace if(getRversion() >= "3.6.0") {
#' S3method(huxtable::caption, etable)
#' } else {
#' export(caption.etable)
#' }
caption.etable = get_caption

######### get_footer ##############
#' @export
#' @rdname set_caption
get_footer <- function (obj) UseMethod("get_footer")

#' @export
get_footer.default = function(obj){
    attr(obj, "footer", exact = TRUE)     
}

#' @export
get_footer.etable = function(obj){
    get_footer.default(obj)     
}


############

#' @export
#' @rdname set_caption
is.with_caption = function(obj){
    inherits(obj, "with_caption")     
}