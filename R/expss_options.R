#' Options for controlling behavior of the package
#' 
#' All options can be set with \code{options(option.name = option.value)} or
#' with special functions (see below). You can get value of option with
#' \code{getOption("option.name").}
#' \itemize{
#' \item{\code{expss.digits} }{ Number of digits after decimal separator which
#' will be shown for tables. This parameter is supported in the
#' \link[expss]{as.datatable_widget}, \link[expss]{htmlTable.etable} and \code{print}
#' methods. \code{NULL} is default and means one digit. \code{NA} means no
#' rounding. There is a convenience function for this option:
#' \code{expss_digits}.}
#' \item{\code{expss.enable_value_labels_support} }{By default, all labelled 
#' variables will use labels as labels for factor levels when \link{factor} is 
#' called. So any function which calls \link{factor}/\link{as.factor} will use 
#' value labels. In details this option changes behavior of two methods for
#' class \code{labelled} - \code{as.character} and \code{unique} - on which 
#' \code{factor} depends entirely. If you have compatibility problems set this 
#' option to zero: \code{options(expss.enable_value_labels_support = 0)}. There
#' are shortcuts for these options: \code{expss_enable_value_labels_support()}
#' and \code{expss_disable_value_labels_support()}.}
#' \item{\code{expss.output} }{ By default tables are printed in the console. 
#' You can change this behavior by setting this option. There are four possible
#' values: \code{'rnotebook'}, \code{'viewer'}, \code{'commented'} or 
#' \code{'raw'}. First option is useful when you run your code in the R Notebook
#' - output will be rendered to nice HTML. The second option will render tables 
#' to RStudio viewer. \code{knitr} is supported automatically via 
#' \code{knit_print} method. \code{'commented'} prints default output to the 
#' console with comment symbol (\code{#}) at the beginning of the each line.
#' With comment symbol you can easily copy and paste your output into the
#' script. Option \code{raw} disables any formatting and all tables are printed
#' as data.frames. Shortcuts for options: \code{expss_output_default()}, 
#' \code{expss_output_raw()}, \code{expss_output_viewer()},
#' \code{expss_output_commented()} and \code{expss_output_rnotebook()}.}
#' \item{\code{expss_fix_encoding_on}/\code{expss_fix_encoding_off} }{ If you
#' expreience problems with character experience in RStudio Viewer/RNotebooks under Windows
#' try \code{expss_fix_encoding_on()}. In some cases it can help.}
#' }
#' 
#' @param digits integer. Number of digits after decimal point. \code{NULL} is
#'   default and means 1 digit. \code{NA} means no rounding.
#' 
#' @name expss.options


#' @rdname expss.options
#' @export
expss_digits = function(digits = NULL){
    options(expss.digits = digits)
}


#' @rdname expss.options
#' @export
get_expss_digits = function(){
    digits = getOption("expss.digits")
    digits = if_null(digits, 1)
    digits
}


#' @rdname expss.options
#' @export
expss_enable_value_labels_support = function(){
    options(expss.enable_value_labels_support = NULL)
}

#' @rdname expss.options
#' @export
expss_disable_value_labels_support = function(){
    options(expss.enable_value_labels_support = 0)
}

#' @rdname expss.options
#' @export
expss_output_default = function(){
    options(expss.output = "")
}

#' @rdname expss.options
#' @export
expss_output_commented = function(){
    options(expss.output = "commented")
}

#' @rdname expss.options
#' @export
expss_output_raw = function(){
    options(expss.output = "raw")
}

#' @rdname expss.options
#' @export
expss_output_viewer = function(){
    options(expss.output = "viewer")
}

#' @rdname expss.options
#' @export
expss_output_rnotebook = function(){
    options(expss.output = "rnotebook")
}

#' @rdname expss.options
#' @export
expss_fix_encoding_on = function(){
    options(expss.fix_encoding = TRUE)
}

#' @rdname expss.options
#' @export
expss_fix_encoding_off = function(){
    options(expss.fix_encoding = NULL)
}