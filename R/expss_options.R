#' Options for contolling behaviour of the package
#' 
#' All options can be set with \code{options(option.name = option.value)}. You
#' can get value of option with \code{getOption("option.name").}
#' \itemize{
#' \item{expss.digits }{ Number of digits after decimal separator which will be 
#' shown for tables. This parameter us supported in the \link[expss]{datatable},
#' \link[expss]{htmlTable.simple_table} and \code{print} methods. \code{NULL} is
#' default and means 1 digit. \code{NA} means no rounding.}
#' \item{expss.disable_value_labels_support }{If not \code{FALSE} (the default) 
#' then all labelled variables will use labels as labels for factor levels when 
#' \link{factor} is called. So in any function which calls 
#' \link{factor}/\link{as.factor} will use value labels.}
#' \item{expss.prepend_var_lab }{ Should we prepend value labels with variable 
#' label ('var_lab|val_lab') during conversion to factor? This option only makes
#' sense if option 'disable_value_labels_support' is not \code{FALSE}.}
#' \item{expss.use_viewer }{ Not available yet.}
#' }
#' 
#' @name expss.options
NULL