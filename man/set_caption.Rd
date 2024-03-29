% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caption.R
\name{set_caption}
\alias{set_caption}
\alias{get_caption}
\alias{is.with_caption}
\title{Add caption to the table}
\usage{
set_caption(obj, caption)

get_caption(obj)

is.with_caption(obj)
}
\arguments{
\item{obj}{object of class \code{etable} - result of \code{cross_cpct} and etc.}

\item{caption}{character caption for the table.}
}
\value{
object of class \code{with_caption}.
}
\description{
To drop caption use \code{set_caption} with \code{caption = NULL}. Captions
are supported by \link{htmlTable.etable}, \link{xl_write} and
\link{as.datatable_widget} functions.
}
\examples{

data(mtcars)
mtcars = apply_labels(mtcars,
                      vs = "Engine",
                      vs = num_lab("
                             0 V-engine 
                             1 Straight engine
                             "),
                      am = "Transmission",
                      am = num_lab("
                             0 Automatic 
                             1 Manual
                             ")
)
tbl_with_caption = cross_cases(mtcars, am, vs) \%>\% 
    set_caption("Table 1. Type of transimission.")
    
tbl_with_caption

}
