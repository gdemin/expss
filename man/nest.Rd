% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nest.R
\name{nest}
\alias{nest}
\alias{\%nest\%}
\title{Compute nested variable(-s) from several variables}
\usage{
nest(...)

x \%nest\% y
}
\arguments{
\item{...}{vectors/data.frames/lists}

\item{x}{vector/data.frame/list}

\item{y}{vector/data.frame/list}
}
\value{
vector/data.frame/list
}
\description{
\code{nest} mainly intended for usage with table functions such as 
\link{cro}. See examples. \code{\%nest\%} is infix version of this function. 
You can apply \code{nest} on multiple-response variables/list of variables
and data.frames.
}
\examples{
data(mtcars)

mtcars = apply_labels(mtcars,
                      cyl = "Number of cylinders",
                      vs = "Engine",
                      vs = num_lab("
                             0 V-engine 
                             1 Straight engine
                             "),
                      am = "Transmission",
                      am = num_lab("
                             0 Automatic 
                             1 Manual
                             "),
                      carb = "Number of carburetors"
)

data.table::setDTthreads(2) # for running on CRAN
cross_cases(mtcars, cyl, am \%nest\% vs)

# list of variables
cross_cases(mtcars, cyl, am \%nest\% list(vs, cyl))

# list of variables - multiple banners/multiple nesting
cross_cases(mtcars, cyl, list(total(), list(am, vs) \%nest\% cyl))

# three variables 
cross_cases(mtcars, am \%nest\% vs \%nest\% carb, cyl)

# the same with usual version
cross_cases(mtcars, cyl, nest(am, vs))

# three variables 
cross_cases(mtcars, nest(am, vs, carb), cyl)
}
\seealso{
See also \link[base]{interaction}
}
