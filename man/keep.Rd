% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keep.R
\name{keep}
\alias{keep}
\alias{except}
\title{Keep or drop elements by name/criteria in data.frame/matrix}
\usage{
keep(data, ...)

except(data, ...)
}
\arguments{
\item{data}{data.frame/matrix/list}

\item{...}{column names of type character/numeric or criteria/logical functions}
}
\value{
object of the same type as \code{data}
}
\description{
\code{keep} selects variables/elements from data.frame by their names or by 
criteria (see \link{criteria}). \code{except} drops variables/elements from 
data.frame by their names or by criteria. Names at the top-level can be
unquoted (non-standard evaluation). For standard evaluation of parameters you
can surround them by round brackets. See examples. Methods for list will apply
\code{keep}/\code{except} to each element of the list separately.
}
\examples{
data(iris)
columns(iris, Sepal.Length, Sepal.Width)  
columns(iris, -Species)

columns(iris, Species, "^.") # move 'Species' to the first position

columns(iris, -"^Petal") # remove columns which names start with 'Petal'

columns(iris, -5) # remove fifth column

data(mtcars)
columns(mtcars, mpg:qsec) # keep columns from 'mpg' to 'qsec'
columns(mtcars, mpg \%to\% qsec) # the same result

 # standard and non-standard evaluation
 many_vars = c("am", "vs", "cyl")
 columns(mtcars, many_vars)
 
# character expansion
dfs = data.frame(
     a =   rep(10, 5),
     b_1 = rep(11, 5),
     b_2 = rep(12, 5),
     b_3 = rep(12, 5),
     b_4 = rep(14, 5),
     b_5 = rep(15, 5) 
 )
 i = 1:5
 columns(dfs, b_1 \%to\% b_5) 
 columns(dfs, "b_{i}") # the same result  
}
