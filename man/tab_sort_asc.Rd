% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_sort.R
\name{tab_sort_asc}
\alias{tab_sort_asc}
\alias{tab_sort_desc}
\title{Partially (inside blocks) sort tables/data.frames}
\usage{
tab_sort_asc(x, ..., excluded_rows = "#", na.last = FALSE)

tab_sort_desc(x, ..., excluded_rows = "#", na.last = TRUE)
}
\arguments{
\item{x}{data.frame}

\item{...}{character/numeric or criteria/logical functions (see 
\link{criteria}). Column names/numbers for data.frame/matrix by which 
object will be sorted. Names at the top-level can be unquoted (non-standard
evaluation). For standard evaluation of parameters you can surround them by
round brackets. See examples. If this argument is missing then table will 
be sorted by second column. Usually second column is the first column with
numbers in the table (there are row labels in the first column).}

\item{excluded_rows}{character/logical/numeric rows which won't be sorted. 
Rows of the table will be sorted between excluded rows. If it is characters
then they will be considered as pattern/vector of patterns. Patterns will
be matched with Perl-style regular expression with values in the first
column of \code{x} (see \link[base]{grep}, \code{perl = TRUE} argument).
Rows which have such patterns will be excluded. By default, pattern is "#"
because "#" marks totals in the result of \link{cro}.}

\item{na.last}{for controlling the treatment of NAs. If TRUE, missing values
in the data are put last; if FALSE, they are put first; if NA, they are
removed.}
}
\value{
sorted table('etable')/data.frame
}
\description{
\code{tab_sort_asc}/\code{tab_sort_desc} sort tables (usually result of 
\link{cro}/\link{tables}) in ascending/descending order between specified
rows (by default, it is rows which contain '#' in the first column).
}
\examples{
\dontrun{
data(mtcars)

# apply labels
mtcars = apply_labels(mtcars,
    cyl = "Number of cylinders",
    vs = "Engine",
    vs = c("V-engine" = 0,
                    "Straight engine" = 1),
    am = "Transmission",
    am = c(automatic = 0,
                    manual=1),
    gear = "Number of forward gears",
    carb = "Number of carburetors"
)

# without sorting
mtcars \%>\% cross_cpct(list(cyl, gear, carb), list("#total", vs, am))

# with sorting
mtcars \%>\% 
    cross_cpct(list(cyl, gear, carb), list("#total", vs, am)) \%>\% 
    tab_sort_desc
    
# sort by parameter
sorting_column = "Engine|V-engine"

mtcars \%>\% 
    cross_cpct(list(cyl, gear, carb), list("#total", vs, am)) \%>\% 
    tab_sort_desc((sorting_column))
}    
}
