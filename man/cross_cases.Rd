% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross.R, R/deprecated_cro.R
\name{cross_cases}
\alias{cross_cases}
\alias{cross_cpct}
\alias{cross_rpct}
\alias{cross_tpct}
\alias{cross_cpct_responses}
\alias{cro}
\alias{cro_cases}
\alias{cro_cpct}
\alias{cro_rpct}
\alias{cro_tpct}
\alias{cro_cpct_responses}
\alias{total}
\alias{calc_cro}
\alias{calc_cro_cases}
\alias{calc_cro_cpct}
\alias{calc_cro_rpct}
\alias{calc_cro_tpct}
\alias{calc_cro_cpct_responses}
\title{Cross tabulation with support of labels, weights and multiple response variables.}
\usage{
cross_cases(
  data,
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_cases",
  total_row_position = c("below", "above", "none")
)

cross_cpct(
  data,
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_cases",
  total_row_position = c("below", "above", "none")
)

cross_rpct(
  data,
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_cases",
  total_row_position = c("below", "above", "none")
)

cross_tpct(
  data,
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_cases",
  total_row_position = c("below", "above", "none")
)

cross_cpct_responses(
  data,
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_responses",
  total_row_position = c("below", "above", "none")
)

cro(
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_cases",
  total_row_position = c("below", "above", "none")
)

cro_cases(
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_cases",
  total_row_position = c("below", "above", "none")
)

cro_cpct(
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_cases",
  total_row_position = c("below", "above", "none")
)

cro_rpct(
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_cases",
  total_row_position = c("below", "above", "none")
)

cro_tpct(
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_cases",
  total_row_position = c("below", "above", "none")
)

cro_cpct_responses(
  cell_vars,
  col_vars = total(),
  row_vars = NULL,
  weight = NULL,
  subgroup = NULL,
  total_label = NULL,
  total_statistic = "u_responses",
  total_row_position = c("below", "above", "none")
)

total(x = 1, label = "#Total")
}
\arguments{
\item{data}{data.frame in which context all other arguments will be evaluated
(for \code{cross_*}).}

\item{cell_vars}{vector/data.frame/list. Variables on which percentage/cases
will be computed. Use \link{mrset}/\link{mdset} for multiple-response
variables.}

\item{col_vars}{vector/data.frame/list. Variables which breaks table by
columns. Use \link{mrset}/\link{mdset} for multiple-response variables.}

\item{row_vars}{vector/data.frame/list. Variables which breaks table by rows.
Use \link{mrset}/\link{mdset} for multiple-response variables.}

\item{weight}{numeric vector. Optional cases weights. Cases with NA's,
negative and zero weights are removed before calculations.}

\item{subgroup}{logical vector. You can specify subgroup on which table will be computed.}

\item{total_label}{By default "#Total". You can provide several names - each name for
each total statistics.}

\item{total_statistic}{By default it is "u_cases" (unweighted cases). 
Possible values are "u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
"w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct". "u_" means
unweighted statistics and "w_" means weighted statistics.}

\item{total_row_position}{Position of total row in the resulting table. Can
be one of "below", "above", "none".}

\item{x}{vector/data.frame of class 'category'/'dichotomy'.}

\item{label}{character. Label for total variable.}
}
\value{
object of class 'etable'. Basically it's a data.frame but class
  is needed for custom methods.
}
\description{
\itemize{
\item{\code{cross_cases}}{ build a contingency table of the counts.}
\item{\code{cross_cpct}, \code{cross_cpct_responses}}{ build a contingency table 
of the column percent. These functions give different results only for 
multiple response variables. For \code{cross_cpct} base of percent is number 
of valid cases. Case is considered as valid if it has at least one non-NA 
value. So for multiple response variables sum of percent may be greater than
100. For \code{cross_cpct_responses} base of percent is number of valid 
responses. Multiple response variables can have several responses for single 
case. Sum of percent of \code{cross_cpct_responses} always equals to 100\%.}
\item{\code{cross_rpct}}{ build a contingency table of the row percent. Base
for percent is number of valid cases.}
\item{\code{cross_tpct}}{ build a contingency table of the table percent. Base
for percent is number of valid cases.}
\item{\code{cross_*}}{ functions evaluate their arguments
in the context of the first argument \code{data}.}
\item{\code{cro_*}}{ functions use standard evaluation, e. g 'cro(mtcars$am, mtcars$vs)'.}
\item{\code{total}}{ auxiliary function - creates variables with 1 for valid
case of its argument \code{x} and NA in opposite case.}
}
You can combine tables with \link{add_rows} and \link{merge.etable}. For
sorting table see \link{tab_sort_asc}. 
To provide multiple-response variables as arguments use \link{mrset} for 
multiples with category encoding and \link{mdset} for multiples with 
dichotomy (dummy) encoding. To compute statistics with nested 
variables/banners use \link{nest}. For more sophisticated interface with
modern piping via \code{magrittr} see \link{tables}.
}
\examples{
\dontrun{
data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (1000 lbs)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)

cross_cases(mtcars, am, vs) 
cro(mtcars$am, mtcars$vs) # the same result

# column percent with multiple banners
cross_cpct(mtcars, cyl, list(total(), vs, am)) 

# nested banner
cross_cpct(mtcars, cyl, list(total(), vs \%nest\% am))

# stacked variables
cross_cases(mtcars, list(cyl, carb), list(total(), vs \%nest\% am))

# nested variables
cross_cpct(mtcars, am \%nest\% cyl, list(total(), vs))

# row variables
cross_cpct(mtcars, cyl, list(total(), vs), row_vars = am)

# several totals above table
cross_cpct(mtcars, cyl, 
              list(total(), vs), 
              row_vars = am,
              total_row_position = "above",
              total_label = c("number of cases", "row \%"),
              total_statistic = c("u_cases", "u_rpct")
              )

# multiple-choice variable
# brands - multiple response question
# Which brands do you use during last three months? 
set.seed(123)
brands = data.table(t(replicate(20,sample(c(1:5,NA),4,replace = FALSE)))) \%>\% 
    setNames(paste0("brand_", 1:4))
# score - evaluation of tested product
brands = brands \%>\% 
    let(
        score = sample(-1:1,.N,replace = TRUE)
    ) \%>\% 
    apply_labels(
        brand_1 = "Used brands",
        brand_1 = num_lab("
                              1 Brand A
                              2 Brand B
                              3 Brand C
                              4 Brand D
                              5 Brand E
                              "),

        score = "Evaluation of tested brand",
        score = num_lab("
                             -1 Dislike it
                             0 So-so
                             1 Like it    
                             ")
)
cross_cpct(brands, mrset(brand_1 \%to\% brand_4), list(total(), score))
# responses
cross_cpct_responses(brands, mrset(brand_1 \%to\% brand_4), list(total(), score))
}
}
\seealso{
\link{tables}, \link{fre}, \link{cro_fun}.
}
