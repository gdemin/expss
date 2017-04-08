#' Data from product test of chocolate confectionary
#'
#' It is truncated dataset with data from product test of two samples of
#' chocolate sweets. 150 respondents tested two kinds of sweets (codenames:
#' VSX123 and SDF546). Sample was divided into two groups (cells) of 75
#' respondents in each group. In cell 1 product VSX123 was presented first and
#' then SDF546. In cell 2 sweets were presented in reversed order. Questions
#' about respondent impressions about first product are in the block A (and
#' about second tested product in the block B). At the end of the questionnaire 
#' there is a question about preferences between sweets.
#'    
#' 
#' @format A data frame with 150 rows and 18 variables:
#' \describe{
#'    \item{id}{Respondent Id.}
#'    \item{cell}{First tested product (cell number).}
#'    \item{s2a}{Age.}
#'    \item{a1_1}{What did you like in these sweets? Multiple response. First tested product.}
#'    \item{a1_2}{(continue) What did you like in these sweets? Multiple response. First tested product.}
#'    \item{a1_3}{(continue) What did you like in these sweets? Multiple response. First tested product.}
#'    \item{a1_4}{(continue) What did you like in these sweets? Multiple response. First tested product.}
#'    \item{a1_5}{(continue) What did you like in these sweets? Multiple response. First tested product.}
#'    \item{a1_6}{(continue) What did you like in these sweets? Multiple response. First tested product.}
#'    \item{a22}{Overall liking. First tested product.}
#'    \item{b1_1}{What did you like in these sweets? Multiple response. Second tested product.}
#'    \item{b1_2}{(continue) What did you like in these sweets? Multiple response. Second tested product.}
#'    \item{b1_3}{(continue) What did you like in these sweets? Multiple response. Second tested product.}
#'    \item{b1_4}{(continue) What did you like in these sweets? Multiple response. Second tested product.}
#'    \item{b1_5}{(continue) What did you like in these sweets? Multiple response. Second tested product.}
#'    \item{b1_6}{(continue) What did you like in these sweets? Multiple response. Second tested product.}
#'    \item{b22}{Overall liking. Second tested product.}
#'    \item{c1}{Preferences.}
#' }
#' @docType data
"product_test"



