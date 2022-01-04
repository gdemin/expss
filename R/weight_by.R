#' Create dataset according to its frequency weights
#' 
#' This is a "brute force" weighting procedure. Each row of the dataset is
#' replicated "case weight" times. If 'weight' is not integer it will
#' be rounded to the nearest integer. So cases with weight less than 0.5 will be
#' removed from the dataset. Such weighting is used in the several statistical
#' procedures in the SPSS Statistic, e. g. for the Spearman correlation
#' coefficient or GLM.
#' 
#' @param data data.frame, data.table or matrix. Dataset which will be weighted. 
#' @param weight unquoted column name of weights in 'data' or vector of weights.
#'   If it is NULL 'data' will be returned unchanged.
#'
#' @return 'data' with each row replicated according to case weight.
#' @export
#'
#' @examples
#' data(state) # US states
#' # convert matrix to data.table
#' states = data.table(state.x77, keep.rownames = "State")
#' 
#' # create weighted dataset
#' states_weighted = states %>% 
#'     let(
#'         # calculate 'weight' variable. 
#'         weight = Population/100
#'     ) %>% 
#'     weight_by(weight)
#' 
#' # Each row in the weighted dataset is represented proportionally to the population of the state
#' nrow(states) # unweigthed number of cases
#' nrow(states_weighted) # number of cases in the weighted dataset
#' str(states_weighted)
weight_by = function(data, weight = NULL){
    if(is.data.frame(data)) weight = eval(substitute(weight), envir = data, enclos = parent.frame())
    if(is.null(weight)) return(data)
    if(length(weight) == 1L){
        weight = rep(weight, NROW(data))
    }
    (length(weight) == NROW(data)) || stop(
        "length of 'weight' must equal to the length of 'data' but NROW(data) == ", NROW(data),
        " and length(weight) == ", length(weight))
    weight = set_negative_and_na_to_zero(weight)
    weight = trunc(weight+0.5)
    need_rows = rep(seq_len(nrow(data)), weight)
    data[need_rows, , drop = FALSE]
}