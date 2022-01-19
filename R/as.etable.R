#' Convert data.frame/matrix to object of class 'etable'
#' 
#' If \code{x} is \code{data.frame} then \code{as.etable} just adds
#' \code{etable} to \code{class} attribute of \code{x}. If \code{x} is matrix
#' then it will be converted to data.frame.
#'
#' @param x data.frame/matrix
#' @param rownames_as_row_labels logical. If it is TRUE than rownames of 
#'   \code{x} will be added to result as first column with name 
#'   \code{row_labels}. By default row names will be added if they are not NULL
#'   and are not sequential numerics.
#' @param ... unused
#' @return object of class \code{etable}
#' @export
#'
#' @examples
#' data(mtcars)
#' etable_mtcars = as.etable(mtcars)
#' is.etable(etable_mtcars) #TRUE
#' 
#' etable_mtcars #another 'print' method is used
#' 
#' cor(mtcars) %>% as.etable()
as.etable = function(x, rownames_as_row_labels = NULL, ...){
    UseMethod("as.etable")
}

#' @export
as.etable.etable = function(x, rownames_as_row_labels = NULL, ...){
    x
}

#' @export
as.etable.default = function(x, rownames_as_row_labels = NULL, ...){
    as.etable(as.sheet(x), rownames_as_row_labels = rownames_as_row_labels)
}

#' @export
as.etable.data.table = function(x, rownames_as_row_labels = NULL, ...){
    as.etable(as.sheet(x), rownames_as_row_labels = rownames_as_row_labels)
}

#' @export
as.etable.data.frame = function(x, rownames_as_row_labels = NULL, ...){
    rownames_as_row_labels = if_null(rownames_as_row_labels, has_rownames(x))
    if(rownames_as_row_labels){
        x = sheet(row_labels = rownames(x), x)
    }
    class(x) = union("etable", class(x))
    x
}

#' @export
as.etable.matrix = function(x, rownames_as_row_labels = NULL, ...){
    res = as.sheet(x)
    if(is.null(colnames(x))){
        colnames(res) = rep("", NCOL(res))
    }
    as.etable(res, rownames_as_row_labels = rownames_as_row_labels)
}

# return FALSE if rownames are trivial
has_rownames = function(x){
   curr  = rownames(x)
   !(is.null(curr) |
       identical(as.character(curr), as.character(seq_len(NROW(x))))
   )
}

#' @export
as.etable.table = function(x, rownames_as_row_labels = NULL, ...){
    res = do.call(expand.grid, c(dimnames(provideDimnames(x, unique = FALSE)), 
                                 KEEP.OUT.ATTRS = FALSE))
    res = prepend_names(res)
    weight = c(x)
    if(NCOL(res)<2){
        cro(cell_vars = res[[1]], 
            col_vars = list("|"),
            weight = weight,
            total_row_position = "none")
    } else {
        if(NCOL(res)<3){
            cro(cell_vars = res[[1]], 
                col_vars = res[[2]], 
                weight = weight,
                total_row_position = "none")
        } else {
            rowvars = res
            rowvars[, 1:2] = NULL
            rowvars = rowvars[, rev(seq_len(NCOL(rowvars))), drop = FALSE]
            if(NCOL(rowvars)>1){
                rowvars = do.call(nest, rowvars)
            }
            cro(cell_vars = res[[1]], 
                col_vars = res[[2]], 
                row_vars = rowvars,
                weight = weight,
                total_row_position = "none"
                )
        }
    }
}

#' @export
as.etable.lm = function(x, rownames_as_row_labels = NULL, ...){
    as.etable(summary(x), rownames_as_row_labels = rownames_as_row_labels, ...)   
}

#' @export
as.etable.summary.lm = function(x, rownames_as_row_labels = NULL, ...){
    curr_coef = coefficients(x)  
    model = expr_to_character(as.expression(as.formula(x$terms))[[1]])
    if(isTRUE(x$weight_is_frequency) && !is.null(x$weights)){
        weighted_obs = sum(x$weights, na.rm = TRUE)
    } else {
        weighted_obs = NA
    }
    summary_res = data.table(
        row_labels = "Summary",
        "Parameter" = c("R2", "Adjusted R2", "Observations", "Weighted obs.", "Degrees of freedom", "F-statistic"),
        "Estimate" =  c(x$r.squared, x$adj.r.squared, length(x$residuals), weighted_obs, x$df[2], x$fstatistic[1]),
        "Std. Error" = NA, "t value" = NA,
        "Pr(>|t|)" = c(NA, NA, NA, NA, NA, 
                       stats::pf(x$fstatistic[1L], 
                                      x$fstatistic[2L], 
                          x$fstatistic[3L], 
                          lower.tail = FALSE)
                       )
  
    )
    res = cbind(data.table(row_labels = "Coefficients", "Parameter" = rownames(curr_coef)), curr_coef)
    res = rbindlist(list(summary_res, res), fill = TRUE, use.names = TRUE)
    symb = symnum(res[["Pr(>|t|)"]], corr = FALSE, na = FALSE, 
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                      symbols = c("***", "**", "*", ".", " "))
    res$symb = symb
    legend = attr(symb, "legend", exact = TRUE)
    colnames(res) = c("row_labels", "Parameter", "Estimate", "Std. Error", "t value", "Sig.", "")
    res = set_caption(as.etable(res), model)
    res = set_footer(res, paste0("Signif. codes: ", legend))
    class(res) = union(c("etable_summary_lm"), class(res))
    res

}

#' @export
#' @rdname as.etable
is.etable = function(x){
    inherits(x, "etable")
}