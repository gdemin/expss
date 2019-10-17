#' Convert table to huxtable
#' 
#' @param x etable. Table to convert to a huxtable.
#' @export
as_hux <- function(x, ...) {
  
  if (!requireNamespace("huxtable", quietly = TRUE)) {
    stop("Package \"huxtable\" needed for this function to work. Please install it first.",
      call. = FALSE)
  }

}
