#' Codeframe for open-ended questions of product test
#'
#' @format List with three character elements:
#' \describe{
#'    \item{likes}{Codeframe for questions a1_1-a1_6, b1_1-b1_6 and c2_1-c2_6 (without codes 1 and 2)}
#'    \item{dislikes}{Codeframe for questions a2_1-a2_6, b2_1-b2_6}
#'    \item{dislikes_in_appearance}{Codeframe for questions a4_1-a4_6, b4_1-b4_6}
#' }
#' @docType data
#' @examples
#' data(ProductTest)
#' data(codeframe)
#' 
#' val_lab(ProductTest$a1_1) = make_labels(codeframe$likes)
#' 
#' val_lab(ProductTest$c2_1) = make_labels(codeframe$likes)[-(1:2)]
"codeframe"



