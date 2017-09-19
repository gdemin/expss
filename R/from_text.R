#' Make data.frame from text
#' 
#' Convert delimited text lines to data.frame. Blank lines are always skipped,
#' trailing whitespaces are trimmed. You can use comments with '#' inside your text.
#' For details see \link[utils]{read.table}.
#'  
#' @param text character/vector of characters
#' @param header a logical value indicating whether the \code{text} contains the
#'   names of the variables as its first line.
#' @param sep the field separator character. Values on each line of the file are
#'   separated by this character. If sep = "" (the default for read.table) the
#'   separator is 'white space', that is one or more spaces, tabs, newlines or
#'   carriage returns.
#' @param quote the set of quoting characters. To disable quoting altogether, use quote = "".
#' @param dec the character used in the file for decimal points.
#' @param encoding encoding to be assumed for input strings. It is used to mark
#'   character strings as known to be in Latin-1 or UTF-8 (see \link[utils]{read.table}).
#' @param ... further parameters which will be passed to \link[utils]{read.table}.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' from_text("
#' # simple data.frame 
#'      a b   c
#'      1 2.5 a
#'      4 5.5 b
#'      7 8.5 c
#' ")
#' 
from_text = function(text, 
                     header = TRUE, 
                     sep = "", 
                     quote = "",
                     dec = ".", 
                     encoding = "unknown", 
                     ...
){
    split="\n"
    if (length(text)>1) text = paste(text, collapse=split)
    text = unlist(strsplit(text, split=split))
    text = text[!is.na(text)]
    text = trimws(text)
    text = text[text!=""]
    text = paste(text, collapse=split)
    read.table(text = text,
               header = header,
               sep = sep,
               quote = quote,
               dec = dec,
               encoding = encoding,
               stringsAsFactors = FALSE,
               strip.white = TRUE,
               blank.lines.skip = TRUE,
               fill = TRUE,
               ...
               )
}

#' @export
#' @rdname from_text 
from_text_csv = function(text, 
                         header = TRUE, 
                         sep = ",", 
                         quote = "",
                         dec = ".", 
                         encoding = "unknown", 
                         ...
){
    from_text(
        text = text,
        header = header,
        sep = sep,
        quote = quote,
        dec = dec,
        encoding = encoding,
        ...
    )
}

#' @export
#' @rdname from_text 
from_text_csv2 = function(text, 
                         header = TRUE, 
                         sep = ";", 
                         quote = "",
                         dec = ",", 
                         encoding = "unknown", 
                         ...
){
    from_text(
        text = text,
        header = header,
        sep = sep,
        quote = quote,
        dec = dec,
        encoding = encoding,
        ...
    )
}

#' @export
#' @rdname from_text 
from_text_tab = function(text, 
                         header = TRUE, 
                         sep = "\t", 
                         quote = "",
                         dec = ".", 
                         encoding = "unknown", 
                         ...
){
    from_text(
        text = text,
        header = header,
        sep = sep,
        quote = quote,
        dec = dec,
        encoding = encoding,
        ...
    )
}

#' @export
#' @rdname from_text 
from_text_tab2 = function(text, 
                          header = TRUE, 
                          sep = "\t", 
                          quote = "",
                          dec = ",", 
                          encoding = "unknown", 
                          ...
){
    from_text(
        text = text,
        header = header,
        sep = sep,
        quote = quote,
        dec = dec,
        encoding = encoding,
        ...
    )
}