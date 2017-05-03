#' Provides variables description for dataset
#' 
#' \code{info} returns data.frame with variables description and some summary
#' statistics. Resulting data.frame mainly intended to keep in front of eyes in 
#' RStudio viewer or to be saved as csv to view in the spreadsheet software as 
#' reference about working dataset.
#' 
#' @param x vector/factor/list/data.frame.
#' @param stats Logical. Should we calculate summary for each variable?
#' @param frequencies Logical. Should we calculate frequencies for each 
#'   variable? This calculation can take significant amount of time for large 
#'   datasets.
#' @param max_levels Numeric. Maximum levels for using in frequency 
#'   calculations. Levels above this value will convert to 'Other values'.
#' @return data.frame with following columns: Name, Class, Length, NotNA, NA, 
#'   Distincts, Label, ValueLabels, Min., 1st Qu., Median, Mean, 3rd Qu., Max., 
#'   Frequency.
#' @examples
#' data(mtcars)
#' var_lab(mtcars$am) = "Transmission"
#' val_lab(mtcars$am) = c("Automatic"=0, "Manual"=1)
#' info(mtcars, max_levels = 5)                             
#' @export
info=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    UseMethod("info")
}

#' @export
info.default=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    max_levels = max(1,max_levels)
    varlab=var_lab(x)
    if (length(varlab)==0 || is.na(varlab) || varlab=="") varlab=NA
    vallab=val_lab(x)
    if (length(vallab)==0) {
        vallab=NA
    }
    else {
        vallab=paste(paste(names(vallab),vallab,sep="="),collapse=", ")
    }
    nas = is.na(x)
    not_na = sum(!nas)
    res = list("Class"=paste(class(x),collapse=","),
               "Length"=length(x),
               "NotNA"= not_na,
                "NA"= length(x) - not_na,
               "Distincts" = length(unique(x)),
               "Label"=varlab,
               "ValueLabels"=vallab)
    if (stats){
        if (is.numeric(x)){
            x_not_na = x[!nas]
            res = c(res, summary(x_not_na))
        } else {
            placeholder = rep(NA, 6) 
            names(placeholder) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max." )
            res = c(res, placeholder)
        }
    } 
    if (frequencies){
        values=sort(table(unlab(x),useNA="ifany"),na.last = TRUE,decreasing = TRUE)
        if (NA %in% names(values)) {
            values=c(values[is.na(names(values))], values[!is.na(names(values))])
        }    
        if (length(values)>max_levels) {
            other=sum(values[-(1:max_levels)])
            values=values[1:max_levels]
            values["Other values"]=other
        }
        values=paste(paste(names(values),values,sep="="),collapse=", ")
        res = c(res, Frequency =values)
    }
    curr_name = expr_to_character(substitute(x))
    stats::setNames(as.data.frame(c(curr_name,res),stringsAsFactors = FALSE, check.names = FALSE),c("Name",names(res)))
}

#' @export
info.data.frame=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    info.list(x,stats, frequencies,max_levels)
}

#' @export
info.matrix=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    info.list(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE), 
              stats = stats, 
              frequencies = frequencies,
              max_levels = max_levels)
}

#' @export
info.list=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    res = lapply(x,function(y) info(y, stats, frequencies, max_levels))
    res = do.call(rbind,res)
    rownames(res) = seq_len(nrow(res))                  
    res$Name = names(x)
    res
}

