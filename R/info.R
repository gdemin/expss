#' Provide variables info for dataset.
#' 
#' \code{into} returns data.frame with variables info.
#' 
#' @param x vector/factor/list/data.frame.
#' @param stats Logical. Should we calculate Min, LowHinge, Median, HighHinge,
#'   Max for each variable?
#' @param frequencies Logical. Should we calculate frequencies for each 
#'   variable? This calculation can take significant amount of time for large 
#'   datasets.
#' @param max_levels Numeric. Maximum levels for using in frequency 
#'   calculations. Levels above this value will convert to 'Other values'.
#' @return data.frame with following columns: Name, Class, Length, NotNA, 
#'   Distincts, Label, ValueLabels, Min, LowHinge, Median, HighHinge, Max, 
#'   Frequency.
#' @details Resulting data.frame of this function mainly intended to be saved as
#'   csv to keep before eyes in RStudio viewer or spreadsheet software as 
#'   reference about working dataset. Min, LowHinge, Median, HighHinge, Max -
#'   values which are produced by boxplot.stats function.
#' @examples
#' data(mtcars)
#' var_lab(mtcars$am) = "Transmission"
#' val_lab(mtcars$am) = c("Automatic"=0, "Manual"=1)
#' info(mtcars,max_levels = 5)                             
#' @export
info=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    UseMethod("info")
}

#' @export
info.default=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    max_levels = max(1,max_levels)
    varlab=var_lab(x)
    if (length(varlab)==0) varlab=NA
    vallab=val_lab(x)
    if (length(vallab)==0) {
        vallab=NA }
    else {
        vallab=paste(paste(names(vallab),vallab,sep="="),collapse=", ")
    }
    res = list("Class"=paste(class(x),collapse=","),
               "Length"=length(x),
               "NotNA"=sum(!is.na(x)),
               "Distincts" = length(unique(x)),
               "Label"=varlab,
               "ValueLabels"=vallab)
    if (stats){
        if (!is.character(x) && !is.factor(x)){
            bxplt = boxplot.stats(x)$stats 
            names(bxplt) = c("Min", "LowHinge", "Median", "HighHinge", "Max")
            res = c(res, bxplt)
        } else {
            bxplt = rep(NA, 5) 
            names(bxplt) = c("Min", "LowHinge", "Median", "HighHinge", "Max")
            res = c(res, bxplt)
        }
    } 
    if (frequencies){
        values=sort(table(x,useNA="ifany"),na.last = TRUE,decreasing = TRUE)
        if (NA %in% names(values)) values=c(tail(values,1),head(values,-1))
        if (length(values)>max_levels) {
            other=sum(values[-(1:max_levels)])
            values=values[1:max_levels]
            values["Other values"]=other
        }
        values=paste(paste(names(values),values,sep="="),collapse=", ")
        res = c(res, Frequency =values)
    }
    curr_name = paste(as.character(substitute(x)),collapse = "")
    as.data.frame(c(Name=curr_name,res),stringsAsFactors = FALSE)
}

#' @export
info.data.frame=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    info.list(x,stats, frequencies,max_levels)
}

info.matrix=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    info.list(as.data.frame(x,stringsAsFactors = FALSE), stats, frequencies, max_levels)
}

#' @export
info.list=function(x, stats = TRUE, frequencies = TRUE, max_levels= 10){
    res = lapply(x,function(y) info(y, stats, frequencies, max_levels))
    res = do.call(rbind,res)
    rownames(res) = seq_len(nrow(res))                  
    res$Name = names(x)
    res
}