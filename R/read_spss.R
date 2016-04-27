#' Read an SPSS Data File
#' 
#' read_spss reads data from a file stored in SPSS *.sav format.
#'  
#' @param file Character string: the name of the file or URL to read.
#' @param reencode logical: should character strings be re-encoded to the current locale. The default, NA, means to do so in a UTF-8 locale, only. Alternatively a character string specifying an encoding to assume for the file.
#' 
#' @return 
#' \code{read_spss} returns data.frame. 
#' 
#' \code{read_spss_to_list} returns list of variables from SPSS files.
#' 
#' @details
#' This is simple wrapper around \code{read.spss} function from package \code{foreign}.
#' It never converts string variables to factors. Also it prepares SPSS values/variables labels
#' for working with \code{val_lab}/\code{var_lab} functions. User-missings values are ignored.
#' 
#'  @seealso
#'  \code{\link[foreign]{read.spss}} in package \code{foreign}, \code{\link{val_lab}}, \code{\link{var_lab}} 
#' 
#' @export
#' @examples
#' \dontrun{
#' 
#' w = read_spss("project_123.sav") # to data.frame
#' list_w = read_spss_to_list("project_123.sav") # to list
#' 
#' }
read_spss=function(file, reencode = NA){
    res = read_spss_to_list(file,reencode = reencode)
    res = do.call(data.frame,c(res,stringsAsFactors=FALSE))
    res
}



#' @export
#' @rdname read_spss
read_spss_to_list=function(file, reencode = NA){
    spss = foreign::read.spss(enc2native(file),use.value.labels=FALSE,to.data.frame=FALSE,reencode = reencode, use.missings = FALSE)
    var_labs = attr(spss,'variable.labels')
    attr(spss,'label.table') = NULL
    for (var_name in names(var_labs)) {
        curr_lab = var_labs[[var_name]]
        if (!is.null(curr_lab) && (curr_lab!="")) var_lab(spss[[var_name]]) = curr_lab
    }
    for (var_name in names(spss)) {
        # Trim whitespaces from start and end of character variables
        if (is.character(spss[[var_name]])) spss[[var_name]] = gsub("^\\s+|\\s+$","",spss[[var_name]],perl=TRUE)
        val_labs = attr(spss[[var_name]],"value.labels")
        if (!is.null(val_labs)) {
            attr(spss[[var_name]],"value.labels") = NULL
            val_lab(spss[[var_name]]) = sort(val_labs)
            
        }	
    }
    spss
}


# Override default load
# so on windows machines one can open SPSS *.sav/*.csv files by 
# draging and droping it on R Gui
load=function(file, envir = parent.frame()){
    if(is.character(file)) {
        for (each in file){
            dat=NULL
            if(any(grepl("\\.sav$",file,ignore.case=TRUE,perl=TRUE))) dat=read_spss(file)
            if(any(grepl("\\.csv$",file,ignore.case=TRUE,perl=TRUE))) dat=read.table(file,header=TRUE,sep=",")
            if (!is.null(dat)){
                base.name=readline("Enter name for data.frame (if empty file will be loaded into global environement):")
                if (base.name!=""){
                    base.name=make.names(base.name)
                    assign(base.name,dat,envir=envir)
                    message("File '",basename(file),"' loaded into data.frame '",base.name,"'.\n",sep="")
                } else {
                    envir=list2env(dat,envir=envir)
                    message("File '",basename(file),"' loaded into parent frame.\n",sep="")
                } 
            }  else  base::load(file,envir)
        }
    } else base::load(file,envir)
    
}