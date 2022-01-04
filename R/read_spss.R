#' Read an SPSS Data File
#' 
#' \code{read_spss} reads data from a file stored in SPSS *.sav format. It
#' returns data.frame and never converts string variables to factors. Also it
#' prepares SPSS values/variables labels for working with
#' \code{val_lab}/\code{var_lab} functions. User-missings values are ignored.
#' \code{read_spss} is simple wrapper around \code{read.spss} function from
#' package \code{foreign}.
#' 
#' @param file Character string: the name of the file or URL to read.
#' @param reencode logical: should character strings be re-encoded to the current locale.
#'   The default is TRUE. NA means to do so in a UTF-8 locale, only. Alternatively, a
#'   character string specifying an encoding to assume for the file.
#' @param use_missings logical: should information on user-defined missing
#'   values be used to set the corresponding values to NA?
#' @param ... further parameters for \link[foreign]{read.spss}
#' @return 
#' \code{read_spss} returns data.frame. 
#' 
#' @seealso \link[foreign]{read.spss} in package \code{foreign}, \link{val_lab},
#'   \link{var_lab}
#' 
#' @export
#' @examples
#' \dontrun{
#' 
#' w = read_spss("project_123.sav") # to data.frame
#' 
#' }
read_spss=function(file, reencode = TRUE, use_missings = FALSE, ...){
    res = read_spss_to_list(file, reencode = reencode, use_missings = use_missings, ...)
    res = data.frame(res, stringsAsFactors=FALSE, check.names = FALSE)
    res
}



#' @rdname read_spss
read_spss_to_list=function(file, reencode = TRUE, use_missings = FALSE, ...){
    if(is.character(file)){
        file = gsub("^file\\:///", "", file, perl = TRUE)
    }
    spss = foreign::read.spss(enc2native(file), 
                              use.value.labels=FALSE, 
                              to.data.frame=FALSE, reencode = reencode, use.missings = use_missings, ...)
    var_names = names(spss)
    var_labs = attr(spss,'variable.labels')
    attr(spss,'label.table') = NULL
    if(anyNA(var_names)){
        var_names = make.names(var_names, unique = TRUE)
        names(spss) = var_names
        names(var_labs) = var_names
    }
    for (var_name in names(var_labs)) {
            curr_lab = var_labs[[var_name]]
            if (length(curr_lab)>0 && (curr_lab!="") && !is.na(curr_lab)) var_lab(spss[[var_name]]) = curr_lab
    }
    for (var_name in names(spss)) {
        # Trim whitespaces from start and end of character variables
        if (is.character(spss[[var_name]])) {
            spss[[var_name]] = trimws(spss[[var_name]])
            spss[[var_name]][spss[[var_name]] %in% ""] = NA
        }    
        val_labs = attr(spss[[var_name]],"value.labels")
        if (!is.null(val_labs)) {
            attr(spss[[var_name]],"value.labels") = NULL

            if(length(val_labs)>0) {
                if (is.character(val_labs)){
                    temp = utils::type.convert(val_labs, numerals = "no.loss", as.is = TRUE)
                    val_labs = setNames(temp, names(val_labs))
                }
                val_lab(spss[[var_name]]) = val_labs
            }
            
        }	
    }
    spss
}


# Override default load
# so on windows machines one can open SPSS *.sav/*.csv files by 
# dragging and dropping it on R Gui
# load=function(file, envir = parent.frame()){
#     if(is.character(file)) {
#         for (each in file){
#             dat=NULL
#             if(any(grepl("\\.sav$",file,ignore.case=TRUE,perl=TRUE))) dat=read_spss(file)
#             if(any(grepl("\\.csv$",file,ignore.case=TRUE,perl=TRUE))) dat=utils::read.table(file,header=TRUE,sep=",")
#             if (!is.null(dat)){
#                 base.name=readline("Enter name for data.frame (if empty file will be loaded into global environement):")
#                 if (base.name!=""){
#                     base.name=make.names(base.name)
#                     assign(base.name,dat,envir=envir)
#                     message("File '",basename(file),"' loaded into data.frame '",base.name,"'.\n",sep="")
#                 } else {
#                     envir=list2env(dat,envir=envir)
#                     message("File '",basename(file),"' loaded into parent frame.\n",sep="")
#                 } 
#             }  else  base::load(file,envir)
#         }
#     } else base::load(file,envir)
#     
# }