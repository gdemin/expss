#' Write data with labels to file in R code or in SPSS syntax.
#' 
#' \code{write_labelled_*} functions write data in the CSV format and file with 
#' R code/SPSS syntax for labelling data.  SPSS syntax also contains code for 
#' reading data in SPSS. \code{write_labelled_*} doesn't save rownames of 
#' data.frame. \code{write_labels_*} functions write R code/SPSS syntax for 
#' labelling data. It allows to extract labels from *.sav files that come 
#' without accompanying syntax. \code{read_labelled_csv} reads data file in CSV 
#' format and apply labels from accompanying file with R code. \code{*_csv2} 
#' write/read data with semicolon separator and comma as decimal delimiter. 
#' \code{*_tab/*_tab2} write/read data with tab separator and "."/"," as
#' decimal delimiter.
#' 
#' @param x data.frame to be written/data.frame whose labels to be written
#' @param filename the name of the file which the data are to be read from/write to.
#' @param fileEncoding character string: if non-empty declares the encoding to 
#'   be used on a file (not a connection) so the character data can be 
#'   re-encoded as they are written. Used for writing dictionary. See
#'   \link[base]{file}.
#' @param encoding default is "unknown". Other possible options are "UTF-8" and 
#'   "Latin-1". Note: it is not used to re-encode the input, rather enables 
#'   handling of encoded strings in their native encoding. Used for writing data
#'   file. See \link[data.table]{fread}.
#' @param sep the field separator string. Values within each row of x are
#'   separated by this string.
#' @param dec the string to use for decimal points in numeric or complex
#'   columns: must be a single character.
#' @param qmethod A character string specifying how to deal with embedded double
#'   quote characters when quoting strings. "escape" - the quote character (as
#'   well as the backslash character) is escaped in C style by a
#'   backslash, or "double" (default), in which case the double quote is doubled with
#'   another one.
#' @param remove_new_lines A logical indicating should we replace new lines with spaces in
#'   the character variables. TRUE by default.
#' @param undouble_quotes A logical indicating should we undouble quotes which
#'   were escaped by doubling (see \code{qmethod}). TRUE by default. Argument
#'   will be removed when data.table issue #1109 will be fixed.
#' @param ... additional arguments for
#'   \link[data.table]{fwrite}/\link[data.table]{fread}
#'
#' @return Functions for writing invisibly return NULL. Functions for reading
#'   return labelled data.frame.
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mtcars = modify(mtcars,{
#'                 var_lab(mpg) = "Miles/(US) gallon"
#'                 var_lab(cyl) = "Number of cylinders"
#'                 var_lab(disp) = "Displacement (cu.in.)"
#'                 var_lab(hp) = "Gross horsepower"
#'                 var_lab(drat) = "Rear axle ratio"
#'                 var_lab(wt) = "Weight (lb/1000)"
#'                 var_lab(qsec) = "1/4 mile time"
#'                 var_lab(vs) = "Engine"
#'                 val_lab(vs) = c("V-engine" = 0, 
#'                                 "Straight engine" = 1) 
#'                 var_lab(am) = "Transmission"
#'                 val_lab(am) = c(automatic = 0, 
#'                                 manual=1)
#'                 var_lab(gear) = "Number of forward gears"
#'                 var_lab(carb) = "Number of carburetors"
#' })
#' 
#' # to R code
#' # rownames are not preserved
#' write_labelled_csv(mtcars, "mtcars.csv")
#' new_mtcars = read_labelled_csv("mtcars.csv")
#' 
#' # to SPSS syntax
#' write_labelled_spss(mtcars, "mtcars.csv")
#' 
#' }
write_labels = function(x, filename, fileEncoding = ""){
    var_labs = lapply(x,var_lab)
    val_labs = lapply(x,val_lab)
    var_num = ncol(x)
    x_names = colnames(x)
    code = ""
    identical_vallabs = vector()
    write_labels = TRUE
    curr_val_lab = NULL
    
    for (each in seq_len(var_num)){
        
        next_val_lab = val_labs[[each]]
        if(identical(curr_val_lab,next_val_lab)){
            identical_vallabs = c(identical_vallabs,x_names[each])
        } else {
            if(!is.null(curr_val_lab) && (curr_val_lab!="")){
                code = paste0(code,
                              make_make_labs(identical_vallabs,curr_val_lab),           
                              "\n\n")
            }
            identical_vallabs = x_names[each]
            curr_val_lab = next_val_lab
        }
        
        ##### 
        curr_var_lab = var_labs[[each]]
        if (!is.null(curr_var_lab) && (curr_var_lab!="")){
            curr_var_lab = gsub("\\", "\\\\", curr_var_lab, fixed = TRUE) # escape backslash
            code = paste0(code,
                          'var_lab(w$',x_names[each],') = "', gsub('"','\\\\"',curr_var_lab),'\"',           
                          "\n")
            
        }
        
    }
    
    if (length(identical_vallabs)>0){
        code = paste0(code,
                      make_make_labs(identical_vallabs,curr_val_lab),           
                      "\n")       
        
    }
    conn = file(filename, encoding = fileEncoding)
    on.exit(close(conn))
    writeLines(text= code,con = conn)
    invisible(NULL)
    
}

write_dictionary_csv = function(x, filename, fileEncoding = ""){
    
}

read_dictionary_csv = function(filename, fileEncoding = ""){
    
}

#' @export
#' @rdname write_labels
read_labelled_csv = function(filename, 
                             fileEncoding = "", 
                             encoding = "unknown", 
                             sep = ",", 
                             dec = ".",
                             undouble_quotes = TRUE,
                             ...){
    # w = utils::read.table(file = filename,
    #                header = TRUE,
    #                sep = sep,
    #                dec = dec,
    #                stringsAsFactors = FALSE,
    #                na.strings = "",
    #                fileEncoding = fileEncoding,
    #                check.names = FALSE,
    #                ...
    #                )
    w = data.table::fread(filename, 
              sep = sep,  
              header= TRUE, 
              na.strings="", 
              stringsAsFactors=FALSE, 
              integer64 = "character",         
              dec = dec, 
              encoding = encoding, 
              data.table = FALSE)
    if(undouble_quotes){
        all_columns = seq_along(w)
        for(i in all_columns){
            if(is.character(w[[i]])){
                w[[i]] = gsub('\"\"','\"',w[[i]], fixed = TRUE)    
            }
        }
    }
    dic_file = paste0(filename,".dic.R")
    if (file.exists(dic_file)){
        source(dic_file, local = TRUE, encoding = fileEncoding, verbose = FALSE)
    } else {
        warning(".dic.R file doesn't exists. Labels will not be applied to data.")
    }

    w
  
}

# @export
# @rdname write_labels
# fread_df = data.table::fread
# 
# formals(fread_df)$data.table = FALSE
# formals(fread_df)$integer64 = "character"


#' @export
#' @rdname write_labels
read_labelled_csv2 = function(filename, 
                              fileEncoding = "", 
                              encoding = "unknown", 
                              sep = ";", 
                              dec = ",",
                              undouble_quotes = TRUE,
                              ...){
    read_labelled_csv(filename = filename,
                      fileEncoding = fileEncoding,
                      encoding = encoding, 
                      sep = sep, 
                      dec = dec, 
                      undouble_quotes = undouble_quotes,
                      ...)
}

#' @export
#' @rdname write_labels
read_labelled_tab = function(filename, 
                             fileEncoding = "", 
                             encoding = "unknown", 
                             sep = "\t", 
                             dec = ".", 
                             undouble_quotes = TRUE,
                             ...){
    read_labelled_csv(filename = filename,
                      fileEncoding = fileEncoding,
                      encoding = encoding, 
                      sep = sep, 
                      dec = dec, 
                      undouble_quotes = undouble_quotes,
                      ...)
}

#' @export
#' @rdname write_labels
read_labelled_tab2 = function(filename, 
                              fileEncoding = "", 
                              encoding = "unknown", 
                              sep = "\t", 
                              dec = ",", 
                              undouble_quotes = TRUE,
                              ...){
    read_labelled_csv(filename = filename,
                      fileEncoding = fileEncoding,
                      encoding = encoding, 
                      sep = sep, 
                      dec = dec, 
                      undouble_quotes = undouble_quotes,
                      ...)
}

#' @export
#' @rdname write_labels
write_labelled_csv = function(x, 
                              filename, 
                              fileEncoding = "", 
                              sep = ",", 
                              dec = ".", 
                              qmethod = c("double", "escape"), 
                              remove_new_lines = TRUE,
                              ...){
    if (!is.data.frame(x)) x = as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    if(remove_new_lines){
        for(each in seq_along(x)){
            if (is.factor(x[[each]])){
                levels(x[[each]]) = gsub("[\\n\\r]+"," ", levels(x[[each]]), perl = TRUE)
            }
            if (is.character(x[[each]])){
                x[[each]] = gsub("[\\n\\r]+"," ", x[[each]], perl = TRUE)
                # x[[each]] = gsub('"',"'", x[[each]], fixed = TRUE)
            }
        }
    }
    qmethod = match.arg(qmethod)
    data.table::fwrite(x = x, 
           file = filename,
           quote = TRUE,
           col.names = TRUE,
           row.names = FALSE,
           sep = sep,
           dec = dec,
           na = "",
           qmethod = qmethod,
           ...
    )
    dic_file = paste0(filename,".dic.R")
    write_labels(x = x, filename = dic_file, fileEncoding = fileEncoding)
    invisible(NULL)
    
}

#' @export
#' @rdname write_labels
write_labelled_csv2 = function(x, filename, fileEncoding = "", sep = ";", dec = ",", 
                               qmethod = c("double", "escape"), 
                               remove_new_lines = TRUE,
                               ...){
    write_labelled_csv(x = x, 
                       filename = filename,
                       fileEncoding = fileEncoding,
                       sep = sep, 
                       dec = dec,
                       qmethod = qmethod,
                       remove_new_lines = remove_new_lines,
                       ...                       
    )
}

#' @export
#' @rdname write_labels
write_labelled_tab = function(x, filename, fileEncoding = "", sep = "\t", dec = ".", 
                              qmethod = c("double", "escape"), 
                              remove_new_lines = TRUE,
                              ...){
    write_labelled_csv(x = x, 
                       filename = filename,
                       fileEncoding = fileEncoding,
                       sep = sep, 
                       dec = dec,
                       qmethod = qmethod,
                       remove_new_lines = remove_new_lines,
                       ...                       
    )
}

#' @export
#' @rdname write_labels
write_labelled_tab2 = function(x, filename, fileEncoding = "", sep = "\t", dec = ",", 
                               qmethod = c("double", "escape"), 
                               remove_new_lines = TRUE,
                               ...){
    write_labelled_csv(x = x, 
                       filename = filename,
                       fileEncoding = fileEncoding,
                       sep = sep, 
                       dec = dec,
                       qmethod = qmethod,
                       remove_new_lines = remove_new_lines,
                       ...                       
    )
}

#' @export
#' @rdname write_labels
write_labelled_spss = function(x, 
                               filename, 
                               fileEncoding = "",
                               remove_new_lines = TRUE,
                               ...){
    if (!is.data.frame(x)) x = as.data.frame(x, stringsAsFactors = FALSE, check.names = TRUE)
    cln = colnames(x)
    # replace starting point in variables names .name -> name
    cln = gsub("^\\.", "", cln)
    cln = make.unique(cln, sep = "_")
    colnames(x) = cln
    all_columns = seq_along(x)
    for(each in all_columns){
        if (is.factor(x[[each]])){
            x[[each]] = as.character(x[[each]])
        }
        if (remove_new_lines && is.character(x[[each]])){
            x[[each]] = gsub("[\\n\\r]+"," ", x[[each]], perl = TRUE)
            # x[[each]] = gsub('"',"'", x[[each]], fixed = TRUE)
        }
        if(is.logical(x[[each]])){
            x[[each]] = 1*x[[each]]
        }
    }
    data.table::fwrite(x = x, 
           file = filename,
           col.names = TRUE,
           row.names = FALSE,
           sep = ",",
           na = "",
           qmethod = "double",
           quote = TRUE,
           ...
    )
    #dic_file = paste0(filename,".dic.R")
    #write_labels(x = x, filename = dic_file, fileEncoding = fileEncoding)
    syntax = "GET DATA  /TYPE = TXT
         /FILE = '%s'
         /DELCASE = LINE
         /DELIMITERS = \",\"
         /QUALIFIER = '\"'
         /ARRANGEMENT = DELIMITED
         /FIRSTCASE = 2
         /VARIABLES ="
    syntax = sprintf(syntax, normalizePath(filename, mustWork = FALSE))
    vars = lapply(colnames(x), function(col){
        
        if(is.numeric(x[[col]])){
            if (all(is.na(x[[col]]))){
                paste0(col, " F1.0")    
            } else {
                resid = max(abs(trunc(x[[col]]) - x[[col]]), na.rm = TRUE)
                if (resid ==0 ){
                    paste0(col, " F8.0")
                } else {
                    paste0(col, " F8.3")
                }
            }
            
        }       
        else {
            if (all(is.na(x[[col]]))){
                paste0(col, " A1")    
            } else {
                paste0(col, " A", max(nchar(x[[col]]), na.rm = TRUE)) 
            }    
        }
        
        
        
    })
    vars = paste(unlist(vars), collapse = "\n")
    syntax = paste0(syntax, "\n", vars,
                    " .\nCACHE.\nEXECUTE.\n")
    
    file.create(paste0(filename, ".sps"))
    conn = file(paste0(filename, ".sps"), encoding = fileEncoding, open = "at")
    on.exit(close(conn))
    writeLines(text = syntax, con = conn)
    writeLines(text = "\n\n\n\n", con = conn)
    write_labels_spss(x, filename = conn)
    invisible(NULL) 
}

#' @export
#' @rdname write_labels
write_labels_spss = function(x, filename){
    var_labs = lapply(x,var_lab)
    val_labs = lapply(x,val_lab)
    var_num = ncol(x)
    x_names = colnames(x)
    code = ""
    identical_vallabs = vector()
    write_labels = TRUE
    curr_val_lab = NULL
    
    for (each in seq_len(var_num)){
        
        next_val_lab = val_labs[[each]]
        if(identical(curr_val_lab,next_val_lab)){
            identical_vallabs = c(identical_vallabs,x_names[each])
        } else {
            if(!is.null(curr_val_lab) && (curr_val_lab!="")){
                code = paste0(code,
                              make_make_labs_spss(identical_vallabs,curr_val_lab),           
                              "\n\n")
            }
            identical_vallabs = x_names[each]
            curr_val_lab = next_val_lab
        }
        
        ##### 
        curr_var_lab = var_labs[[each]]
        if (!is.null(curr_var_lab) && (curr_var_lab!="")){
            code = paste0(code,
                          'VAR LAB ',x_names[each],' "', gsub('"', "'", curr_var_lab),'".\n')
            
        }
        
    }
    
    if (length(identical_vallabs)>0){
        code = paste0(code,
                      make_make_labs_spss(identical_vallabs,curr_val_lab),           
                      "\n")       
        
    }

    writeLines(text= code, con = filename)
    invisible(NULL)
    
}


make_make_labs = function(vars, named_vec){
    if (is.null(named_vec) || is.null(names(named_vec)) || (length(vars)==0)) return(NULL)
    if (length(vars)>1) {
        vars = paste0("val_lab(w[,c(",paste(paste0('"',vars,'"'),collapse = ", "),")])")
    } else {
        vars = paste0("val_lab(w$",vars,")")
    }
    labs = gsub("\\", "\\\\", names(named_vec), fixed = TRUE) # escape backslash
    labs = gsub('"','\\\\"', labs)
    vallab = paste0("    ",named_vec,' ',labs,'')#[labs!=""]
    pattern = "^(-*)([\\d\\.]+)([\\.\\s\\t]+)(.+?)$"
    if(all(grepl(pattern, gsub("^([\\s\\t]+)|([\\s\\t]+)$","",vallab,perl = TRUE), perl = TRUE))){
        vallab = paste(vallab, collapse = "\n")
        sprintf('%s = make_labels("\n%s\n")',vars,vallab) 
    } else {
        store = ""
        con = textConnection("store", "w", local = TRUE)
        dput(named_vec, con)
        close(con)
        sprintf('%s = %s',vars,paste(store, collapse = "\n"))
    }   
    
    
}


make_make_labs_spss = function(vars,named_vec){
    if (is.null(named_vec) || is.null(names(named_vec)) || (length(vars)==0)) return(NULL)
    sorted = order(as.numeric(named_vec))
    named_vec = named_vec[sorted]
    
    if (length(vars)>1) {
        vars = paste0("VAL LAB ",paste(vars,collapse = " "))
    } else {
        vars = paste0("VAL LAB ",vars)
    }
    labs = paste0('"', gsub('"',"'",names(named_vec)), '"')
    vallab = paste0("    ",named_vec,' ',labs,'')[labs!=""]
    vallab = paste(vallab, collapse = "\n")
    sprintf('%s\n%s.\n',vars,vallab)
    
}

# apply_labels_from_file = function(x, filename, fileEncoding = ""){
#     
#     if (file.exists(filename)){
#         source(filename, local = TRUE, encoding = fileEncoding, verbose = FALSE)
#     } else {
#         warning(paste(filename,"file doesn't exists. Labels will not be applied to data."))
#     }
#     x
#     
# }