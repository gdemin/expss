#' Write labelled data to file or export file to SPSS syntax.
#' 
#' \itemize{
#' \item{\code{write_labelled_csv} and \code{read_labelled_csv}}{ writes csv
#' file with labels. By default labels are stored in the commented lines at the
#' beginning of the file before the data part. \code{*_csv2} write and read data
#' with a semicolon separator and comma as decimal delimiter. \code{*_tab/*_tab2}
#' write and read data with 'tab' separator and "."/"," as decimal delimiter. }
#' \item{\code{write_labelled_xlsx} and \code{read_labelled_xlsx}}{ write and
#' read labelled 'xlsx' format. It is a simple Excel file with data and labels on
#' separate sheets. It can help you with labelled data exchange in the
#' corporate environment.}
#' \item{\code{write_labelled_fst} and \code{read_labelled_fst}}{ write and read
#' labelled data in the 'fst' format. See \href{https://www.fstpackage.org/}{Fst Package}.
#' Data and labels are stored in the separate files. With 'fst' format you can
#' read and write a huge amount of data very quickly.}
#' \item{write_labelled_spss}{ write 'csv' file with SPSS syntax for reading it.
#' You can use it for the data exchange with SPSS.}
#' \item{\code{create_dictionary} and \code{apply_dictionary}}{ make data.frame
#' with dictionary, e. g. variable and value labels for each variable. See
#' format description in the 'Details' section.}
#' \item{\code{write_labels} and \code{write_labels_spss}}{ Write R code and
#' SPSS syntax for labelling data. It allows to extract labels from *.sav files
#' that come without accompanying syntax. }
#' \item{\code{old_write_labelled_csv} and \code{old_read_labelled_csv}}{ Read
#' and write labelled 'csv' in format of the 'expss' version before 0.9.0.
#' }
#' }
#' 
#' @details Dictionary is a data.frame with the following columns:
#' \itemize{
#' \item{variable}{ variable name in the data set. It can be omitted
#' (\code{NA}). In this case name from the previous row will be taken.}
#' \item{value}{ code for label in the column \code{'label'}.}
#' \item{label}{ in most cases it is value label but its meaning can be changed
#' by the column \code{'meta'}.}
#' \item{meta}{ if it is NA then we have value label in the \code{'label'}
#' column. If it is \code{'varlab'}, then there is a variable label in the
#' \code{'label'} column and column \code{'value'} is ignored. If it is
#' \code{'reference'}, then there is a variable name in the \code{'label'}
#' column and we use value labels from this variable, column \code{'value'} is
#' ignored.}
#' }
#' 
#' @param x data.frame to be written/data.frame whose labels to be written
#' @param filename the name of the file which the data are to be read from/write to.
#' @param remove_new_lines A logical indicating should we replace new lines with spaces in
#'   the character variables. TRUE by default.
#' @param undouble_quotes A logical indicating should we undouble quotes which
#'   were escaped by doubling. TRUE by default. Argument
#'   will be removed when data.table issue #1109 will be fixed.
#' @param ... additional arguments for
#'   \link[data.table]{fwrite}/\link[data.table]{fread}, e. g. column separator,
#'   decimal separator, encoding and etc.
#' @param single_file logical. TRUE by default. Should we write labels into the
#'   same file as data? If it is FALSE dictionary will be written in the
#'   separate file.
#' @param use_references logical. When TRUE (default) then if the variable has
#'   the same value labels as the previous variable, we use reference to this
#'   variable. It makes dictionary significantly more compact for datasets
#'   with many variables with the same value labels.
#' @param remove_repeated logical. FALSE by default. If TRUE then we remove
#'   repeated variable names. It makes a dictionary to look nicer for humans but
#'   less convenient for usage.
#' @param data_sheet character "data" by default. Where data will be placed in the '*.xlsx' file.
#' @param dict_sheet character "dictionary" by default. Where dictionary will be placed in the '*.xlsx' file.
#' @param dict data.frame with labels - a result of \code{create_dictionary}.  
#' @param fileEncoding character string: if non-empty declares the encoding to 
#'   be used on a file (not a connection) so the character data can be 
#'   re-encoded as they are written. Used for writing dictionary. See
#'   \link[base:connections]{file}.
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
#' write_labelled_csv(mtcars, "mtcars.csv")
#' new_mtcars = read_labelled_csv("mtcars.csv")
#' str(new_mtcars)
#' 
#' # identically, for xlsx
#' write_labelled_xlsx(mtcars, "mtcars.xlsx")
#' new_mtcars = read_labelled_xlsx("mtcars.xlsx")
#' str(new_mtcars)
#' 
#' # to SPSS syntax
#' write_labelled_spss(mtcars, "mtcars.csv")
#' 
#' }
write_labelled_csv = function(x, 
                              filename, 
                              remove_new_lines = TRUE,
                              single_file = TRUE,
                              ...){
  if(remove_new_lines){
    col_index = seq_along(x)
    for(each in col_index){
      if (is.factor(x[[each]])){
        levels(x[[each]]) = gsub("[\\n\\r]+"," ", levels(x[[each]]), perl = TRUE)
      }
      if (is.character(x[[each]])){
        x[[each]] = gsub("[\\n\\r]+"," ", x[[each]], perl = TRUE)
      }
    }
  }

  fwrite_args = list(file = filename, ...) 
  internal_args = list(quote = "auto",
                       col.names = TRUE,
                       row.names = FALSE,
                       sep = ",",
                       dec = ".",
                       na = "",
                       logical01 = FALSE)
  internal_args = internal_args %n_d% names(fwrite_args)
  fwrite_args = c(fwrite_args, internal_args)
  dict = create_dictionary(x, remove_repeated = FALSE, use_references = TRUE)
  if(single_file){
    if(nrow(dict)>0){
      # we don't write empty dictionary to single file
      dict = sheet("#dict" = "#", dict)
      do.call(fwrite, c(list(x = dict), fwrite_args))
      fwrite_args[["append"]] = TRUE
    }
  } else {
    fwrite_args$file = dic_filename(filename)
    do.call(fwrite, c(list(x = dict), fwrite_args))
    fwrite_args$file = filename
  }
  do.call(fwrite, c(list(x = x), fwrite_args))
  invisible(NULL)
  
}

#' @export
#' @rdname write_labelled_csv
write_labelled_csv2 = function(x, 
                               filename, 
                               remove_new_lines = TRUE,
                               single_file = TRUE,
                               ...){
  write_labelled_csv(x = x, 
                     filename = filename,
                     remove_new_lines = remove_new_lines,
                     single_file = single_file,
                     sep = ";", 
                     dec = ",",
                     ...                       
  )
}

#' @export
#' @rdname write_labelled_csv
write_labelled_tab = function(x, 
                              filename, 
                              remove_new_lines = TRUE,
                              single_file = TRUE,
                              ...){
  write_labelled_csv(x = x, 
                     filename = filename,
                     remove_new_lines = remove_new_lines,
                     single_file = single_file,
                     sep = "\t", 
                     dec = ".",
                     ...                       
  )
}

#' @export
#' @rdname write_labelled_csv
write_labelled_tab2 = function(x, 
                               filename, 
                               remove_new_lines = TRUE,
                               single_file = TRUE,
                               ...){
  write_labelled_csv(x = x, 
                     filename = filename,
                     remove_new_lines = remove_new_lines,
                     single_file = single_file,
                     sep = "\t", 
                     dec = ",", 
                     ...                       
  )
}

#' @export
#' @rdname write_labelled_csv
write_labelled_xlsx = function(x, 
                               filename, 
                               data_sheet = "data",
                               dict_sheet = "dictionary",
                               remove_repeated = FALSE, 
                               use_references = TRUE){
  if(!requireNamespace("openxlsx", quietly = TRUE)){
    stop("write_labelled_xlsx: 'openxlsx' is required for this function. Please, install it with 'install.packages('openxlsx')'.")
  }
  stopifnot(
    is.data.frame(x),
    length(filename)==1L,
    is.character(filename),
    length(remove_repeated)==1L,
    remove_repeated %in% c(TRUE, FALSE),
    length(use_references)==1L,
    use_references %in% c(TRUE, FALSE),
    is.character(data_sheet),
    length(data_sheet) == 1L,
    is.character(dict_sheet),
    length(dict_sheet) == 1L
  )
  wb = openxlsx::createWorkbook()
  sh = openxlsx::addWorksheet(wb, sheetName = data_sheet)
  openxlsx::writeData(wb = wb, 
                      sheet = sh,
                      x = unlab(x),
                      borderColour = "black",
                      borderStyle = "none",
                      keepNA = FALSE)
  openxlsx::freezePane(wb, sh, firstCol = TRUE, firstRow = TRUE)
  dict = create_dictionary(x,
                           remove_repeated = remove_repeated,
                           use_references = use_references
  )
  if(nrow(dict)>0){
    sh = openxlsx::addWorksheet(wb, sheetName = dict_sheet)
    openxlsx::writeData(wb = wb, 
                        sheet = sh,
                        x = dict,
                        borderColour = "black",
                        borderStyle = "none",
                        keepNA = FALSE)
    openxlsx::freezePane(wb, sh, firstCol = TRUE, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}

#' @export
#' @rdname write_labelled_csv
write_labelled_fst = function(x, 
                              filename,
                              ...){
  if(!requireNamespace("fst", quietly = TRUE)){
    stop("write_labelled_fst: 'fst' package is required for this function. 
         Please, install it with 'install.packages('fst')'.")
  }
  stopifnot(
    is.data.frame(x),
    is.character(filename),
    length(filename)==1L
  )
  dict = create_dictionary(x, remove_repeated = FALSE, use_references = TRUE)
  if(nrow(dict)>0){
    # it's a pity but fst cannot write empty data.frame
    fst::write_fst(dict, dic_filename(filename), ...)
  } else {
    if(file.exists(dic_filename(filename))){
      # if we rewrite file without labels over file which had labels we need to remove old dictionary
      unlink(dic_filename(filename))      
    }
  }
  fst::write_fst(x, filename, ...)
  invisible(NULL)
}

#' @export
#' @rdname write_labelled_csv
read_labelled_csv = function(filename, 
                             undouble_quotes = TRUE,
                             ...){
  fread_args = list(file = filename, ...) 
  internal_args = list(header= TRUE, 
                       sep = ",", 
                       dec = ".",
                       na.strings="", 
                       stringsAsFactors=FALSE, 
                       integer64 = "character",
                       logical01 = FALSE,
                       data.table = FALSE)
  internal_args = internal_args %n_d% names(fread_args)
  fread_args = c(fread_args, internal_args)
  dict = extract_dictionary(fread_args)
  if(!is.null(dict)){
    # embedded dictionary
    fread_args$skip = nrow(dict) + 1 
    
  } else {
    # dictionary in standalone file
    dic_filename = dic_filename(filename)
    if(file.exists(dic_filename)){
      fread_args$file = dic_filename
      dict = do.call(fread, fread_args) 
      fread_args$file = filename
    } else {
      # old format dictionary
      old_dic = paste0(filename,".dic.R")
      if(file.exists(old_dic)){
        old_read_labelled_csv_args = fread_args %n_i% c("sep", "dec", "encoding")
        old_read_labelled_csv_args = c(list(filename =filename), 
                                       old_read_labelled_csv_args,
                                       list(undouble_quotes = undouble_quotes))
        return(
          do.call(old_read_labelled_csv, old_read_labelled_csv_args)
        )
      } else {
        # dictionary not found
        # message("read_labelled_csv: embedded dictionary or dictionary file '", basename(dic_filename),
        #         "' not found. Labels will not be applied to data.")
      }
    }
  }
  if(undouble_quotes && !is.null(dict)) dict = remove_duplicated_quotes(dict)
  res = do.call(fread, fread_args)
  if(undouble_quotes) res = remove_duplicated_quotes(res)
  if(!is.null(dict)) res = apply_dictionary(res, dict)
  res
}




#' @export
#' @rdname write_labelled_csv
read_labelled_csv2 = function(filename, 
                              undouble_quotes = TRUE,
                              ...){
  read_labelled_csv(filename = filename,
                    undouble_quotes = undouble_quotes,
                    sep = ";", 
                    dec = ",",
                    ...)
}

#' @export
#' @rdname write_labelled_csv
read_labelled_tab = function(filename, 
                             undouble_quotes = TRUE,
                             ...){
  read_labelled_csv(filename = filename,
                    undouble_quotes = undouble_quotes,
                    sep = "\t", 
                    dec = ".", 
                    ...)
}

#' @export
#' @rdname write_labelled_csv
read_labelled_tab2 = function(filename, 
                              undouble_quotes = TRUE,
                              ...){
  read_labelled_csv(filename = filename,
                    undouble_quotes = undouble_quotes,
                    sep = "\t", 
                    dec = ",", 
                    ...)
}

#' @export
#' @rdname write_labelled_csv
read_labelled_xlsx = function(filename, 
                              data_sheet = 1,
                              dict_sheet = "dictionary"){
  if(!requireNamespace("openxlsx", quietly = TRUE)){
    stop("read_labelled_xlsx: 'openxlsx' is required for this function. Please, install it with 'install.packages('openxlsx')'.")
  }
  stopifnot(
    length(filename)==1,
    is.character(filename),
    length(data_sheet)==1,
    is.numeric(data_sheet) || is.character(data_sheet),
    length(dict_sheet)==1,
    is.numeric(dict_sheet) || is.character(dict_sheet)
  )
  wb = openxlsx::loadWorkbook(file = filename)
  data = openxlsx::readWorkbook(wb,
                                sheet = data_sheet,
                                colNames = TRUE,
                                rowNames = FALSE,
                                skipEmptyRows = FALSE,
                                check.names = FALSE,
                                na.strings = ""
  )
  sheet_names = names(wb)
  if((dict_sheet %in% sheet_names) ||(dict_sheet %in% seq_along(sheet_names))){
    dict = openxlsx::readWorkbook(wb,
                                  sheet = dict_sheet,
                                  colNames = TRUE,
                                  rowNames = FALSE,
                                  skipEmptyRows = FALSE,
                                  check.names = FALSE,
                                  na.strings = ""
    ) 
    data = apply_dictionary(data, dict)
  } else {
    if(!missing(dict_sheet)){
      message("read_labelled_xlsx: sheet '", dict_sheet,
              "' with dictionary not found. Labels will not be applied to data.")
    }
  }
  data
}  


#' @export
#' @rdname write_labelled_csv
read_labelled_fst = function(filename,
                              ...){
  if(!requireNamespace("fst", quietly = TRUE)){
    stop("read_labelled_fst: 'fst' package is required for this function. 
         Please, install it with 'install.packages('fst')'.")
  }
  dic_filename = dic_filename(filename)
  data = fst::read_fst(filename, ...)
  if(file.exists(dic_filename)){
    dict = fst::read_fst(dic_filename)  
    data = apply_dictionary(data, dict)
  } else {
    # message("read_labelled_fst: file '", basename(dic_filename),
    #         "' with dictionary not found. Labels will not be applied to data.")  
  }
  data
}




#' @export
#' @rdname write_labelled_csv
write_labelled_spss = function(x, 
                               filename, 
                               fileEncoding = "",
                               remove_new_lines = TRUE,
                               ...){
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
  syntax = sprintf("\nSAVE OUTFILE='%s'.\nEXECUTE.\n", paste0(normalizePath(filename, mustWork = FALSE), ".sav"))
  writeLines(text = syntax, con = conn)
  invisible(NULL) 
}

#' @export
#' @rdname write_labelled_csv
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
    vars = paste0("VAL LAB ",paste0(vars[1], " TO ", vars[length(vars)]))
  } else {
    vars = paste0("VAL LAB ",vars)
  }
  labs = paste0('"', gsub('"',"'",names(named_vec)), '"')
  vallab = paste0("    ",named_vec,' ',labs,'')[labs!=""]
  vallab = paste(vallab, collapse = "\n")
  sprintf('%s\n%s.\n',vars,vallab)
  
}




remove_duplicated_quotes = function(df){
  all_columns = seq_along(df)
  for(i in all_columns){
    if(is.character(df[[i]])){
      df[[i]] = gsub('\"\"','\"',df[[i]], fixed = TRUE)    
    }
  }
  df
}

extract_dictionary = function(fread_args){
  filename = fread_args[["file"]]
  first_line = fread(filename, nrows = 1, sep = NULL, header = FALSE)[[1]]
  if(substr(first_line, 1, 5) != "#dict" &&
     substr(first_line, 1, 6) != '"#dict' &&
     substr(first_line, 1, 6) != "'#dict" 
     ) return(NULL)
  
  con = file(filename, "r")
  on.exit(close(con))
  nrows = 0
  while(TRUE) {
    line = readLines(con, n = 1)
    if(length(line) == 0 || (
       substr(line, 1, 1) != "#" &&
       substr(line, 1, 2) != '"#' &&
       substr(line, 1, 2) != "'#" )
       ){
      break
    }
    nrows = nrows + 1
  }
  fread_args$nrows = nrows - 1
  res = do.call(fread, fread_args)
  res[[1]] = NULL
  res
}

dic_filename = function(filename, dic_ext = "dic"){
  base = gsub("^(.+)\\.(.+)$", "\\1", filename, perl = TRUE)
  ext = gsub("^(.+)\\.(.+)$", "\\2", filename, perl = TRUE)
  if(base==filename){
    paste(base, dic_ext, sep = ".")
  } else {
    paste(base, dic_ext, ext, sep = ".")
  }
}

#' @rdname write_labelled_csv
#' @export
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

#' @export
#' @rdname write_labelled_csv
create_dictionary = function(x, remove_repeated = FALSE, use_references = TRUE){
  if (!is.data.frame(x)) x = as.data.frame(x, stringsAsFactors = FALSE, check.names = TRUE)
  all_names = unique(colnames(x))
  
  raw_dict = lapply(all_names, function(each_var) list(variable = each_var, 
                                                       var_lab = var_lab(x[[each_var]]),
                                                       val_lab = val_lab(x[[each_var]])
  )
  )
  
  if(use_references){
    references = rep(NA, length(all_names))
    for(i in seq_along(raw_dict)[-1]){
      if(!is.null(raw_dict[[i]]$val_lab) && identical(raw_dict[[i]]$val_lab, raw_dict[[i-1]]$val_lab)){
        if(is.na(references[i-1])){
          references[i] = all_names[i-1]
        } else {
          references[i] = references[i-1]
        }
      } 
    }
  } 
  for(i in seq_along(raw_dict)){
    curr_dict = raw_dict[[i]]
    varlabs = NULL
    vallabs = NULL
    if(!is.null(curr_dict$var_lab)){
      varlabs = sheet(value = NA, label = curr_dict$var_lab, meta = "varlab")
    } 
    if(!is.null(curr_dict$val_lab)){
      if(use_references && !is.na(references[i])){
        vallabs = sheet(value = NA, label = references[i], meta = "reference")
      } else {
        vallabs = sheet(value = curr_dict$val_lab, label = names(curr_dict$val_lab), meta = NA)
      }
    }
    if(!is.null(varlabs) || !is.null(vallabs)){
      raw_dict[[i]] = sheet(variable = curr_dict$variable, rbind(varlabs, vallabs))
    } else {
      raw_dict[[i]] = logical(0)
    }
  }
  raw_dict = raw_dict[lengths(raw_dict)>0]
  if(length(raw_dict)>0){
    res = do.call(rbind, c(raw_dict, list(stringsAsFactors = FALSE, make.row.names = FALSE)))
    if(remove_repeated){
      to_na = c(FALSE, res[["variable"]][-1] == res[["variable"]][-NROW(res)]) 
      res[["variable"]][to_na] = NA
    }
  } else {
    res = sheet(variable = NA, value = NA, label = NA, meta = NA)[FALSE,]
  }
  res
}


#' @export
#' @rdname write_labelled_csv
apply_dictionary = function(x, dict){
  stopifnot(is.data.frame(x),
            is.data.frame(dict),
            all(c("variable", "value", "label", "meta") %in% colnames(dict)) 
  )
  if(nrow(dict)==0) return(x)
  dict[["variable"]][dict[["variable"]] %in% ""] = NA
  dict[["meta"]][dict[["meta"]] %in% ""] = NA
  # fill NA
  for(i in seq_len(nrow(dict))[-1]){
    if(is.na(dict[["variable"]][i])) {
      dict[["variable"]][i] = dict[["variable"]][i - 1]
    } 
  }
  dict = dict[dict$variable %in% colnames(x), ]
  # variable labels 
  all_varlabs = dict[dict$meta %in% "varlab",]
  for(i in seq_len(nrow(all_varlabs))){
    var_lab(x[[all_varlabs$variable[i]]]) = all_varlabs$label[i]   
  }
  # value labels
  vallabs = dict[dict$meta %in% NA,]
  references = dict[dict$meta %in% "reference",]
  vallabs = lapply(
    split(vallabs, vallabs$variable),
    function(each_dict) setNames(type.convert(each_dict$value, as.is = TRUE), if_na(each_dict$label, ""))
  )
  missing_references = setdiff(unique(references$label), names(vallabs))
  if(length(missing_references)>0){
    warning(paste0(" missing references - ", paste(paste0("'", missing_references, "'"), collapse = ", ")))
    references = references[references$label %in% names(vallabs), ]
  }
  for(i in seq_len(nrow(references))){
    add_val_lab(x[[references$variable[i]]]) = vallabs[[references$label[i]]]
  }
  for(i in seq_along(vallabs)){
    add_val_lab(x[[names(vallabs)[i]]]) = vallabs[[i]]
  }
  x
}


#' @export
#' @rdname write_labelled_csv
old_write_labelled_csv = function(x, 
                              filename, 
                              fileEncoding = "", 
                              remove_new_lines = TRUE,
                              ...){
  .Deprecated("write_labelled_csv")
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
  data.table::fwrite(x = x, 
                     file = filename,
                     quote = TRUE,
                     col.names = TRUE,
                     row.names = FALSE,
                     na = "",
                     ...
  )
  dic_file = paste0(filename,".dic.R")
  write_labels(x = x, filename = dic_file, fileEncoding = fileEncoding)
  invisible(NULL)
  
}

#' @export
#' @rdname write_labelled_csv
old_read_labelled_csv = function(filename, 
                                 fileEncoding = "", 
                                 undouble_quotes = TRUE,
                                 ...){
  w = data.table::fread(filename, 
                        header= TRUE, 
                        na.strings="", 
                        stringsAsFactors=FALSE, 
                        integer64 = "character",         
                        data.table = FALSE,
                        ...)
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