#' @export
write_labelled = function(x, filename, fileEncoding = ""){
    # R doesn' read its own CSV if it contain newlines in the character field.
    for (i in seq_along(x)) if(is.character(x[,i])) x[,i] = gsub("[\\n\\r]+"," ",x[,i],perl = TRUE)
    write.table(x,file = paste0(filename,".csv"),
                sep=",",
                na="",
                row.names = FALSE,
                col.names = TRUE,
                fileEncoding = fileEncoding
    )
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
    conn = file(paste0(filename,".dic.R"),encoding = fileEncoding)
    on.exit(close(conn))
    writeLines(text= code,con = conn)
    invisible(NULL)
    
}



make_make_labs = function(vars,named_vec){
    if (is.null(named_vec) || is.null(names(named_vec)) || (length(vars)==0)) return(NULL)
    if (length(vars)>1) {
        vars = paste0("val_lab(w[,c(",paste(paste0('"',vars,'"'),collapse = ","),"])")
    } else {
        vars = paste0("val_lab(w[,",'"',vars,'"',"])")
    }
    labs = gsub('"','\\\\"',names(named_vec))
    vallab = paste0("    ",named_vec,' ',labs,'')[labs!=""]
    vallab = paste(vallab, collapse = "\n")
    sprintf('%s = make_labels("\n%s\n")',vars,vallab)
    
}


# @export
# @rdname write_labelled
read_labelled = function(filename, colClasses = NA,fileEncoding = ""){
    w = read.table(file = paste0(filename,".csv"),
                   header = TRUE,
                   sep = ",",
                   colClasses = colClasses,
                   stringsAsFactors = FALSE,
                   fileEncoding = fileEncoding
                   )
    dic_file = paste0(filename,".dic.R")
    if (file.exists(dic_file)){
        source(dic_file, local = TRUE, encoding = fileEncoding, verbose = FALSE)
    } else {
        warning(".dic.R file doesn't exists. Labels will not be applied to data.")
    }
    
    
}