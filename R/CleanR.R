#### special columns which are created in data.frame for checking
.CHK_ERR = ".Error" 
.CHK_COND = ".Condition" 
.CHK_SUBSET = ".Subset" 
.CHK_VAL = ".IncorrectValue"
.CHK_QUEST = ".VariableValue"

.CHK_COLUMNS = c(.CHK_ERR,.CHK_VAL, .CHK_COND,.CHK_QUEST)

#### Error codes
# .ERROR_SNGL_MISSING = "Missing/out of range"
# .ERROR_MULT_MISSING = "Missing/out of range"
.ERROR_OUT_OF_RANGE = "Missing/Out of range"
# .ERROR_MULT_OUT_OF_RANGE = "Missing/Out of range"
.ERROR_DUPLICATED_VALUE = "Duplicated value"
.ERROR_UNIQUE = "Value is not unique"
.ERROR_VALUE_SHOULD_BE_MISSING = "Value should be missing"
.NO_ERROR_MESSAGE = "No error"

always = function(dfs){
    UseMethod("always")
}

always.data.frame = function(dfs){
    res = attr(dfs,"always",exact = TRUE)
    if (!is.null(res)) {
        res = intersect(res,colnames(dfs))
        if (length(res)==0) res = NULL
    }
    res
}

"always<-" = function(x,value){
    
    UseMethod("always<-")
}

"always<-.data.frame" = function(x,value){
    stopifnot_message(all(value %in% colnames(x)),"'",paste(setdiff(value,colnames(x)),collapse=","),"' doesn't exist in data.frame")
    attr(x,"always") = value
    x
    
}

check = function(dfs,values=NULL,uniq=NULL,mult=FALSE,no_dup = mult,cond=NULL,subset = NULL){
    raw_dfs = as.data.frame(dfs,stringsAsFactors=FALSE)
    if (is.null(subset)) {
        dfs = raw_dfs        
    } else { 
        dfs = raw_dfs[subset,,drop=FALSE]
        cond = cond[subset]
    }
    
    ##########
    
    if (mult) {
        chk_res = check_mult(dfs,values,cond)
        chk_quest = apply(raw_dfs,1,function(x) paste(x[!is.na(x)],collapse=","))
        chk_quest[chk_quest==""] = NA
    } else {
        chk_res = check_single(dfs,values,cond)
        chk_quest = apply(raw_dfs,1,function(x) paste(x,collapse=","))
    }
    if (is.null(cond)) cond = NA
    ##############
    fin = setNames(data.frame(chk_quest,stringsAsFactors = FALSE),.CHK_QUEST)
    if(!is.null(subset)){
        fin[subset,.CHK_ERR] = chk_res$chk_err        
        fin[subset,.CHK_VAL] = chk_res$chk_val        
        fin[subset,.CHK_COND] = cond        
    } else {
        fin[,c(.CHK_ERR,.CHK_VAL,.CHK_COND)] = data.frame(chk_res,cond,stringsAsFactors = FALSE)
    }
    res = fin[,c(.CHK_ERR,.CHK_VAL,.CHK_COND,.CHK_QUEST)]
    class(res) = unique(c("check",class(res)))
    res
}

# if (!is.null(always(raw_dfs))){
#     res =data.frame(raw_dfs[,always(raw_dfs),drop = FALSE],res,stringsAsFactors = FALSE)
#     
# }

# internal functions
check_single = function(dfs,values,cond){

    valid = build_criterion(values,dfs)
    if (!is.null(cond)){
        na = is.na(as.matrix(dfs)) 
        valid[!cond,] = na[!cond,,drop = FALSE]  # if not cond then valid oly NA's
        count_valid = rowSums(valid,na.rm=TRUE)
        chk = ifelse(count_valid != NCOL(dfs), 
                     ifelse(cond,
                            .ERROR_OUT_OF_RANGE, 
                            .ERROR_VALUE_SHOULD_BE_MISSING), 
                     NA)
        
    } else {
        count_valid = rowSums(valid,na.rm=TRUE)
        chk = ifelse(count_valid != NCOL(dfs),.ERROR_OUT_OF_RANGE, NA) 
    }
    chk_res=data.frame(chk_err=chk,chk_val=NA,stringsAsFactors = FALSE)
    if (!all(is.na(chk))){
        # if errors 
        res = which(!as.matrix(valid),arr.ind=TRUE) # find rows,columns of incorrect values
        # TODO report all values???
        res = res[!duplicated(res[,1]),,drop=FALSE] # we will report only first error in each case
        chk_res[res[,1],"chk_val"] = unlist(lapply(1:nrow(res),function(x) dfs[res[x,1],res[x,2]])) # get incorrect values
        
    }
    chk_res
}

check_mult = function(dfs,values, cond){
    valid = build_criterion(values | crit(is.na),dfs)
    na = is.na(as.matrix(dfs))
    count_notna = rowSums(!na,na.rm=TRUE)
    valid[count_notna==0,1] = FALSE # if all is NA this is not valid
    if (!is.null(cond)){
        
        valid[!cond,] = na[!cond,,drop = FALSE] # if not cond then valid oly NA's
        count_valid = rowSums(valid,na.rm=TRUE)
        chk = ifelse(count_valid != NCOL(dfs), 
                     ifelse(cond,
                            .ERROR_OUT_OF_RANGE, 
                            .ERROR_VALUE_SHOULD_BE_MISSING), 
                     NA)
        
    } else {
        count_valid = rowSums(valid,na.rm=TRUE)
        chk = ifelse(count_valid != NCOL(dfs),.ERROR_OUT_OF_RANGE, NA)
    }
    chk_res=data.frame(chk_err=chk,chk_val=NA,stringsAsFactors = FALSE)
    if (!all(is.na(chk))){
        # if errors 
        res = which(!as.matrix(valid),arr.ind=TRUE) # find rows,columns of incorrect values
        # TODO report all values???
        res = res[!duplicated(res[,1]),,drop=FALSE] # we will report only first error in each case
        chk_res[res[,1],"chk_val"] = unlist(lapply(1:nrow(res),function(x) dfs[res[x,1],res[x,2]])) # get incorrect values
        
    }
    chk_res
}

check_dup = function(dfs){
    chk_dup = apply(dfs,1, function(each_row) {
        dup = anyDuplicated(each_row,incomparables=NA)
        if (dup>0) each_row[dup] else NA
    })
    chk_dup
}

check_uniq = function(dfs,uniqs){
    cnt = row_countif(,dfs)
    cnt2 = row_countif(uniqs,dfs)
    chk_uniqs = ifelse(cnt2>1 | cnt>cnt2, .ERROR_UNIQUE,NA)
    chk
    
}


print.check=function(object,error_num=20,...){
    stopifnot_message(all(.CHK_COLUMNS %in% colnames(object)),"Incorrect 'check' object. There are no some of check columns.")
    check_summary = summary(object)
    if (identical(check_summary,.NO_ERROR_MESSAGE)){
        cat(.NO_ERROR_MESSAGE,"\n")
        return()
    }
    resample = function(x,n) x[sample.int(length(x), n)]
    errors_row =(1:nrow(object))[!is.na(object[,.CHK_ERR])] 
    valid_row = (1:nrow(object))[is.na(object[,.CHK_ERR])]
    errors = length(errors_row) 
    no_errors = length(valid_row)
    if (errors>error_num){
        block_with_error = object[resample(errors_row,error_num),,drop=FALSE]
        cat("==========================================================\n")
        cat("Random sample of",error_num,"error(s) from",errors,"\n")
    } else {
        cat("==========================================================\n")
        cat("Total",errors,"error(s)\n")
        block_with_error = object[errors_row,,drop=FALSE]
        
    }
    print(tbl_df(block_with_error),n=error_num, width=Inf)
    if (no_errors>0){
        if (no_errors>min(error_num,errors)){
            cat("\n==========================================================\n")
            cat("Random sample of",min(error_num,errors),"valid case(s) from",no_errors,"\n")
            valid_block = object[resample(valid_row,min(error_num,errors)),,drop=FALSE]
            
        } else {
            cat("\n==========================================================\n")
            cat("Total",no_errors,"valid case(s)\n")
            valid_block = object[valid_row,,drop=FALSE]
            
        }
        print(tbl_df(valid_block),n=error_num, width=Inf)
    } else {
        cat("\nNo valid cases\n")
    }
    cat("\n==========================================================\n")
    cat("Details\n")
    print(check_summary[[1]])
    cat("\n==========================================================\n")
    cat("Summary\n")
    print(check_summary[[2]])
    return()
    
}

summary.check=function(object,skip_details = FALSE){
    stopifnot_message(all(.CHK_COLUMNS %in% colnames(object)),"Incorrect 'check' object. There are no some of check columns.")
    if (all(is.na(object[,.CHK_ERR]))){
        return(.NO_ERROR_MESSAGE)
    }
    if (!skip_details){
        detailed_res = table(object[,.CHK_VAL],object[,.CHK_ERR],useNA = "always")
        detailed_res = detailed_res[,!is.na(colnames(detailed_res)),drop=FALSE]
        detailed_res = addmargins(detailed_res,1,FUN = list("Total" = sum),quiet = TRUE)
          
    }
    res = table(object[,.CHK_ERR],useNA = "always")
    names(res)[is.na(names(res))] = "No errors"
    res = cbind(res,round(prop.table(res)*100,1))
    res = addmargins(res,1,FUN = list("Total" = sum),quiet = TRUE)
     colnames(res) = c("Count","%")
    if (!skip_details){
        list("Details"= detailed_res,"Summary"=res)
    } else {
        list("Summary"=res)
    }
    
}






