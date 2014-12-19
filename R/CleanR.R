#### special columns which are created in data.frame for checking
.CHK_ERR = ".CHK_ERROR" 
.CHK_COND = ".CHK_COND" 
.CHK_SUBSET = ".CHK_SUBSET" 
.CHK_VAL = ".CHK_VALUE"
.CHK_QUEST = ".CHK_QUESTION"

.CHK_COLUMNS = c(.CHK_ERR,.CHK_VAL, .CHK_COND,.CHK_QUEST)

#### Error codes
.ERROR_SNGL_MISSING = "Value is missing"
.ERROR_MULT_MISSING = "Value is missing"
.ERROR_SNGL_OUT_OF_RANGE = "Value is out of range"
.ERROR_MULT_OUT_OF_RANGE = "Value is out of range"
.ERROR_DUPLICATED_VALUE = "Duplicated value"
.ERROR_UNIQUE = "Value is not unique"
.ERROR_VALUE_SHOULD_BE_MISSING = "Value should be missing"

always = function(dfs){
    UseMethod("always")
}

always.data.frame = function(dfs){
    attr(dfs,"always",exact = TRUE)
}

"always<-" = function(x,value){
    
    UseMethod("always<-")
}

"always<-.data.frame" = function(x,value){
    stopifnot_message(all(value %in% colnames(x)),"'",paste(setdiff(value,colnames(x)),collapse=","),"' doesn't exist in data.frame")
    attr(x,"always") = value
    x
    
}

check = function(dfs,values=NULL,uniq=NULL,mult=FALSE,no_dup = FALSE,cond=NULL,subset = NULL){
    raw_dfs = as.data.frame(dfs,stringsAsFactors=FALSE)
    if (is.null(subset)) {
        dfs = raw_dfs        
    } else { 
        dfs = raw_dfs[subset,,drop=FALSE]
        if (!is.null(cond)) cond = cond[subset]
    }
    valid = build_criterion(values,dfs)
    count_valid = rowSums(valid,na.rm=TRUE)
    chk = ifelse(count_valid == NCOL(dfs),NA,.ERROR_SNGL_OUT_OF_RANGE)
    chk_res=data.frame(chk_err=chk,chk_val=NA,stringsAsFactors = FALSE)
    if (!all(is.na(chk))){
        res = which(!as.matrix(valid),arr.ind=TRUE)
        res = res[!duplicated(res[,1]),,drop=FALSE]
        chk_res[res[,1],"chk_val"] = unlist(lapply(1:nrow(res),function(x) dfs[res[x,1],res[x,2]]))
        chk_res$chk_err = with(chk_res,ifelse(is.na(chk_val) & !is.na(chk_err),.ERROR_SNGL_MISSING,chk_err)) 
    }
    if (!is.null(cond)){
        count_notna = row_countif(NULL,dfs) # count non-NA
        chk_res$chk_err=ifelse((count_notna>0) & !cond,.ERROR_VALUE_SHOULD_BE_MISSING,chk_res$chk_err)
    } else {
        cond=NA
    }    
    chk_quest = apply(raw_dfs,1,function(x) paste(x,collapse=","))
    fin = setNames(data.frame(chk_quest,stringsAsFactors = FALSE),.CHK_QUEST)
    if(!is.null(subset)){
        fin[subset,.CHK_ERR] = chk_res$chk_err        
        fin[subset,.CHK_VAL] = chk_res$chk_val        
        fin[subset,.CHK_COND] = cond        
    } else {
        fin[,c(.CHK_ERR,.CHK_VAL,.CHK_COND)] = data.frame(chk_res,cond,stringsAsFactors = FALSE)
    }
    res = fin[,c(.CHK_ERR,.CHK_VAL,.CHK_COND,.CHK_QUEST)]
    if (!is.null(always(raw_dfs))){
        res =data.frame(raw_dfs[,always(raw_dfs),drop = FALSE],res,stringsAsFactors = FALSE)
        
    }
    class(res) = unique(c("check",class(res)))
    res
}


print.check=function(object,error_num=20,...){
    stopifnot_message(all(.CHK_COLUMNS %in% colnames(object)),"Incorrect 'check' object. There are no some of check columns.")
        
    
}

summary.check=function(object,skip_details = FALSE){
    stopifnot_message(all(.CHK_COLUMNS %in% colnames(object)),"Incorrect 'check' object. There are no some of check columns.")
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


# internal function
check_single_ = function(dfs,values=NULL){
    dfs = as.data.frame(dfs,stringsAsFactors=FALSE)
    res = build_criterion(values,dfs)
    cnt = rowSums(res,na.rm=TRUE)
    chk = ifelse(cnt == NCOL(dfs),NA,.ERROR_SNGL_OUT_OF_RANGE)
    if (!all(cnt == NCOL(dfs))){
        res = which(!as.matrix(res),arr.ind=TRUE)
        res = res[!duplicated(res[,1]),]
    }
    chk = ifelse(cnt == NCOL(dfs),NA,.ERROR_SNGL_OUT_OF_RANGE)
    chk
}

check_mult = function(dfs,values=NULL){
    dfs = as.data.frame(dfs)
    cnt = row_countif(values,dfs)
    cnt2 = row_countif(NULL,dfs)
    chk = ifelse(cnt>0,NA,.ERROR_MULT_MISSING)
    chk = ifelse(is.na(chk),NA,ifelse(cnt2>cnt,NA,.ERROR_MULT_OUT_OF_RANGE)
    chk 
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



