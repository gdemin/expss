#' Internal function for checking multiple/single-response questions for valid values 
#' 
#' This is low-level function intended for programming. For interactive usage
#' see functions \code{\link{mult}} and \code{\link{sngl}}.
#'  
#' @param dfs Vector/data.frame/matrix that should be checked.
#' @param values Valid values. All other values will be considered incorrect.
#' @param exclusive Numeric/character values. These values should be exclusive. 
#' All other values should be NA if any of exclusive values exists in row. 
#' @param mult Logical. Should we check dfs as multiple response question? 
#' @param no_dup Logical. Should we check for absence of duplicated values in each row?
#' @param cond Logical vector. TRUE indicated rows in dfs that should contain valid
#'  values. In other rows all dfs values should be NA. It used for questions that
#'   were asked by condition on answer on previous questions.
#' @param subset Logical vector. TRUE indicated rows in dfs that should be checked. 
#' Other rows will be ignored.
#' @param x Check object for printing.
#' @param error_num Numeric. How many errors should be printed?
#' @param object Check object for summary.
#' @param skip_details Logical. For more terse summary.
#' 
#' @return 
#' \code{check_internal} return object of class 'check'. It is data.frame that contains 
#' check result for each row of dfs and description for each error if any of them
#'  exists.
#' \code{print.check} invisibly returns its argument x. 
#' \code{summary.check} returns list with summary check.
#' 
#' @details
#' 'mult=TRUE' for multiple response questions means that it is allowed to have NA
#'  values in row. It is only necessary for multiple response questions that each
#'  row will have at least one non-NA values.
#'  This function checks multiple response questions only with categorical coding.
#'  For checking multiple response questions with dichotomous coding see \code{\link{dmult}}. 
#'  If 'mult=FALSE' all values should be non-NA. However one can put NA in 'values' argument.
#'  Then NA will be considered valid.
#'  By default if 'mult=TRUE' then no_dup also is TRUE.
#'  If 'values' is missing than all values considered valid except NA.
#'  'check' report only for first error in row. If there are other errors for 
#'  this case they will be reported only after correction of first error.
#'
#' @seealso \code{\link{mult}}, \code{\link{mult_}}, \code{\link{sngl}}, 
#' \code{\link{sngl_}}, \code{\link{dmult}}
#' @export
#' @examples
#' 
#' library(dplyr)
#' data(ProductTestRaw)
#' 
#' ## Example 1 ##
#' 
#' # 4 errors: 2 missing, 2 invalid codes
#' check_internal(ProductTestRaw$s2b,values=2:3)
#' 
#' ## Example 2 ##
#' 
#' data(codeframe)
#' valid_a1 = make_labels(codeframe$likes)
#' 
#' # Exclusive values
#' # 1 Liked everything
#' # 2 Disliked everything
#' # 99 Hard to say
#' 
#' # 5 errors: 1 missing value, 1 invalid code, 1 code duplication, 
#' # 2 non-exclusive values
#' check_internal(select(ProductTestRaw,a1_1:a1_6),values=valid_a1,
#'      mult = TRUE, exclusive=c(1,2,99))
#' 
#' ## Example 3 ##
#' 
#' valid_a4 = make_labels(codeframe$dislikes_in_appearance)
#' # question a4 was asked only if codes 1-4 marked in a3
#' # 3 errors: 1 missing value, 1 invalid code, 1 code in case of a3 in 5-7.
#' check_internal(select(ProductTestRaw,a4_1:a4_6),
#'      values=valid_a4,mult = TRUE, exclusive=99,
#'       cond = ProductTestRaw$a3 %in%  1:4)
#' 
check_internal = function(dfs,values=NULL,exclusive=NULL,mult=FALSE,no_dup = mult,cond=NULL,subset = NULL){
    raw_dfs = as.data.frame(dfs,stringsAsFactors=FALSE)
    if (is.null(subset)) {
        dfs = raw_dfs        
    } else { 
        subset = subset & !is.na(subset)
        dfs = raw_dfs[subset,,drop=FALSE]
        cond = cond[subset]
    }
    if (!is.null(cond)) cond = cond & !is.na(cond)
    ##########
    if (!is.null(exclusive) & !is.null(values)){
        
        values = unique(c(values,exclusive))
    }
    
    if (mult) {
        chk_res = check_mult(dfs,values,cond)
        chk_quest = apply(raw_dfs,1,function(x) paste(x[!is.na(x)],collapse=","))
        chk_quest[chk_quest==""] = NA
    } else {
        chk_res = check_single(dfs,values,cond)
        chk_quest = apply(raw_dfs,1,function(x) paste(x,collapse=","))
    }
    
    ##############
    if (no_dup){
        if(!is.null(cond)){
            next_check = is.na(chk_res$chk_err) & (!cond)
        }   else {
            next_check = is.na(chk_res$chk_err) 
        }     
        chk_dup = check_dup(dfs[next_check,,drop=FALSE])
        chk_res[next_check,"chk_err"] = chk_dup$chk_err        
        chk_res[next_check,"chk_val"] = chk_dup$chk_val  
        
    }
    
    ####################
    if (!is.null(exclusive)){
        if(!is.null(cond)){
            next_check = is.na(chk_res$chk_err) & (!cond)
        }   else {
            next_check = is.na(chk_res$chk_err) 
        }     
        chk_exclusive = check_exclusive(dfs[next_check,,drop=FALSE],exclusive)
        chk_res[next_check,"chk_err"] = chk_exclusive$chk_err        
        chk_res[next_check,"chk_val"] = chk_exclusive$chk_val  
        
    }
        
    ##############
    
    fin = setNames(data.frame(chk_quest,stringsAsFactors = FALSE),.CHK_QUEST)
    
    if(!is.null(subset)){
        fin[subset,.CHK_ERR] = chk_res$chk_err        
        fin[subset,.CHK_VAL] = chk_res$chk_val        
        if (!is.null(cond)) fin[subset,.CHK_COND] = cond        
    } else {
        fin[,c(.CHK_ERR,.CHK_VAL)] = chk_res
        if (!is.null(cond)) fin[,.CHK_COND] = cond
    }
    res = data.frame(fin[,-1],fin[,1,drop=FALSE],stringsAsFactors = FALSE)
    class(res) = unique(c("check",class(res)))
    res
}

#' @export
#' @rdname check_internal
print.check=function(x,error_num=20,...){
    stopif(!all(.CHK_COLUMNS %in% colnames(x)),"Incorrect 'check' object. There are no some of check columns.")
    check_summary = summary(x)
    if (identical(check_summary,.NO_ERROR_MESSAGE)){
        cat(.NO_ERROR_MESSAGE,"\n")
        return()
    }
    resample = function(x,n) x[sort(sample.int(length(x), n))]
    errors_row =(1:nrow(x))[!is.na(x[,.CHK_ERR])] 
    valid_row = (1:nrow(x))[is.na(x[,.CHK_ERR])]
    errors = length(errors_row) 
    no_errors = length(valid_row)
    if (errors>error_num){
        block_with_error = x[resample(errors_row,error_num),,drop=FALSE]
        cat("==========================================================\n")
        cat("Random sample of",error_num,"error(s) from",errors,"\n")
    } else {
        cat("==========================================================\n")
        cat("Total",errors,"error(s)\n")
        block_with_error = x[errors_row,,drop=FALSE]
        
    }
    print(tbl_df(block_with_error),n=error_num, width=Inf)
    if (no_errors>0){
        if (no_errors>min(error_num,errors)){
            if (errors<5) {
                num_valid = min(5,no_errors)
            } else {
                num_valid = min(error_num,errors)
            }
            cat("\n==========================================================\n")
            cat("Random sample of",num_valid,"valid case(s) from",no_errors,"\n")
            valid_block = x[resample(valid_row,num_valid),,drop=FALSE]
            
        } else {
            cat("\n==========================================================\n")
            cat("Total",no_errors,"valid case(s)\n")
            valid_block = x[valid_row,,drop=FALSE]
            
        }
        print(tbl_df(valid_block),n=error_num, width=Inf)
    } else {
        cat("\nNo valid cases\n")
    }
    
    if ("Details" %in% names(check_summary)){
        cat("\n==========================================================\n")
        cat("Details\n")
        print(check_summary$Details)
    }
    cat("\n==========================================================\n")
    cat("Summary\n")
    print(check_summary$Summary)
    invisible(x)
    
}


#' @export
#' @rdname check_internal
summary.check=function(object,skip_details = FALSE){
    stopif(!all(.CHK_COLUMNS %in% colnames(object)),"Incorrect 'check' object. There are no some of check columns.")
    valid =is.na(object[,.CHK_ERR]) 
    if (all(valid)){
        return(.NO_ERROR_MESSAGE)
    }
    if ((!skip_details) & (.CHK_VAL %in% colnames(object))){
        only_errors = object[!valid,,drop = FALSE]
        detailed_res = table(only_errors[,.CHK_VAL],only_errors[,.CHK_ERR],useNA = "ifany")
        #         detailed_res = detailed_res[,!is.na(colnames(detailed_res)),drop=FALSE]
        detailed_res = addmargins(detailed_res,1,FUN = list("Total" = sum),quiet = TRUE)
        
    } else detailed_res = NULL
    res = table(object[,.CHK_ERR],useNA = "always")
    names(res)[is.na(names(res))] = "No errors"
    res = cbind(res,round(prop.table(res)*100,1))
    res = addmargins(res,1,FUN = list("Total" = sum),quiet = TRUE)
    colnames(res) = c("Count","%")
    if (!is.null(detailed_res)){
        list("Details"= detailed_res,"Summary"=res)
    } else {
        list("Summary"=res)
    }
    
}

#### special columns which are created in data.frame for checking
.CHK_ERR = ".Error" 
.CHK_COND = ".Condition" 
.CHK_SUBSET = ".Subset" 
.CHK_VAL = ".IncorrectValue"
.CHK_QUEST = ".VariableValue"

.CHK_COLUMNS = c(.CHK_ERR)  # .CHK_VAL,.CHK_QUEST

#### Error codes
# .ERROR_SNGL_MISSING = "Missing/out of range"
# .ERROR_MULT_MISSING = "Missing/out of range"
.ERROR_OUT_OF_RANGE = "Missing/Out of range"
# .ERROR_MULT_OUT_OF_RANGE = "Missing/Out of range"
.ERROR_DUPLICATED_VALUE = "Duplicated value"
.ERROR_EXCLUSIVE = "Exclusive value"
.ERROR_VALUE_SHOULD_BE_MISSING = "Value should be missing"
.NO_ERROR_MESSAGE = "No error"
.ERROR_IF = "Logical error"

# internal functions

######################

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

################

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


#################

check_dup = function(dfs,incomparables = NULL){
    chk_dup = apply(dfs,1, function(each_row) {
        dup = anyDuplicated(each_row,incomparables=c(NA,incomparables))
        if (dup>0) each_row[dup] else NA
    })
    data.frame(
        chk_err=ifelse(is.na(chk_dup),NA,.ERROR_DUPLICATED_VALUE),
        chk_val=chk_dup,
        stringsAsFactors = FALSE)
}

#################

check_exclusive = function(dfs,exclusive){
    count_notna = row_countif(,dfs)
    exclusives = build_criterion(exclusive,dfs)
    count_exclusives = rowSums(exclusives,na.rm = TRUE)
    chk = ifelse((count_exclusives>1) | (count_exclusives>0 & count_notna>count_exclusives), .ERROR_EXCLUSIVE,NA)
    chk_res=data.frame(chk_err=chk,chk_val=NA,stringsAsFactors = FALSE)
    if (!all(is.na(chk))){
        # if errors 
        res = which(as.matrix(exclusives[!is.na(chk),,drop=FALSE]),arr.ind=TRUE) # find rows,columns of incorrect values
        res = res[!duplicated(res[,1]),,drop=FALSE] # we will report only first error in each case
        temp = dfs[!is.na(chk),,drop=FALSE]
        chk_res[!is.na(chk),"chk_val"][res[,1]] = unlist(lapply(1:nrow(res),
                                                                function(x) temp[res[x,1],res[x,2]])) # get incorrect values
        
    }
    chk_res
    
}

##############








