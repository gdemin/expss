correct = function(.data){
    check = last_check(.data)
    chk_res = check_result(.data)
    chk_col = chk_res[,.CHK_ERR]
    vars = check$vars
    vars = select_vars_(names(.data),args=vars)
    if (all(is.na(chk_col))) return(invisible(.data))
    err_rows = which(!is.na(chk_col))
    for (each in err_rows){
        print(chk_res[each,])
        new_val = readline("Enter corrrect value:")
        new_val = gsub("(^[\\s]+)|([\\s]+$)","",new_val,perl=TRUE)
        if(new_val == ""){
            print("skipped")
        } else {
            if(is.numeric(.data[,vars[1]])) new_val = as.numeric(new_val)
            .data[each,vars] = new_val
            print("corrected")
        }
    }
    invisible(.data)
    
}