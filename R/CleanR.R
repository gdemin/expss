#### special columns which are created in data.frame during checking
.CHK_ERR = ".CHK_ERROR" 
.CHK_COND = ".CHK_COND" 
.CHK_IF = ".CHK_IF" 
.CHK_VAL = ".CHK_VALUE"

.CHK_COLUMNS = c(.CHK_ERR, .CHK_COND,.CHK_IF,.CHK_VAL)

#### Error codes
.ERROR_SNGL_OUT_OF_RANGE = "Value is missing/out of range"
.ERROR_MULT_OUT_OF_RANGE = "Value is out of range"
.ERROR_MULT_MISSING = "Value is missing"
.ERROR_DUPLICATED_VALUE = "Duplicated value"
.ERROR_UNIQUE = "Value is not unique"

sngl = function(.data,values=sqrt(.data),...){
    print(list(...))
    print("values")
    print(values)
    
}



############ TODO переписать с оптимизацией ??? ##############
'%in_case%'=function(x,value){
    row_countif(value=value,...)>0
}


