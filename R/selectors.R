'%to%' = function(e1, e2){
    digits1 = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", e1, perl = TRUE))
    digits2 = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", e2, perl = TRUE))
    patt1 = gsub("^(.+?)([\\d]+)$", "\\1", e1, perl = TRUE)
    patt2 = gsub("^(.+?)([\\d]+)$", "\\1", e2, perl = TRUE)
    # cat(patt1,"\n")
    # cat(paste0("^", patt1, "[:digit:]+$"),"\n")
    var_names = ls(name = parent.frame(1), pattern = paste0("^", patt1, "[0-9]+$"))
    digits = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", var_names, perl = TRUE))
    var_names = var_names[digits>=digits1 & digits<=digits2] 
    print(ls(name = parent.frame(1)))
    mget(var_names, envir = parent.frame(1))
}

aa = 42
bab =57
ba = 57
ba2 = 57
a1 = 1
a2 = 2
a3 = 3
a4 = 4

"a1" %to% "a4"
"a2" %to% "a4"
"a1" %to% "a3"

aaa = data.frame(
    b1 = 1,
    b2 = 2,
    b3 = 3,
    b4 = 4
)

with(
    aaa,{
        "a1" %to% "a4"  
        
    }
    
)

library(labelr)
library(dplyr)
with(
    aaa,{
        sum_row("b1" %to% "b4")
    } 
    
    
)

within(
    aaa,{
        total = sum_row("b1" %to% "b4")
    } 
)

aaa %>% mutate(
    tot = sum_row("b1" %to% "b4")
)










