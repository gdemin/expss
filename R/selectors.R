'%to%' = function(e1, e2){
    e1 = as.character(substitute(e1))
    e2 = as.character(substitute(e2))
    digits1 = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", e1, perl = TRUE))
    digits2 = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", e2, perl = TRUE))
    patt1 = gsub("^(.+?)([\\d]+)$", "\\1", e1, perl = TRUE)
    patt2 = gsub("^(.+?)([\\d]+)$", "\\1", e2, perl = TRUE)
    # cat(patt1,"\n")
    # cat(paste0("^", patt1, "[:digit:]+$"),"\n")
    var_names = ls(name = parent.frame(1), pattern = paste0("^", patt1, "[0-9]+$"))
    digits = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", var_names, perl = TRUE))
    var_names = var_names[digits>=digits1 & digits<=digits2] 
    # print(ls(name = parent.frame(1)))
    objects = ls(name = parent.frame(1), all.names = TRUE)
    # for mutate dplyr
    if ("." %in% objects){
        envir = get(".", envir = parent.frame(1))
    } else {
        envir = parent.frame(1)
    }
    mget(var_names, envir = envir)
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

temp = function(x){
    res = ls(name = parent.frame(x))
    cat(x, ls(name = parent.frame(x)), "\n")
    # if ("enclos" %in% res ){
    #     cat(ls(name = get("enclos",envir = parent.frame(x))),"\n")
    # }
    if ("envir" %in% res ){
        cat(ls(name = get("envir",envir = parent.frame(x))),"\n")
        print(get("envir",envir = parent.frame(x)))
    }
    if ("expr" %in% res ){
        cat(ls(name = get("expr",envir = parent.frame(x))),"\n")
        print(get("expr",envir = parent.frame(x)))
    }
    1
}

mutate(aaa,
    z = temp(1),
    z = temp(2),
    z = temp(3),
    z = temp(4)
)

with(
    aaa,{
        sum_row("b1" %to% "b4")
    } 
    
    
)

within(
    aaa,{
        total = sum_row("b1" %to% "b4")
        total2 = sum_row("b1" %to% "b3", total)
        kuku = data.frame(
            wah = 1,
            bah = 3,
            uuu = 7
        )
    } 
)

transform(
    aaa,
        total = sum_row("b1" %to% "b4"),
        total2 = sum_row("b1" %to% "b3")
)
aaa %>% mutate(
    tot = sum_row("b1" %to% "b4")
)

library(magrittr)

qqq = aaa %$% {tot = sum_row("b1" %to% "b4")
tot2 = sum_row("b1" %to% "b4", tot)
}

inside = function (data, expr, ...) {
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    eval(substitute(expr), e)
    l = as.list(e, all.names = TRUE)
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    # stopifnot(all(nrows==1L | nrows==nrow(data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    nl = c(names(data), new_vars)
    data[nl] = l[nl]
    data
}


inside(
    aaa,{
        b4 = 1
        total = sum_row("b1" %to% "b4")
        total2 = sum_row("b1" %to% "b3", total)
        kuku = data.frame(
            wah = 1,
            bah = 3,
            uuu = 7
        )
        b1 = b4*42
    } 
)







