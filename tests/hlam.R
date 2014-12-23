library(CleanR)

raw_data  %>% chk()  %>% sngl()
raw_data  %>% chk()  %>% cond()  %>% sngl()  %>% View
raw_data  %>% chk()  %>% chk_if()  %>% sngl() %>% fix_in_place() 
raw_data  %>% chk()  %>% chk_if()  %>% sngl() %>% () 

sngl(data,...,values=)

raw_data  %>% sngl(a1,a2,a3,values=c(1,2,3,4))
raw_data  %>% chk(a1,a2,a3) %>% sngl(1,2,3,4)  # плохо передавать проверяемые переменные в атрибутах... можно списком, но тоже плохо
raw_data  %>% chk(a1,a2,a3) %>% sngl(1,2,3,4)  # хотя можно объявить так sngl(.data,...,variabes = check_var(.data)). тогда можно будет и автономно использовать

raw_data %>% chk(sngl(a1,a2,a3),1,2,3,5)
chk(raw_data,a1,a2,a3)  %>% sngl(1,2,3,4)


# будет ли оно лучше выглядеть с плюсом???
raw_data  + sngl(a1,a2,a3,values=c(1,2,3,4))
raw_data  + chk(a1,a2,a3) + sngl(1,2,3,4)  # плохо передавать проверяемые переменные в атрибутах... можно списком, но тоже плохо
raw_data  + chk(a1,a2,a3) + sngl(1,2,3,4)  # хотя можно объявить так sngl(.data,...,variabes = check_var(.data)). тогда можно будет и автономно использовать

raw_data + chk(sngl(a1,a2,a3),1,2,3,5)
chk(raw_data,a1,a2,a3)  + sngl(1,2,3,4)


raw_data  + chk(a1,a2,a3) + sngl(1,2,3,4) 


############ check 

load("tests/checked_data.RData")
load("tests/raw_data.RData")
always(raw_data) = c("reg","nn")

w = checked_data
w = raw_data
reg_vec = c(301,342,343,345,351,353,381:385,395,421,423,473,483:485,487,492,495:497,812:814,817,831,835,842,844,845,861,863,865,3513,3854,3951,4234,8453,8635,846,843,8622)

check(w$reg,reg_vec)


check(w$s1,1:2)
check(w$s1,5:6) # always errors

check(w$s2_1,14:65)

check(w$s2_1,crit(">",13) & crit("<",66))

check(w$s2_2,2:6)

check(select(w,s4_1:s4_99),c(7:9,99))  # always errors
check(select(w,s4_1:s4_99),c(7:9),mult=TRUE)  # always errors
check(select(w,s4_1:s4_99),c(7:9,99),mult=TRUE)

check(select(w,q1_1:q1_99),c(1:7,99),mult=TRUE)
check(select(w,q1_1:q1_99),c(1:7,99),mult=TRUE,uniq = 1:2) # always errors
check(select(w,q1_1:q1_99),c(1:7),mult=TRUE)  # always errors

check(select(w,q5_1:q5_99),c(1:6,98,99),mult=TRUE)   # always errors
check(select(w,q5_1:q5_99),c(1:6,98,99),cond=with(w,!((q4 %in% 2) | (q4 %in% 99))),mult=TRUE,uniq=99)
check(select(w,q8_1:q8_99),c(1:6,99),mult=TRUE,uniq=99)  
check(select(w,q21_1:q21_99),c(6,13,15,18,39,50,58,61,62,66,68,84,86,98,99,126,134,150,155,271),mult=TRUE,uniq=99)  



chk = check_single(checked_data$s1,1:2)
table(chk)
chk = check_single(checked_data$s2_1,14:65)
table(chk)
chk = check_single(checked_data$s2_2,2:6)
table(chk)





checkall.
mult q1_1 to q1_99=1 thru 7 99; uniq= 99.

mult = function(.data=NULL,...,unique=NULL,duplicates=NULL){
    aaa = substitute(list(...))
    aaa
}

mult = function(.data,...){
    res = list(...)
    class(res) = "mult"
    res
}


'==.mult' = function(e1,e2){
    print(e1)
    print("should contain")
    print(e2)
    TRUE
    
}

'&.mult' = function(e1,e2){
    print(e1)
    print("should contain")
    print(e2)
    TRUE
    
}

dfs = dfs %>% mult(a1,a2,a45,duplicates=TRUE) + values() + fix_in_place
dfs = dfs %>% sngl(a1,a2,a45,duplicates=FALSE) +  values() + autofix
dfs = dfs %>% move(a1,a2,a45) +  values() + report


dfs = mult(dfs,a1,a2,a45,cond=) + values() + fix_in_place
dfs = sngl(dfs,a1,a2,a45,filt=) +  values() + autofix
dfs = move(dfs,a1,a2,a45) +  values() + report

dfs = mult(dfs,a1,a2,a45) + cond() + values() + fix_in_place
dfs = sngl(dfs,a1,a2,a45) + filt() + values() + autofix
dfs = move(dfs,a1,a2,a45) + values() + report

brands = values()
dfs = mult(dfs,a1,a2,a45,checkif=) + brands + fix_in_place
dfs = sngl(dfs,a1,a2,a45,filt=) +  brands + autofix
dfs = move(dfs,a1,a2,a45) +  brands  

brands = values(val_lab(dfs$a1))

'mult<-' = function(x,...,checkif=NULL,value){
    print(x)
    print("dots")
    print(substitute(list(...)))
    print("checkif")
    print(substitute(checkif))
    print("value")
    print(value)
    x
    
}

# мы не передаем проверки атрибутами
# короткая запись
# только одно присваивание
# # хотелось бы один набор использовать и для множественных и для синглов
check(dfs,a1,a2,a45,checkif=123) = brands + fix_in_place
check(dfs,a1,a2,a45,filt=145) = mult(1,2,3,4,unique=c(5:7)) + duplicates + silent + autofix 
check(dfs,a1,a2,a45,filt=145) = sngl(1,2,3,4,unique=c(5:7)) + duplicates + silent + autofix
move(dfs,a1,a2,a45) = brands  

sngl(dfs,a1,a2,a45,checkif=123) = brands + fix_in_place
mult(dfs,a1,a2,a45,filt=145) = values(1,2,3,4,unique=c(5:7)) + duplicates + silent + autofix 
sngl(dfs,a1,a2,a45,filt=145) = values(1,2,3,4,unique=c(5:7)) + duplicates + silent + autofix

# и новый более оригинальный способ

mult(dfs,a1,a2,a45,filt=145)(1,2,3,4,uniq=5:7) -> autofix(w)
sngl(dfs,a1,a2,a45,chekif=145)(1,2,3,4,uniq=5:7) -> fix_in_place(w)
sngl(dfs,a1,a2,a45,filt=145)(1,2,3,4,uniq=5:7) = autofix()
sngl(dfs,a1,a2,a45,filt=145)(1,2,3,4,uniq=5:7) = fix_in_place

# а скорее всего будет

dfs %<>% mult(a1,a2,a45,filt=145)(1,2,3,4,uniq=5:7)  %>% autofix
dfs %<>% sngl(a1,a2,a45,chekif=145)(1,2,3,4,uniq=5:7)  %>% fix_in_place
dfs %<>% sngl(a1,a2,a45,filt=145)(1,2,3,4,uniq=5:7)  %>% autofix
dfs %<>% sngl(a1,a2,a45,filt=145)(1,2,3,4,uniq=5:7)  %>% dialog


"mult" = function(a1){
    a1 = a1*2    
    "loc<-" = function(x,value){
        (a1+x)*value
    }
    `loc<-`
    
}

a2 = 3

mult(a2)(3) = 5



chk(dfs)  = mult(a1,a2,a45)(1,2,3,4,uniq=5:7)
chk(dfs,cond=a1>5)  = mult(a1,a2,a45)(1,2,3,4,uniq=5:7)
chk(dfs,filt=a1>5)  = sngl(a1,a2,a45,no_dup = TRUE)(1,2,3,4,uniq=5:7)


check_single(iris)


ddd = function(dfs,cond){
    
    eval(substitute(expr), data, enclos = parent.frame())
}





