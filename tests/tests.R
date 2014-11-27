load("tests/checked_data.RData")
load("tests/raw_data.RData")

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


## countif checks

df1 = data.frame(
    a=c("apples",   "oranges",     "peaches",	 "apples"),
    b = c(32, 54, 75, 86)
)

countif("apples",df1$a) == 2
countif("apples",df1) == 2
with(df1,countif("apples",a,b)) == 2
countif(crit(">",55),df1$b) == 2
countif(crit("!=",75),df1$b) == 3
countif(crit(">=",32),df1$b) == 4


Formula	Description
=COUNTIF(A2:A5,"apples")	Number of cells with apples in cells A2 through A5.
=COUNTIF(A2:A5,A4)	Number of cells with peaches in cells A2 through A5.
=COUNTIF(A2:A5,A3)+COUNTIF(A2:A5,A2)	Number of cells with oranges and apples in cells A2 through A5.
=COUNTIF(B2:B5,">55")	Number of cells with a value greater than 55 in cells B2 through B5.
=COUNTIF(B2:B5,"<>"&B4)	Number of cells with a value not equal to 75 in cells B2 through B5.
=COUNTIF(B2:B5,">=32")-COUNTIF(B2:B5,">85")	Number of cells with a value greater than or equal to 32 and less than or equal to 85 in cells B2 through B5.


