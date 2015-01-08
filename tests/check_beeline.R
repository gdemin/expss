load("tests/checked_data.RData")
load("tests/raw_data.RData")
always(raw_data) = c("reg","nn")

w = checked_data
w = raw_data
reg_vec = c(301,342,343,345,351,353,381:385,395,421,423,473,483:485,487,492,495:497,812:814,817,831,835,842,844,845,861,863,865,3513,3854,3951,4234,8453,8635,846,843,8622)

action = invisible
action = report

w  %>% sngl(reg)(reg_vec)  %>% action
w  %>% sngl(s1)(1:2)  %>% action
w  %>% sngl(s2_1)(14:65)  %>% action
w  %>% sngl(s2_1)(crit(">",13) & crit("<",66))  %>% action

w  %>% sngl(s2_2)(2:6)  %>% action
w  %>% sngl(s3)(1:3)  %>% action
w  %>% mult(s4_1:s4_99)(7:9,exclusive = 99)  %>% action
w  %>% sngl(s6)(3:5)  %>% action
w  %>% mult(q1_1:q1_99)(1:7,exclusive = 99)  %>% action

w %<>% mutate(
    cntq1_12399 = row_countif(c(1:3,99),q1_1,q1_2,q1_3,q1_4,q1_5,q1_6,q1_7,q1_99),
    cntq1else = row_countif(c(4:7),q1_1,q1_2,q1_3,q1_4,q1_5,q1_6,q1_7,q1_99),
    cond1 = cntq1_12399>0
    )


w  %>% sngl(q3,cond=cntq1else==0)(1,2,99)  %>% action
w  %>% sngl(q4)(1,2,99)  %>% action
w  %>% mult(q5_1:q5_99,cond=q4!=2 & q4!=99)(1:6,98,exclusive = 99)  %>% action

cond = row_countif(select(w,q5_1:q5_99),98)>0 & (w$q4!=2 & w$q4!=99)
w  %>% sngl(q5_dr1_1,cond=cond)(10,13)  %>% action
w  %>% sngl(q6,cond=q4==1)(1,2,99)  %>% action
w  %>% sngl(q7,cond=q4==2 | q4==99)(1,2,99)  %>% action
w  %>% mult(q8_1:q8_99,cond=(q4==2 | q4==99) & (q7!=2 & q7!=99) )(1:6,exclusive=99)  %>% action


# checkif (any(98,q5_1 to q5_99)) & (q4~=2 & q4~=99).
# sngl q5_dr1_1	= 10 13.

checkif q4=2 | q4=99.
sngl q7=1 2 99.
checkif (q4=2 | q4=99) & (q7~=2 & q7~=99).
mult q8_1 to q8_99=1 thru 6  99; uniq= 99.



check(w$s2_1,crit(">",13) & crit("<",66))



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
