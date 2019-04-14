context("significance_cell_chisq")

test_table = text_to_columns("
row_labels       Total Segment|PRO	Segment|AMT	Segment|DES
#Total	384	128	180	76
ADIDAS	112	92	16	4
NIKE	102	18	76	8
ASICS	67	3	56	8
MIZUNO	47	7	24	16
NEWBALANCE	56	8	8	40
                ",  comment.char = "", check.names = FALSE)

test_table[,-1] = lapply(test_table[,-1], function(x) c(x[1], x[-1]/x[1]*100)) 
test_table = as.etable(test_table)
expss_digits(2)
res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128  ", "71.88 >", "14.06 <", 
" 2.34 <", " 5.47 <", " 6.25 <"), `Segment|AMT` = c("180  ", 
" 8.89 <", "42.22 >", "31.11 >", "13.33  ", " 4.44 <"), `Segment|DES` = c("76  ", 
" 5.26 <", "10.53 <", "10.53  ", "21.05 >", "52.63 >")), row.names = c(NA, 
-6L), class = c("etable", "data.frame"))
expect_identical(significance_cell_chisq(test_table), res)
expect_identical(significance_cell_chisq(test_table, row_margin = "auto"), res)
expect_identical(significance_cell_chisq(test_table, row_margin = "sum_row"), res)

res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128  ", "71.88 +", "14.06 -", 
" 2.34 -", " 5.47 -", " 6.25 -"), `Segment|AMT` = c("180  ", 
" 8.89 -", "42.22 +", "31.11 +", "13.33  ", " 4.44 -"), `Segment|DES` = c("76  ", 
" 5.26 -", "10.53 -", "10.53  ", "21.05 +", "52.63 +")), row.names = c(NA, 
 -6L), class = c("etable", "data.frame"))
expect_identical(significance_cell_chisq(test_table, sig_labels_chisq = c("-", "+")), res)
expect_identical(significance_cell_chisq(test_table, sig_labels_chisq = c("-", "+"), subtable_marks = "both"), res)

res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128  ", "71.88 >", "14.06  ", 
" 2.34  ", " 5.47  ", " 6.25  "), `Segment|AMT` = c("180  ", 
" 8.89  ", "42.22 >", "31.11 >", "13.33  ", " 4.44  "), `Segment|DES` = c("76  ", 
" 5.26  ", "10.53  ", "10.53  ", "21.05 >", "52.63 >")), row.names = c(NA, 
-6L), class = c("etable", "data.frame"))
expect_identical(significance_cell_chisq(test_table, subtable_marks = "greater"), res)
res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128  ", "71.88  ", "14.06 <", 
" 2.34 <", " 5.47 <", " 6.25 <"), `Segment|AMT` = c("180  ", 
" 8.89 <", "42.22  ", "31.11  ", "13.33  ", " 4.44 <"), `Segment|DES` = c("76  ", 
" 5.26 <", "10.53 <", "10.53  ", "21.05  ", "52.63  ")), row.names = c(NA, 
 -6L), class = c("etable", "data.frame"))
expect_identical(significance_cell_chisq(test_table, subtable_marks = "less"), res)
res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128  ", "71.88 >", "14.06 <", 
" 2.34 <", " 5.47 <", " 6.25 <"), `Segment|AMT` = c("180  ", 
" 8.89 <", "42.22 >", "31.11 >", "13.33  ", " 4.44 <"), `Segment|DES` = c("76", 
" 5.26", "10.53", "10.53", "21.05", "52.63")), row.names = c(NA, 
-6L), class = c("etable", "data.frame"))
expect_identical(significance_cell_chisq(test_table, min_base = 100), res)
res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128", "71.88", "14.06", 
" 2.34", " 5.47", " 6.25"), `Segment|AMT` = c("180  ", " 8.89 <", 
"42.22 >", "31.11 >", "13.33  ", " 4.44 <"), `Segment|DES` = c("76", 
" 5.26", "10.53", "10.53", "21.05", "52.63")), row.names = c(NA, 
-6L), class = c("etable", "data.frame"))
expect_identical(significance_cell_chisq(test_table, min_base = 150), res)
res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128", "71.88", "14.06", 
" 2.34", " 5.47", " 6.25"), `Segment|AMT` = c("180", " 8.89", 
"42.22", "31.11", "13.33", " 4.44"), `Segment|DES` = c("76", 
" 5.26", "10.53", "10.53", "21.05", "52.63")), row.names = c(NA, 
-6L), class = c("etable", "data.frame"))
expect_identical(significance_cell_chisq(test_table, min_base = 400), res)

test_table = text_to_columns("
row_labels       Total PRO	AMT	DES
#Total	384	128	180	76
ADIDAS	112	92	16	4
NIKE	102	18	76	8
ASICS	67	3	56	8
MIZUNO	47	7	24	16
NEWBALANCE	56	8	8	40
                ",  comment.char = "", check.names = FALSE) %>% as.etable()
test_table[,-1] = lapply(test_table[,-1], function(x) c(x[1], x[-1]/x[1]*100)) 


res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128  ", "71.88 >", "14.06 <", 
" 2.34 <", " 5.47 <", " 6.25 <"), `Segment|AMT` = c("180  ", 
" 8.89 <", "42.22 >", "31.11 >", "13.33  ", " 4.44 <"), `Segment|DES` = c("76  ", 
" 5.26 <", "10.53 <", "10.53  ", "21.05 >", "52.63 >")), row.names = c(NA, 
-6L), class = c("etable", "data.frame"))
colnames(res) = gsub(".+\\|", "", colnames(res), perl = TRUE)
expect_identical(significance_cell_chisq(test_table, row_margin = "first_column"), res)
expect_identical(significance_cell_chisq(test_table, total_column_marker = "Total"), res)
colnames(res)[2] = "#Total"
colnames(test_table)[2] = "#Total"
expect_identical(significance_cell_chisq(test_table, row_margin = "auto"), res)

res = structure(list(row_labels = c("ADIDAS", "NIKE", "ASICS", "MIZUNO", 
"NEWBALANCE"), `#Total` = c("", "", "", "", ""), PRO = c(">", 
"<", "<", "<", "<"), AMT = c("<", ">", ">", "", "<"), DES = c("<", 
"<", "", ">", ">")), row.names = 2:6, class = c("etable", "data.frame"
))
expect_identical(significance_cell_chisq(test_table, row_margin = "auto", keep = "none"), res)

res = structure(list(row_labels = c("ADIDAS", "NIKE", "ASICS", "MIZUNO", 
"NEWBALANCE"), `#Total` = c("29.17", "26.56", "17.45", "12.24", 
"14.58"), PRO = c("71.88 >", "14.06 <", " 2.34 <", " 5.47 <", 
" 6.25 <"), AMT = c(" 8.89 <", "42.22 >", "31.11 >", "13.33  ", 
" 4.44 <"), DES = c(" 5.26 <", "10.53 <", "10.53  ", "21.05 >", 
"52.63 >")), row.names = 2:6, class = c("etable", "data.frame"
))

expect_identical(significance_cell_chisq(test_table, row_margin = "auto", keep = "percent"), res)
expss_digits()
expect_identical(significance_cell_chisq(test_table, row_margin = "auto", keep = "percent", digits = 2), res)


res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), `#Total` = c("384", "", "", "", "", 
""), PRO = c("128", ">", "<", "<", "<", "<"), AMT = c("180", 
"<", ">", ">", "", "<"), DES = c("76", "<", "<", "", ">", ">"
)), row.names = c(NA, -6L), class = c("etable", "data.frame"))
expect_identical(significance_cell_chisq(test_table, row_margin = "auto", keep = "bases"), res)

expect_identical(significance_cell_chisq(test_table[,1]), test_table[,1])

context("cell_chisq correct")


test_table = text_to_columns("
row_labels       Total Segment|PRO	Segment|AMT	Segment|DES
#Total	384	128	180	76
ADIDAS	112	92	16	4
NIKE	102	18	76	8
ASICS	67	3	56	8
MIZUNO	47	7	24	16
NEWBALANCE	56	8	8	40
                ",  comment.char = "", check.names = FALSE)



test_table[,-1] = lapply(test_table[,-1], function(x) c(x[1], x[-1]/x[1]*100)) 
test_table = as.etable(test_table)
expss_digits(2)
res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128  ", "71.88 >", "14.06 <", 
" 2.34 <", " 5.47 <", " 6.25 <"), `Segment|AMT` = c("180  ", 
" 8.89 <", "42.22 >", "31.11 >", "13.33  ", " 4.44 <"), `Segment|DES` = c("76  ", 
" 5.26 <", "10.53 <", "10.53  ", "21.05  ", "52.63 >")), row.names = c(NA, 
-6L), class = c("etable", "data.frame"))

expect_identical(
    significance_cell_chisq(test_table, sig_level = 0.01, correct = TRUE),
    res
)

res = structure(list(row_labels = c("#Total", "ADIDAS", "NIKE", "ASICS", 
"MIZUNO", "NEWBALANCE"), Total = c("384", "29.17", "26.56", "17.45", 
"12.24", "14.58"), `Segment|PRO` = c("128  ", "71.88 >", "14.06 <", 
" 2.34 <", " 5.47 <", " 6.25 <"), `Segment|AMT` = c("180  ", 
" 8.89 <", "42.22 >", "31.11 >", "13.33  ", " 4.44 <"), `Segment|DES` = c("76  ", 
" 5.26 <", "10.53 <", "10.53  ", "21.05 >", "52.63 >")), row.names = c(NA, 
-6L), class = c("etable", "data.frame"))
expect_identical(
    significance_cell_chisq(test_table, sig_level = 0.01, correct = FALSE),
    res
)

expss_digits()

# data(mtcars)
# mtcars = apply_labels(mtcars,
#                       mpg = "Miles/(US) gallon",
#                       cyl = "Number of cylinders",
#                       disp = "Displacement (cu.in.)",
#                       hp = "Gross horsepower",
#                       drat = "Rear axle ratio",
#                       wt = "Weight (lb/1000)",
#                       qsec = "1/4 mile time",
#                       vs = "Engine",
#                       vs = c("V-engine" = 0,
#                              "Straight engine" = 1),
#                       am = "Transmission",
#                       am = c("Automatic" = 0,
#                              "Manual"=1),
#                       gear = "Number of forward gears",
#                       carb = "Number of carburetors"
# )
