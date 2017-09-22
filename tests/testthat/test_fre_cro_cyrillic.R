context("fre and cro from real life")

data = readRDS("rds/data.rds")

data$reg[1:10] = NA
q8 = sprintf("q8r_%s", c(1:12, 99))
data[1:10, q8] = NA
var_lab(data$q8r_1) = "Используемые услуги"
expect_equal_to_reference(fre(data$reg), "rds/fre_real1.rds")
expect_equal_to_reference(fre(data$s1), "rds/fre_real2.rds")
expect_equal_to_reference(with(data, fre(q8r_1 %to% q8r_99)), "rds/fre_real3.rds")
expect_equal_to_reference(with(data, fre(as.dichotomy(q8r_1 %to% q8r_99))), "rds/fre_real3.rds")


expect_equal_to_reference(cro(data$reg, data$s1), "rds/cro_real1.rds")


expect_equal_to_reference(cro_cpct(data$reg, data$s1), "rds/cro_real2.rds")
expect_equal_to_reference(with(data, cro(mrset(q8r_1 %to% q8r_99), reg)), "rds/cro_real3.rds")
expect_identical(with(data, cro(as.dichotomy(q8r_1 %to% q8r_99, keep_unused = TRUE), reg)),
                 with(data, cro(mrset(q8r_1 %to% q8r_99), reg))
)
expect_equal_to_reference(with(data, cro_cpct(mrset(q8r_1 %to% q8r_99), reg)), "rds/cro_real4.rds")
expect_equal_to_reference(with(data, cro_rpct(mrset(q8r_1 %to% q8r_99), reg)), "rds/cro_real4r.rds")
expect_equal_to_reference(with(data, cro_tpct(mrset(q8r_1 %to% q8r_99), reg)), "rds/cro_real4t.rds")

expect_equal_to_reference(with(data, cro(mrset(q8r_1 %to% q8r_99), s1)), "rds/cro_real5.rds")
expect_equal_to_reference(with(data, cro_cpct(mrset(q8r_1 %to% q8r_99), s1)), "rds/cro_real6.rds")

#### with weight
expect_equal_to_reference(fre(data$reg, weight = data$weight1), "rds/fre_real1w.rds")
expect_equal_to_reference(fre(data$s1, weight = data$weight1), "rds/fre_real2w.rds")
expect_equal_to_reference(with(data, fre(q8r_1 %to% q8r_99, weight = weight1)), "rds/fre_real3w.rds")

expect_equal_to_reference(
    fre(with(data, list(reg, s1, q8r_1 %to% q8r_99)), weight = data$weight1)
    , "rds/fre_real_list.rds")

expect_identical(
    fre(with(data, list(as.dichotomy(reg, keep_unused = TRUE), 
                        as.dichotomy(s1, keep_unused = TRUE), 
                        as.dichotomy(q8r_1 %to% q8r_99, keep_unused = TRUE))), 
        weight = data$weight1)
    ,     fre(with(data, list(reg, 
                              s1, 
                              q8r_1 %to% q8r_99)), 
              weight = data$weight1))

expect_equal_to_reference(cro(data$reg, data$s1, weight = data$weight1), "rds/cro_real1w.rds")
expect_equal_to_reference(cro(data$reg, as.data.frame(data$s1), weight = data$weight1), "rds/cro_real1w.rds")
expect_equal_to_reference(cro_cpct(data$reg, data$s1, weight = data$weight1), "rds/cro_real2w.rds")
expect_equal_to_reference(with(data, cro(mrset(q8r_1 %to% q8r_99), reg, weight = weight1)), "rds/cro_real3w.rds")
expect_equal_to_reference(with(data, cro(mrset(q8r_1 %to% q8r_99), list(reg, total()), 
                                         weight = weight1, 
                                         total_statistic = "w_cases")), 
                          "rds/cro_real3ww.rds")

## here
expect_equal_to_reference(with(data, cro_cpct(mrset(q8r_1 %to% q8r_99), reg, weight = weight1)),
                          "rds/cro_real4w.rds")
expect_equal_to_reference(with(data, cro(mrset(q8r_1 %to% q8r_99), s1, weight = weight1)),
                          "rds/cro_real5w.rds")
expect_equal_to_reference(with(data, cro_cpct(mrset(q8r_1 %to% q8r_99), s1, weight = weight1)),
                          "rds/cro_real6w.rds")
expect_equal_to_reference(with(data, cro_rpct(mrset(q8r_1 %to% q8r_99), s1, weight = weight1)), 
                          "rds/cro_real6wr.rds")
expect_identical(with(data, cro_rpct(as.dichotomy(q8r_1 %to% q8r_99, keep_unused = TRUE),
                                     s1, weight = weight1)), 
                 with(data, cro_rpct(mrset(q8r_1 %to% q8r_99), s1, weight = weight1)))
expect_equal_to_reference(with(data, cro_tpct(mrset(q8r_1 %to% q8r_99), s1, weight = weight1)), 
                          "rds/cro_real6wt.rds")
expect_identical(with(data, cro_tpct(as.dichotomy(q8r_1 %to% q8r_99, keep_unused = TRUE),
                                     s1, 
                                     weight = weight1)),
                 with(data, cro_tpct(mrset(q8r_1 %to% q8r_99), s1, weight = weight1)))


context("experimental cyrillic")
data = readRDS("rds/data.rds")

default_dataset(data)

.compute({
    reg[1:10] = NA
})

expect_error(.recode(q8_1 %to% q8_99, (1:NROW(data)<11) ~ NA))
.recode(q8r_1 %to% q8r_99, (1:NROW(data)<11) ~ NA)
# .if_val(q8r_1 %to% q8r_99, (1:NROW(data)<11) ~ NA)

.apply_var_labs(
    q8r_1 = "Используемые услуги"    
)
expect_equal_to_reference(.fre(reg), "rds/fre_real1.rds")
expect_equal_to_reference(.fre(s1), "rds/fre_real2.rds")
expect_equal_to_reference(.fre(q8r_1 %to% q8r_99), "rds/fre_real3.rds")


expect_equal_to_reference(.cro(reg, s1), "rds/cro_real1.rds")
expect_equal_to_reference(.cro_cpct(reg, s1), "rds/cro_real2.rds")
# expect_equal_to_reference(.cro_tpct(mrset(q8r_1 %to% q8r_99), reg), "rds/cro_real4t.rds")

#### with weight
expect_equal_to_reference(.fre(reg, weight = weight1), "rds/fre_real1w.rds")
expect_equal_to_reference(.cro_rpct(mrset(q8r_1 %to% q8r_99), s1, weight = weight1), "rds/cro_real6wr.rds")

