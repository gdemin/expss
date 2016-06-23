context("experimental")
data = readRDS("rds/data.rds")

default_dataset(data)

..compute({
    reg[1:10] = NA
})

expect_error(.recode(q8_1 %to% q8_99, (1:NROW(data)<11) ~ NA))
..recode(q8r_1 %to% q8r_99, (1:NROW(data)<11) ~ NA)

..set_var_lab(q8r_1, "Используемые услуги")
expect_equal_to_reference(.fre(reg), "rds/fre_real1.rds")
expect_equal_to_reference(.fre(s1), "rds/fre_real2.rds")
# expect_equal_to_reference(.fre(q8r_1 %to% q8r_99), "rds/fre_real3.rds")


expect_equal_to_reference(.cro(reg, s1), "rds/cro_real1.rds")
expect_equal_to_reference(.cro_cpct(reg, s1), "rds/cro_real2.rds")
expect_equal_to_reference(.cro_tpct(q8r_1 %to% q8r_99, reg), "rds/cro_real4t.rds")

#### with weight
expect_equal_to_reference(.fre(reg, weight = weight1), "rds/fre_real1w.rds")
expect_equal_to_reference(.cro_rpct(q8r_1 %to% q8r_99, s1, weight = weight1), "rds/cro_real6wr.rds")

