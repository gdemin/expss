prop_pvalue = function(prop1, prop2, base1, base2, common_base = 0){
    # ftp://public.dhe.ibm.com/software/analytics/spss/documentation/statistics/20.0/en/client/Manuals/IBM_SPSS_Statistics_Algorithms.pdf
    # IBM SPSS Statistics Algorithms v20, p. 263
    base1 = round(base1)
    base2 = round(base2)
    pooled_prop = (prop1*base1 + prop2*base2)/(base1 + base2)
    z_statistic = (prop1 - prop2)/
        sqrt(pooled_prop*(1 - pooled_prop)*(1/base1 + 1/base2 - 2*common_base/base1/base2))
    2*(1 - pnorm(abs(z_statistic)))
} 


