context("error_if")

library(dplyr)
data(ProductTestRaw)

expect_equal_to_reference(ProductTestRaw  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2)),"error_if1.rds"  )
expect_equal_to_reference(ProductTestRaw  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2),show=c(id,cell)),"error_if2.rds"  )
expect_equal_to_reference(ProductTestRaw  %>% error_if((s2a %in% 27:35) & !(s2b %in% 3)),"error_if3.rds"  )

expect_equal_to_reference(ProductTestRaw  %>% check_if(cell==1)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2)),"error_if4.rds"  )
expect_equal_to_reference(ProductTestRaw  %>% check_if(cell==2)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2)) ,"error_if5.rds")

expect_equal_to_reference(ProductTestRaw  %>% check_subset(cell==1)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2)),"error_if6.rds")  
expect_equal_to_reference(ProductTestRaw  %>% check_subset(cell==2)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2)) ,"error_if7.rds") 
