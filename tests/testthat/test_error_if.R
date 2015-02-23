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


checked_vars = c("a3","a22","b3","b23")

# there is one error in a22

for (each_var in checked_vars){
    expect_equal_to_reference(ProductTestRaw  %>% error_if(!(ProductTestRaw[,each_var] %in% 1:7),show=c(id,cell)),sprintf("error_if_%s.rds",each_var))  
}


context("error_if default_dataset")
data(ProductTestRaw)

default_dataset(ProductTestRaw)
expect_equal_to_reference(error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% ref,"error_if1.rds"  )
expect_equal_to_reference(error_if((s2a %in% 18:26) & !(s2b %in% 2),show=c(id,cell))   %>% ref,"error_if2.rds"  )
expect_equal_to_reference(error_if((s2a %in% 27:35) & !(s2b %in% 3))  %>% ref,"error_if3.rds"  )

expect_equal_to_reference(check_if(cell==1)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% ref,"error_if4.rds"  )
expect_equal_to_reference(check_if(cell==2)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% ref ,"error_if5.rds")

expect_equal_to_reference(check_subset(cell==1)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% ref,"error_if6.rds")  
expect_equal_to_reference(check_subset(cell==2)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% ref ,"error_if7.rds") 

checked_vars = c("a3","a22","b3","b23")

# there is one error in a22

for (each_var in checked_vars){
    expect_equal_to_reference(error_if(!(ProductTestRaw[,each_var] %in% 1:7),show=c(id,cell))  %>% ref,sprintf("error_if_%s.rds",each_var))  
}


context("error_if default_dataset with comma")
data(ProductTestRaw)

default_dataset(ProductTestRaw)
expect_equal_to_reference(error_if(,(s2a %in% 18:26) & !(s2b %in% 2))  %>% ref,"error_if1.rds"  )
expect_equal_to_reference(error_if(,(s2a %in% 18:26) & !(s2b %in% 2),show=c(id,cell))   %>% ref,"error_if2.rds"  )
expect_equal_to_reference(error_if(,(s2a %in% 27:35) & !(s2b %in% 3))  %>% ref,"error_if3.rds"  )

expect_equal_to_reference(check_if(,cell==1)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% ref,"error_if4.rds"  )
expect_equal_to_reference(check_if(,cell==2)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% ref ,"error_if5.rds")

expect_equal_to_reference(check_subset(,cell==1)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% ref,"error_if6.rds")  
expect_equal_to_reference(check_subset(,cell==2)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% ref ,"error_if7.rds") 

checked_vars = c("a3","a22","b3","b23")

# there is one error in a22

for (each_var in checked_vars){
    expect_equal_to_reference(error_if(,!(ProductTestRaw[,each_var] %in% 1:7),show=c(id,cell))  %>% ref,sprintf("error_if_%s.rds",each_var))  
}