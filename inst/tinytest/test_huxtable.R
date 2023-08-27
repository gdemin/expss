if(FALSE){ # isTRUE(getOption("covr")) 
# ! FIXME FIXME FIXME !!!
  context("huxtable")
  library(huxtable)
  library(expss)
  
  data(mtcars)
  mtcars = apply_labels(mtcars,
                        mpg = "Miles/(US) gallon|Mean",
                        cyl = "Number of cylinders",
                        disp = "Displacement (cu.in.)|Mean",
                        hp = "Gross horsepower|Mean",
                        drat = "Rear axle ratio",
                        wt = "Weight (lb/1000)",
                        qsec = "1/4 mile time|Mean",
                        vs = "Engine",
                        vs = c("V-engine" = 0,
                               "Straight engine" = 1),
                        am = "Transmission",
                        am = c("Automatic" = 0,
                               "Manual"=1),
                        gear = "Number of forward gears",
                        carb = "Number of carburetors"
  )
  
  options(expss.digits = NA)
  mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), 
                          list(mtcars$vs %nest% mtcars$am, "#Total")) 
  
  expect_equal_to_reference(as_hux(mtcars_table),
                            "rds/as_hux1.rds",  update = TRUE)
  
  expect_equal_to_reference(as_hux(set_caption(mtcars_table, "mtcars_table")),
                            "rds/as_hux1_caption.rds",  update = TRUE)
  
  expect_equal_to_reference(suppressWarnings(as_hux(mtcars_table[FALSE, ])) , 
                            "rds/as_hux2.rds",  update = TRUE)
  expect_equal_to_reference(as_hux(mtcars_table[, 1])  ,  
                            "rds/as_hux2single1.rds",  update = TRUE)
  
  
  # expect_equal_to_reference(as_hux(mtcars_table[, FALSE, drop = FALSE])  , 
  #                           "rds/as_hux2empty1.rds",  update = TRUE)
  
  expect_equal_to_reference(as_hux(set_caption(mtcars_table, "My caption")) ,
                            "rds/as_hux3_no_rowgroups_caption.rds",  update = TRUE)
  
    mtcars_table = query(mtcars,
                           cro_mean(list(mpg, hp), list(am %nest% vs)) )
  expect_equal_to_reference(as_hux(mtcars_table) ,
                            "rds/as_hux10.rds",  update = TRUE)
  
  mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs, "#Total"))
  expect_equal_to_reference(as_hux(mtcars_table) ,
                            "rds/as_hux11.rds",  update = TRUE)
  
  mtcars_table = cro_cpct(list(unvr(mtcars$vs)), list(mtcars$vs %nest% mtcars$am, "#Total"))
  expect_equal_to_reference(as_hux(mtcars_table) ,
                            "rds/as_hux12.rds",  update = TRUE)
  

  expect_equal_to_reference(as_hux(mtcars_table[,1]) ,    #####
                            "rds/as_hux12single.rds",  update = TRUE)
  
  colnames(mtcars_table)[1] = "My table"
  
  expect_equal_to_reference(as_hux(mtcars_table[,1]) ,    #####
                            "rds/as_hux12single2.rds",  update = TRUE)
  
  new_am = mtcars$am
  mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total"))  %>% 
    merge(cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total")))
  
  expect_equal_to_reference(as_hux(mtcars_table) ,
                            "rds/as_hux13.rds",  update = TRUE)

  var_lab(new_am) = "|"
  val_lab(new_am) = setNames(0:1, c("", " "))
  mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total"))  %>% 
    merge(cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total")))
  colnames(mtcars_table)[7] = ""
  # FIXME
  # expect_equal_to_reference(as_hux(mtcars_table) ,
  #                           "rds/as_hux14.rds",  update = TRUE)
  
  mtcars_table = cro_cpct(list(mtcars$vs), 
                          list(mtcars$vs %nest% mtcars$am, "#Total")) 
  
  expect_equal_to_reference(as_hux(mtcars_table),
                            "rds/as_hux14_2.rds",  update = TRUE)

  
  # simple_table = cro(list(1:5), list(1:5))
  # 
  # expss_output_viewer()
  # as_hux(simple_table)
  
  data("product_test")
  res = product_test %>%
    tab_cols(c1) %>%
    tab_cells(unvr(mrset(a1_1 %to% a1_6))) %>%
    tab_stat_cpct(label = var_lab(a1_1)) %>%
    tab_cells(unvr(mrset(b1_1 %to% b1_6))) %>%
    tab_stat_cpct(label = var_lab(b1_1)) %>%
    tab_pivot(stat_position = "inside_columns")
  
  ## first row of header with duplicates
  ## FIXME
  # expect_equal_to_reference(as_hux(res), "rds/as_hux15.rds",  update = TRUE)
  
  res = product_test %>%
    let(
      total = 1,
      total = set_var_lab(total, "Total"),
      total = set_val_lab(total, setNames(1, " "))
    ) %>% 
    tab_cols(total) %>%
    tab_cells(unvr(mrset(a1_1 %to% a1_6))) %>%
    tab_stat_cpct() %>%
    tab_pivot()
  
  # single column header
  expect_equal_to_reference(as_hux(res), "rds/as_hux16.rds",  update = TRUE)
  
  # single row header
  res = mtcars %>% query(cro(am, list(unvr(vs))))
  expect_equal_to_reference(as_hux(res), "rds/as_hux17.rds",  update = TRUE)
  
  # single row header
  res = mtcars %>% query(cro(list(unvr(am)), list(unvr(vs))))
  expect_equal_to_reference(as_hux(res), "rds/as_hux18.rds",  update = TRUE)
  
  # temp = function(x) as_hux:::print.as_hux(x, useViewer = TRUE)
  
  res = fre(mtcars$cyl)
  expect_equal_to_reference(as_hux(res), "rds/as_hux19.rds",  update = TRUE)
  
  
  
  q1 = c(1:5)
  var_lab(q1) = "My label"
  
  expect_silent(
    as_hux(fre(list(q1)))
  )
  

  
  
}

