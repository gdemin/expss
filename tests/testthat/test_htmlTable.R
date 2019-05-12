if(isTRUE(getOption("covr"))){ 

  context("htmlTable")
  
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
  
  expect_equal_to_reference(htmlTable(mtcars_table),
                            "rds/htmlTable1.rds",  update = FALSE)
  
  expect_equal_to_reference(htmlTable(set_caption(mtcars_table, "mtcars_table")),
                            "rds/htmlTable1_caption.rds",  update = FALSE)
  
  
  expect_equal_to_reference(htmlTable(
    list(
      set_caption(mtcars_table, "mtcars_table"),
      mtcars_table,
      set_caption(mtcars_table, "mtcars_table2")
    )
  ),                    "rds/htmlTable1_list.rds",  update = FALSE)
  
  expect_equal_to_reference(suppressWarnings(htmlTable(mtcars_table[FALSE, ])) , 
                            "rds/htmlTable2.rds",  update = FALSE)
  expect_equal_to_reference(htmlTable(mtcars_table[, 1])  ,  
                            "rds/htmlTable2single1.rds",  update = FALSE)
  
  
  expect_equal_to_reference(htmlTable(mtcars_table[, FALSE, drop = FALSE])  , 
                            "rds/htmlTable2empty1.rds",  update = FALSE)
  
  
  expect_equal_to_reference(htmlTable(mtcars_table, digits = 0) ,
                            "rds/htmlTable3.rds",  update = FALSE)
  
  expect_equal_to_reference(htmlTable(mtcars_table, digits = 1, row_groups = FALSE) ,
                            "rds/htmlTable3_no_rowgroups.rds",  update = FALSE)
  
  expect_equal_to_reference(htmlTable(set_caption(mtcars_table, "My caption"), digits = 1, row_groups = FALSE) ,
                            "rds/htmlTable3_no_rowgroups_caption.rds",  update = FALSE)
  
  expect_equal_to_reference(expss:::repr_html.with_caption(set_caption(mtcars_table, "My caption"), digits = 1) ,
                            "rds/htmlTable3_no_rowgroups_caption.rds",  update = FALSE)
  
  expect_equal_to_reference(htmlTable(
    list(
      set_caption(mtcars_table, "mtcars_table"),
      mtcars_table,
      set_caption(mtcars_table, "mtcars_table2")
    ),
    digits = 0, row_groups = FALSE),                    
    "rds/htmlTable1_list_norogroups.rds",  update = FALSE)
  
  expect_equal_to_reference(expss:::repr_html.etable(mtcars_table, digits = 1),
                            "rds/htmlTable3_no_rowgroups.rds",  update = FALSE)
  
  mtcars_table = calculate(mtcars,
                           cro_mean(list(mpg, hp), list(am %nest% vs)) )
  expect_equal_to_reference(htmlTable(mtcars_table) ,
                            "rds/htmlTable10.rds",  update = FALSE)
  
  mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs, "#Total"))
  expect_equal_to_reference(htmlTable(mtcars_table) ,
                            "rds/htmlTable11.rds",  update = FALSE)
  
  mtcars_table = cro_cpct(list(unvr(mtcars$vs)), list(mtcars$vs %nest% mtcars$am, "#Total"))
  expect_equal_to_reference(htmlTable(mtcars_table) ,
                            "rds/htmlTable12.rds",  update = FALSE)
  
  expect_equal_to_reference(htmlTable(mtcars_table, digits = 1, row_groups = FALSE) ,
                            "rds/htmlTable12_no_rowgroups.rds",  update = FALSE)
  
  
  expect_equal_to_reference(htmlTable(mtcars_table[,1]) ,    #####
                            "rds/htmlTable12single.rds",  update = FALSE)
  
  colnames(mtcars_table)[1] = "My table"
  
  expect_equal_to_reference(htmlTable(mtcars_table[,1]) ,    #####
                            "rds/htmlTable12single2.rds",  update = FALSE)
  
  new_am = mtcars$am
  mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total")) %merge%
    cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"))
  
  expect_equal_to_reference(htmlTable(mtcars_table) ,
                            "rds/htmlTable13.rds",  update = FALSE)
  
  expect_equal_to_reference(htmlTable(mtcars_table, digits = 1) ,
                            "rds/htmlTable13_1.rds",  update = FALSE)
  
  var_lab(new_am) = "|"
  val_lab(new_am) = setNames(0:1, c("", " "))
  mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total")) %merge%
    cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"))
  colnames(mtcars_table)[7] = ""
  
  expect_equal_to_reference(htmlTable(mtcars_table) ,
                            "rds/htmlTable14.rds",  update = FALSE)
  
  expect_equal_to_reference(htmlTable(mtcars_table, digits = 1) ,
                            "rds/htmlTable14_1.rds",  update = FALSE)
  
  options(expss.digits = 0)
  expect_equal(htmlTable(mtcars_table),
               htmlTable(mtcars_table, digits = 0))
  
  options(expss.digits = 3)
  expect_equal(htmlTable(mtcars_table),
               htmlTable(mtcars_table, digits = 3))
  
  expect_true(!isTRUE(all.equal(htmlTable(mtcars_table),
                                htmlTable(mtcars_table, digits = 4))))
  
  options(expss.digits = NULL)
  expect_equal(htmlTable(mtcars_table),
               htmlTable(mtcars_table, digits = 1))
  
  
  mtcars_table = cro_cpct(list(mtcars$vs), 
                          list(mtcars$vs %nest% mtcars$am, "#Total")) 
  
  expect_equal_to_reference(htmlTable(mtcars_table, align = rep("c", "6")),
                            "rds/htmlTable14_2.rds",  update = FALSE)
  expect_equal_to_reference(htmlTable(mtcars_table, align = rep("c", "6"), row_groups = FALSE),
                            "rds/htmlTable14_3.rds",  update = FALSE)
  
  # simple_table = cro(list(1:5), list(1:5))
  # 
  # expss_output_viewer()
  # htmlTable(simple_table)
  
  data("product_test")
  res = product_test %>%
    tab_cols(c1) %>%
    tab_cells(unvr(mrset(a1_1 %to% a1_6))) %>%
    tab_stat_cpct(label = var_lab(a1_1)) %>%
    tab_cells(unvr(mrset(b1_1 %to% b1_6))) %>%
    tab_stat_cpct(label = var_lab(b1_1)) %>%
    tab_pivot(stat_position = "inside_columns")
  
  ## first row of header with duplicates
  expect_equal_to_reference(htmlTable(res), "rds/htmlTable15.rds",  update = FALSE)
  
  res = product_test %>%
    compute({
      total = 1
      var_lab(total) = "Total"
      val_lab(total) = setNames(1, " ")
    }) %>% 
    tab_cols(total) %>%
    tab_cells(unvr(mrset(a1_1 %to% a1_6))) %>%
    tab_stat_cpct() %>%
    tab_pivot()
  
  # single column header
  expect_equal_to_reference(htmlTable(res), "rds/htmlTable16.rds",  update = FALSE)
  
  # single row header
  res = mtcars %>% calc(cro(am, list(unvr(vs))))
  expect_equal_to_reference(htmlTable(res), "rds/htmlTable17.rds",  update = FALSE)
  
  # single row header
  res = mtcars %>% calc(cro(list(unvr(am)), list(unvr(vs))))
  expect_equal_to_reference(htmlTable(res), "rds/htmlTable18.rds",  update = FALSE)
  
  # temp = function(x) htmlTable:::print.htmlTable(x, useViewer = TRUE)
  
  res = fre(mtcars$cyl)
  expect_equal_to_reference(htmlTable(res), "rds/htmlTable19.rds",  update = FALSE)
  
  
  
  q1 = c(1:5)
  var_lab(q1) = "My label"
  
  expect_silent(
    expss:::repr_html.etable(fre(list(q1)))
  )
  
  
  my_df = as.etable(data.frame(
    row_labels = c("!", "!=", "$", "$<-", "%in%", "$", "(", "<NA>"),
    # row_labels = c("!", "!=",  "%in%",  "(", "*"), 
    a1 = c(1:7, "<NA>"),
    "price, $" = paste0(11:18, "$"),
    stringsAsFactors = FALSE,
    check.names = FALSE))
  
  expect_equal_to_reference(
    htmlTable(my_df, escape.html = TRUE), 
    "rds/htmlTable20.rds",  update = FALSE)
  expect_equal_to_reference(
    htmlTable(my_df, escape.html = FALSE), 
    "rds/htmlTable21.rds",  update = FALSE)
  expect_equal_to_reference(
    htmlTable(my_df), 
    "rds/htmlTable21.rds",  update = FALSE)
  
}