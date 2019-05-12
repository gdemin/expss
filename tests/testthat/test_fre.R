if(isTRUE(getOption("covr"))){ 
  context("elementary_fre")
  suppressWarnings(RNGversion("3.5.0"))
  
  data(mtcars)
  mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0,
                    "Straight engine" = 1)
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0,
                    manual=1)
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
  })
  
  
  
  mtcars$am[1:2] = NA
  
  expect_equal_to_reference(fre(mtcars$am), "rds/fre2.5.rds",  update = FALSE)
  
  
  mtcars$vs[4:5] = NA
  
  
  
  
  
  mtcars$weight = 2
  
  mtcars$vs1 = ifelse(1:32<=16, mtcars$vs, NA)
  mtcars$vs2 = ifelse(1:32>16, mtcars$vs, NA)
  
  
  mtcars$vs1[4:5] = NA
  
  
  
  ############################################################################################
  ############################################################################################
  ############################################################################################
  ############################################################################################
  
  context("etable methods")
  expect_known_value(fre(mtcars$am)[,"Count"], "rds/fre2.6.rds",  update = FALSE)
  expect_known_value(fre(mtcars$am)[2, ], "rds/fre2.7.rds",  update = FALSE)
  
  expect_known_value(fre(mtcars$am)[,"Count"][,1], "rds/fre2.6.rds",  update = FALSE)
  expect_known_value(fre(mtcars$am)[,"Count"][2,], "rds/fre2.8.rds",  update = FALSE)
  
  expect_known_value(fre(mtcars$am)[,"Count", drop = TRUE], "rds/fre2.9.rds",  update = FALSE)
  expect_known_value(fre(mtcars$am)[,"Count", drop = TRUE][3], "rds/fre2.10.rds",  update = FALSE)
  expect_known_value(fre(mtcars$am)[,"Count"][2, , drop = TRUE], "rds/fre2.11.rds",  update = FALSE)
  
  context("fre drop_unused")
  
  a = factor(c("a", "b", "c"), levels = rev(c("a", "b", "c", "d", "e")))
  expect_known_value(fre(a, drop_unused_labels = FALSE, prepend_var_lab = FALSE)
                     ,"rds/fre_new_args1.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = FALSE, prepend_var_lab = TRUE),"rds/fre_new_args2.rds",  update = FALSE)
  var_lab(a) = "My 'a' with labels"
  expect_known_value(fre(a),"rds/fre_new_args3.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = FALSE),"rds/fre_new_args4.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = FALSE, prepend_var_lab = TRUE),"rds/fre_new_args5.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = TRUE, prepend_var_lab = TRUE),"rds/fre_new_args6.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = FALSE, prepend_var_lab = FALSE),"rds/fre_new_args7.rds",  update = FALSE)
  
  a = 3:5
  val_lab(a) = autonum(letters[5:1])
  expect_known_value(fre(a, drop_unused_labels = FALSE, prepend_var_lab = FALSE),"rds/fre_new_args1.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = FALSE, prepend_var_lab = TRUE),"rds/fre_new_args2.rds",  update = FALSE)
  var_lab(a) = "My 'a' with labels"
  expect_known_value(fre(a),"rds/fre_new_args3.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = FALSE),"rds/fre_new_args4.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = FALSE, prepend_var_lab = TRUE),"rds/fre_new_args5.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = TRUE, prepend_var_lab = TRUE),"rds/fre_new_args6.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = FALSE, prepend_var_lab = FALSE),"rds/fre_new_args7.rds",  update = FALSE)
  
  context("fre and cro examples")
  
  a = factor(c("a", "b", "c"), levels = rev(c("a", "b", "c")))
  expect_known_value(fre(a), "rds/order_factor_fre20.rds",  update = FALSE)
  expect_known_value(cro(a, list(a, total())), "rds/order_factor_cro20a.rds",  update = FALSE)
  expect_known_value(cro(list(a), list(a, total())), "rds/order_factor_cro20.rds",  update = FALSE)
  expect_known_value(cro(list(a), list(a, total()), total_label = "BASE"), "rds/order_factor_cro21.rds",  update = FALSE)
  expect_known_value(
    cro(list(a), 
        list(a, total()),
        weight = 2,
        total_label = "BASE"), 
    "rds/order_factor_cro22.rds",  update = FALSE)
  expect_known_value(
    cro(list(a), 
        list(a, total()),
        weight = 2,
        total_statistic = "w_cases",
        total_label = "BASE"), 
    "rds/order_factor_cro23.rds",  update = FALSE)
  
  expect_known_value(
    cro(list(a), 
        list(a, total()),
        weight = 2,
        total_statistic = c("u_cases", "w_cases"),
        total_label = "BASE"), 
    "rds/order_factor_cro24.rds",  update = FALSE)
  
  expect_known_value(
    cro(list(a), 
        list(a, total()),
        weight = 2,
        total_statistic = c("u_cases", "w_cases"),
        total_label = c("BASE", "W_BASE")), 
    "rds/order_factor_cro25.rds",  update = FALSE)
  
  expect_known_value(
    cro(list(a), 
        list(a, total()),
        weight = 2,
        total_statistic = c("u_cases", "w_cases"),
        total_label = c("BASE", "W_BASE"), 
        subgroup = a=="a"),
    "rds/order_factor_cro26.rds",  update = FALSE)
  
  expect_known_value(
    cro(list(a), 
        list(a, total()),
        weight = 2,
        total_statistic = c("u_cases", "w_cases"),
        total_label = c("BASE", "W_BASE"), 
        subgroup = FALSE),
    "rds/order_factor_cro27.rds",  update = FALSE)
  
  expect_known_value(
    cro(list(a), 
        list(a, total()),
        weight = 2,
        total_statistic = c("w_cases", "u_cases"),
        total_label = c("W_BASE", "BASE")),
    "rds/order_factor_cro28.rds",  update = FALSE)
  
  expect_known_value(
    drop_rc(cro(list(a), 
                list(a, total()),
                weight = 2,
                total_statistic = c("u_cases", "w_cases"),
                total_label = c("BASE", "W_BASE"), 
                subgroup = a=="a")),
    "rds/order_factor_cro29.rds",  update = FALSE)
  
  expect_known_value(
    drop_rc(cro(list(a), 
                list(a, total()),
                weight = 2,
                total_statistic = c("u_cases", "w_cases"),
                total_label = c("BASE", "W_BASE"),
                subgroup = FALSE)),
    "rds/order_factor_cro30.rds",  update = FALSE)
  
  expect_known_value(cro(list(a)), "rds/order_factor_cro31.rds",  update = FALSE)
  
  
  
  # data(mtcars)
  mtcars = modify(mtcars,{
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0, 
                    manual=1)
  })
  
  expect_known_value(fre(mtcars$vs), "rds/fre_ex1.rds",  update = FALSE)
  expect_known_value(fre(mtcars$vs, weight = 1), "rds/fre_ex1.rds",  update = FALSE)
  expect_known_value(with(mtcars, cro(am, vs)), "rds/fre_ex2.rds",  update = FALSE)
  expect_known_value(with(mtcars, cro(am, vs, weight = 1)), "rds/fre_ex2.rds",  update = FALSE)
  expect_known_value(with(mtcars, cro_cpct(am, list(vs, total()))), "rds/fre_ex3.rds",  update = FALSE)
  expect_known_value(with(mtcars, cro_cpct(am, list(vs, total())))[, '#Total'], "rds/fre_ex3.1.rds",  update = FALSE)
  expect_known_value(with(mtcars, cro_cpct(am, list(vs, total())))[3, ], 
                     "rds/fre_ex3.2.rds",  update = FALSE)
  expect_known_value(with(mtcars, cro_cpct(am, list(vs, total())))[['#Total']],
                     "rds/fre_ex3.3.rds",  update = FALSE)
  
  expect_identical(fre(list(mtcars$vs, mtcars$am)), 
                   add_rows(fre(mtcars$vs, prepend_var_lab = TRUE), 
                            fre(mtcars$am, prepend_var_lab = TRUE))
  )
  
  double_fre = fre(list(mtcars$vs, mtcars$am))
  expect_known_value( split_columns(double_fre), "rds/fre_split_columns.rds",  update = FALSE)
  expect_identical(fre(list(mtcars$vs, mtcars$am), prepend_var_lab = FALSE), 
                   add_rows(fre(mtcars$vs, prepend_var_lab = FALSE), 
                            fre(mtcars$am, prepend_var_lab = FALSE))
  )
  
  # multiple-choice variable
  # brands - multiple response question
  # Which brands do you use during last three months? 
  set.seed(123)
  brands = data.frame(t(replicate(20,sample(c(1:5,NA),4,replace = FALSE))))
  # score - evaluation of tested product
  score = sample(-1:1,20,replace = TRUE)
  var_lab(brands) = "Used brands"
  val_lab(brands) = make_labels("
                              1 Brand A
                              2 Brand B
                              3 Brand C
                              4 Brand D
                              5 Brand E
                              ")
  
  var_lab(score) = "Evaluation of tested brand"
  val_lab(score) = make_labels("
                             -1 Dislike it
                             0 So-so
                             1 Like it    
                             ")
  
  expect_known_value(fre(brands), "rds/fre_ex4.rds",  update = FALSE)
  expect_known_value(fre(as.dichotomy(brands)), "rds/fre_dichotomy.rds",  update = FALSE)
  mat_brands = as.matrix(brands)
  
  
  expect_known_value(fre(mat_brands), "rds/fre_ex4mat.rds",  update = FALSE)
  expect_known_value(cro(brands, list(total(), score)), "rds/fre_ex5.rds",  update = FALSE)
  expect_known_value(cro(mrset(brands), list(total(), score)), "rds/fre_ex5mrset.rds",  update = FALSE)
  expect_known_value(cro(as.dichotomy(brands), list(total(), score)), "rds/fre_ex5mrset.rds",  update = FALSE)
  expect_known_value(cro_cpct(mrset(brands), list(total(), score)), "rds/fre_ex6mrset.rds",  update = FALSE)
  expect_known_value(cro_cpct(as.dichotomy(brands), list(total(), score)), "rds/fre_ex6mrset.rds",  update = FALSE)
  
  a = 1
  var_lab(a) = "Total"
  val_lab(a) = c("all" = 1)
  expect_known_value(cro_cpct(mrset(brands), a), "rds/fre_ex7.rds",  update = FALSE)
  expect_known_value(cro_cpct_responses(mrset(brands), a), "rds/fre_ex7responses.rds",  update = FALSE)
  expect_known_value(cro_cpct_responses(mrset(brands), score), "rds/fre_ex7responses2.rds",  update = FALSE)
  expect_identical(cro_cpct_responses(score, a, total_label = "#Total"), cro_cpct(score, a, total_label = "#Total"))
  # expect_known_value(cro_cpct_responses(as.dichotomy(mrset(brands)), a), "rds/fre_ex7responses.rds",  update = FALSE)
  
  
  #################################################
  context("fre and cro some special cases")
  
  expect_known_value(fre(numeric(0)), "rds/fre1.rds",  update = FALSE)
  
  a = matrix(1:9, 3)
  expect_known_value(fre(a[, FALSE, drop = FALSE]), "rds/fre1matrix.rds",  update = FALSE)
  
  a = numeric(0)
  val_lab(a) = autonum(letters[1:3])
  expect_known_value(fre(a), "rds/fre1_empty_with_labels.rds",  update = FALSE)
  expect_known_value(cro(a), "rds/cro_empty_with_labels.rds",  update = FALSE)
  expect_known_value(cro(as.dichotomy(as.data.frame(matrix(1, 3, 3)))), "rds/cro_single_column_mdset.rds",  update = FALSE)
  
  aaa = data.frame(1:5)[, -1, drop = FALSE]
  expect_known_value(cro(mdset(aaa)), "rds/cro_zero_column_multiple_set.rds",  update = FALSE)
  expect_known_value(cro(mrset(aaa)), "rds/cro_zero_column_multiple_set.rds",  update = FALSE)
  expect_known_value(cro(mrset(aaa[FALSE, ])), "rds/cro_zero_column_multiple_set.rds",  update = FALSE)
  
  
  
  expect_known_value(cro(mrset(as.data.frame(rep(1,3)))), "rds/cro_single_column_mdset.rds",  update = FALSE)
  expect_known_value(fre(a, drop_unused_labels = FALSE), "rds/fre1_empty_with_labels_not_drop.rds",  update = FALSE)
  
  a = rep(NA, 5)
  
  expect_known_value(fre(a), "rds/fre2.rds",  update = FALSE)
  
  expect_known_value(cro(list(a), list(a, total())), "rds/cro1.rds",  update = FALSE)
  expect_known_value(drop_rc(cro(list(a), list(a, total()))), "rds/cro1_drop.rds",  update = FALSE)
  expect_known_value(cro_cpct(list(a), list(a, total())), "rds/cro1.rds",  update = FALSE)
  
  a = c(1,1,1, NA, NA)
  b = c(NA, NA, NA, 1, 1)
  expect_known_value(cro(list(a), list(b)), "rds/cro3.rds",  update = FALSE)
  if(sessionInfo()$R.version$arch!="i386"){
    expect_known_value(cro_cpct(list(a), list(b)), "rds/cro3.rds",  update = FALSE)
    
    expect_known_value(cro_rpct(list(a), list(b)), "rds/cro3.rds",  update = FALSE)
    expect_known_value(cro_tpct(list(a), list(b)), "rds/cro3.rds",  update = FALSE)
    a = c(1,1,1, 1, 1)
    b = c(1, 1, 1, 1, 1)
    weight = rep(NA, 5)
    expect_known_value(cro(list(a), list(b), weight = weight), "rds/cro3.rds",  update = FALSE)
    expect_known_value(cro_cpct(list(a), list(b), weight = weight), "rds/cro3.rds",  update = FALSE)
    expect_known_value(cro_rpct(list(a), list(b), weight = weight), "rds/cro3.rds",  update = FALSE)
    expect_known_value(cro_tpct(list(a), list(b), weight = weight), "rds/cro3.rds",  update = FALSE)
  }
  
  context("cro_fun")
  
  a = c(1,1,1, NA, NA)
  b = c(NA, NA, NA, 1, 1)
  expect_error(cro_fun(a, b))
  expect_known_value(cro_fun(a, list(b, total()), fun = length), "rds/cro_fun1.rds",  update = FALSE)
  
  a = c(1,1,1, 1, 1)
  b = c(1, 1, 2, 2, 2)
  
  
  expect_known_value(cro_fun(b, list(a, total()), fun = mean), "rds/cro_fun2.rds",  update = FALSE)
  expect_known_value(cro_fun(b, list(as.matrix(a), total()), fun = mean), "rds/cro_fun2.rds",  update = FALSE)
  
  weight = rep(1, 5)
  expect_known_value(cro_fun(b, list(a, total()), weight = weight, fun = function(x, weight){
    weighted.mean(x, w = weight)
    
  }), "rds/cro_fun2.rds",  update = FALSE)
  
  expect_known_value(cro_fun(b, list(a, total()), weight = 1, fun = function(x, weight){
    weighted.mean(x, w = weight)
    
  }), "rds/cro_fun2.rds",  update = FALSE)
  
  expect_known_value(cro_fun(b, list(1, total()), weight = 1, fun = function(x, weight){
    weighted.mean(x, w = weight)
    
  }), "rds/cro_fun2.rds",  update = FALSE)
  
  ##############
  expect_known_value(cro_fun_df(b, list(a, total()), weight = weight, fun = function(x, weight){
    setNames(weighted.mean(x[[1]], w = weight), names(x))
    
  }), "rds/cro_fun3.rds",  update = FALSE)
  
  
  expect_known_value(cro_fun_df(b, list(1, total()), weight = 1, fun = function(x, weight){
    setNames(weighted.mean(x[[1]], w = weight), names(x))
    
  }), "rds/cro_fun3.rds",  update = FALSE)
  
  
  expect_error(
    cro_fun_df(b, a, weight = 1:2, fun = function(x, weight){
      setNames(weighted.mean(x[[1]], w = weight), names(x))
    })
  )
  
  expect_known_value(cro_fun_df(b, list(a, total()), weight = 1, fun = function(x, weight){
    setNames(weighted.mean(x[[1]], w = weight), names(x))
    
  }), "rds/cro_fun3.rds",  update = FALSE)
  
  expect_known_value(cro_fun_df(b, list(as.matrix(a), total()), weight = 1, 
                                fun = function(x, weight){
                                  setNames(weighted.mean(x[[1]], w = weight), names(x))
                                  
                                }), "rds/cro_fun3.rds",  update = FALSE)
  
  weight = rep(NA, 5)
  expect_known_value(cro_fun(b, list(as.labelled(a), total()), weight = weight, fun = function(x, weight){
    weighted.mean(x, w = weight)
    
  }), "rds/cro_fun4.rds",  update = FALSE)
  
  weight = c(0, 0, 1, 1, 1)
  
  expect_known_value(cro_fun(b, list(a, total()), weight = weight, fun = function(x, weight){
    weighted.mean(x, w = weight)
    
  }), "rds/cro_fun5.rds",  update = FALSE)
  
  a = c(1,1,1, 1, 1)
  b = c(0, 1, 2, 2, NA)
  weight = c(0, 0, 1, 1, 1)
  expect_known_value(cro_fun(b,  list(a, total()), weight = weight, fun = function(x, weight){
    weighted.mean(x, w = weight)
    
  }), "rds/cro_fun6.rds",  update = FALSE)
  
  expect_known_value(cro_fun(b,  list(a, total()), 
                             weight = weight, fun = function(x, weight, na.rm){
                               weighted.mean(x, w = weight, na.rm = na.rm)
                               
                             }, na.rm = TRUE), "rds/cro_fun7.rds",  update = FALSE)
  
  expect_error(
    cro_fun(b, a, weight = 1:2, fun = function(x, weight, na.rm){
      weighted.mean(x, w = weight, na.rm = na.rm)
    }, na.rm = TRUE)
  )
  
  expect_error(cro_fun(b, a, weight = weight, fun = function(x, w, na.rm){
    weighted.mean(x, w = w, na.rm = na.rm)
    
  }, na.rm = TRUE))
  
  
  
  
  
  expect_known_value(cro_fun(iris[,-5], list(iris$Species, total()), fun = median), "rds/cro_fun8.rds",  update = FALSE)
  
  # data(mtcars)
  mtcars = modify(mtcars,{
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0, 
                    manual=1)
  })
  
  
  expect_known_value(
    with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(am, total()), fun = mean)),
    "rds/cro_fun9.rds",  update = FALSE)
  
  expect_known_value(
    with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(as.data.frame(am), total()), fun = mean)),
    "rds/cro_fun9.rds",  update = FALSE)
  
  
  expect_known_value(
    with(mtcars, cro_fun_df(data.frame(hp, mpg, disp), list(am, total()), fun = mean_col)),
    "rds/cro_fun9.rds",  update = FALSE)
  
  expect_known_value(
    with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(fctr(am):fctr(vs), total()), fun = mean)), 
    "rds/cro_fun10.rds",  update = FALSE)
  
  if(as.numeric(version$major) ==3 && as.numeric(version$minor)<4){
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), am, fun = summary)), 
      "rds/cro_fun11.rds",  update = FALSE)
  } else {
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), am, fun = summary)), 
      "rds/cro_fun11_R3.4.rds",  update = FALSE)
    
  }
  
  expect_known_value(
    with(mtcars, cro_fun_df(data.frame(hp, mpg, disp), list(am, total()), fun = colMeans)), 
    "rds/cro_fun9.rds",  update = FALSE)
  
  expect_known_value(
    with(mtcars, cro_fun_df(data.frame(hp, mpg, disp), list(as.sheet(am), total()), fun = colMeans)), 
    "rds/cro_fun9.rds",  update = FALSE)
  
  if(as.numeric(version$major) ==3 && as.numeric(version$minor)<4){
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), am, fun = summary)),
      "rds/cro_fun11.rds",  update = FALSE)
    
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), am, row_vars = vs, fun = summary)),
      "rds/cro_fun11vs.rds",  update = FALSE)
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(am, total()), fun = function(x) t(summary(x)))),
      "rds/cro_fun12.rds",  update = FALSE)
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(am, total()), 
                           row_vars = vs, 
                           fun = function(x) t(summary(x)))),
      "rds/cro_fun12vs.rds",  update = FALSE)
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), 
                           list(am, total()), 
                           fun = function(x) matrix(summary(x),2))
      ), 
      "rds/cro_fun13.rds",  update = FALSE)
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(am, total()), fun = function(x) {
        res = matrix(summary(x),2)
        rownames(res) = c("a","b")
        colnames(res) = c("c","d","e")
        res
      })), 
      "rds/cro_fun14.rds",  update = FALSE)
  } else {
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), am, fun = summary)),
      "rds/cro_fun11_R3.4.rds",  update = FALSE)
    
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), am, row_vars = vs, fun = summary)),
      "rds/cro_fun11vs_R3.4.rds",  update = FALSE)
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(am, total()), fun = function(x) t(summary(x)))),
      "rds/cro_fun12_R3.4.rds",  update = FALSE)
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(am, total()), 
                           row_vars = vs, 
                           fun = function(x) t(summary(x)))),
      "rds/cro_fun12vs_R3.4.rds",  update = FALSE)
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), 
                           list(am, total()), 
                           fun = function(x) matrix(summary(x),2))
      ), 
      "rds/cro_fun13_R3.4.rds",  update = FALSE)
    
    expect_known_value(
      with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(am, total()), fun = function(x) {
        res = matrix(summary(x),2)
        rownames(res) = c("a","b")
        colnames(res) = c("c","d","e")
        res
      })), 
      "rds/cro_fun14_R3.4.rds",  update = FALSE)    
  }
  
  expect_known_value(
    with(mtcars, cro_fun(data.frame(hp, mpg, disp), list(am, total()), fun = function(x) {
      c(mean = mean(x, na.rm = TRUE), stdev = sd(x, na.rm = TRUE), valid = sum(!is.na(x)))
    })), 
    "rds/cro_fun15.rds",  update = FALSE)
  
  expect_error(
    with(mtcars, cro_fun(data.frame(hp, mpg, disp), 
                         list(am, total()), fun = function(x) sheet(t(x)))
    )
  )
  
  
  
  context("cro_mean")
  
  a = c(1,1,1, NA, NA)
  b = c(NA, NA, NA, 1, 1)
  expect_known_value(cro_mean(a, list(b, total())), "rds/cro_mean1.rds",  update = FALSE)
  expect_known_value(cro_sum(a, list(b, total())), "rds/cro_sum1.rds",  update = FALSE)
  expect_known_value(calc_cro_sum(sheet(a, b), a, list(b, total())), 
                     "rds/cro_sum1.rds",  update = FALSE)
  expect_known_value(cro_median(a, list(b, total())), "rds/cro_median1.rds",  update = FALSE)
  
  a = c(1,1,1, 1, 1)
  b = c(1, 1, 2, 2, 2)
  
  
  expect_known_value(cro_mean(b, list(a, total())), "rds/cro_mean2.rds",  update = FALSE)
  expect_known_value(cro_median(b, list(a, total())), "rds/cro_median2.rds",  update = FALSE)
  expect_known_value(calc_cro_median(sheet(a, b), b, list(a, total())), "rds/cro_median2.rds",  update = FALSE)
  
  weight = rep(1, 5)
  expect_known_value(cro_mean(b, list(a, total()), weight = weight), "rds/cro_mean2.rds",  update = FALSE)
  expect_known_value(cro_sum(b, list(a, total()), weight = weight), "rds/cro_sum3.rds",  update = FALSE)
  expect_known_value(cro_sum(b, list(a, total()), weight = 1), "rds/cro_sum3.rds",  update = FALSE)
  
  weight = rep(NA, 5)
  expect_known_value(cro_mean(b, list(a, total()), weight = weight), "rds/cro_mean4.rds",  update = FALSE)
  expect_known_value(cro_sum(b, list(a, total()), weight = weight), "rds/cro_sum4.rds",  update = FALSE)
  
  weight = c(0, 0, 1, 1, 1)
  
  expect_known_value(cro_mean(b, list(a, total()), weight = weight), "rds/cro_mean5.rds",  update = FALSE)
  expect_known_value(cro_sum(b, list(a, total()), weight = weight), "rds/cro_sum5.rds",  update = FALSE)
  
  a = c(1,1,1, 1, 1)
  b = c(0, 1, 2, 2, NA)
  weight = c(0, 0, 1, 1, 1)
  expect_known_value(cro_mean(b, list(a, total()), weight = weight), "rds/cro_mean6.rds",  update = FALSE)
  expect_known_value(cro_median(b, list(a, total()), weight = weight), "rds/cro_median6.rds",  update = FALSE)
  expect_known_value(cro_sum(b, list(a, total()), weight = weight), "rds/cro_sum6.rds",  update = FALSE)
  
  
  
  expect_known_value(cro_median(iris[,-5], list(iris$Species, total())), "rds/cro_median8.rds",  update = FALSE)
  expect_known_value(calc_cro_median(iris, ..[!perl("Species")], list(Species, total())), 
                     "rds/cro_median8.rds",  update = FALSE)
  expect_known_value(cro_median(iris[,-5], list(iris$Species, total()), weight = rep(1, 150)),
                     "rds/cro_median8.rds",  update = FALSE)
  expect_known_value(cro_median(iris[,-5], list(iris$Species, total()), weight = 1),
                     "rds/cro_median8.rds",  update = FALSE)
  expect_known_value(cro_mean(iris[,-5], list(iris$Species, total())), 
                     "rds/cro_mean8.rds",  update = FALSE)
  
  
  
  expect_known_value(cro_sum(iris[,-5], list(iris$Species, total())), "rds/cro_sum8.rds",  update = FALSE)
  expect_known_value(calc_cro_sum(iris, ..[!perl("Species")], list(Species, total())), 
                     "rds/cro_sum8.rds",  update = FALSE)
  
  expect_known_value(cro_fun(iris[,-5], list(iris$Species, total()), fun = mean), "rds/cro_mean8.rds",  update = FALSE)
  
  expect_known_value(cro_fun_df(iris[,-5], list(iris$Species, total()), fun = mean_col), "rds/cro_mean8.rds",  update = FALSE)
  
  #####
  expect_known_value(cro_median(as.list(iris[,-5]), list(iris$Species, total())), "rds/cro_median8.rds",  update = FALSE)
  expect_known_value(cro_mean(as.list(iris[,-5]), list(iris$Species, total())), "rds/cro_mean8.rds",  update = FALSE)
  expect_known_value(cro_sum(as.list(iris[,-5]), list(iris$Species, total())), "rds/cro_sum8.rds",  update = FALSE)
  
  expect_known_value(cro_fun(as.list(iris[,-5]), list(iris$Species, total()), fun = mean), "rds/cro_mean8.rds",  update = FALSE)
  
  expect_known_value(cro_fun_df(as.list(iris[,-5]), list(iris$Species, total()), fun = mean_col), "rds/cro_mean8.rds",  update = FALSE)
  # expect_known_value(cro_fun_df(iris[,-5], list(iris$Species, total()), fun = mean_col),
  #                           "rds/cro_mean8.rds",  update = FALSE)
  
  #####
  expect_known_value(cro_median(as.matrix(iris[,-5]), list(iris$Species, total())), "rds/cro_median8.rds",  update = FALSE)
  expect_known_value(cro_mean(as.matrix(iris[,-5]), list(iris$Species, total())), "rds/cro_mean8.rds",  update = FALSE)
  expect_known_value(cro_sum(as.matrix(iris[,-5]), list(iris$Species, total())), "rds/cro_sum8.rds",  update = FALSE)
  
  expect_known_value(cro_fun(as.matrix(iris[,-5]), list(iris$Species, total()), fun = mean), "rds/cro_mean8.rds",  update = FALSE)
  
  # expect_known_value(cro_fun_df(as.list(iris[,-5]), iris$Species, fun = mean_col), "rds/cro_mean8.rds",  update = FALSE)
  
  ############
  
  expect_known_value(cro_fun_df(iris[,-5], list(iris$Species, total()), fun = function(x) cor(x)[,1]),
                     "rds/cro_fun_df1.rds",  update = FALSE)
  expect_known_value(cro_fun_df(as.list(iris[,-5]), list(iris$Species, total()), fun = summary), 
                     "rds/cro_fun_df2.rds",  update = FALSE)
  
  # expect_known_value(cro_fun(iris[,-5], list(iris$Species, total()), fun = summary), 
  #                           "rds/cro_fun_df2.rds",  update = FALSE)
  
  
  context("table_summary methods")
  # data(mtcars)
  mtcars = modify(mtcars,{
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
    var_lab(hp) = "Gross horsepower"
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0, 
                    manual=1)
  })
  expect_known_value(cro_mean(mtcars$mpg, list(unvr(mtcars$am), total()))[,"manual"],
                     "rds/mean_methods_1.rds",  update = FALSE)
  expect_known_value(cro_fun(list(mtcars$mpg), list(unvr(mtcars$am), total()), fun = sum)[, 1], 
                     "rds/fun_methods_1.rds",  update = FALSE)
  
  duplicated_colnames = cro_mean(mtcars$mpg, list(mtcars$am, total())) %merge% 
    cro_mean(mtcars$mpg, list(mtcars$am, total())) 
  expect_known_value(duplicated_colnames[,-2], 
                     "rds/cro_methods_2.rds",  update = FALSE)
  expect_known_value(duplicated_colnames[,seq_along(duplicated_colnames)[-2]], 
                     "rds/cro_methods_2.rds",  update = FALSE)
  duplicated_colnames = cro(mtcars$vs, list(mtcars$am, total())) %merge% 
    cro(mtcars$vs, list(mtcars$am, total())) 
  expect_known_value(duplicated_colnames[,-2], 
                     "rds/cro_methods_3.rds",  update = FALSE)
  expect_known_value(duplicated_colnames[,seq_along(duplicated_colnames)[-2]], 
                     "rds/cro_methods_3.rds",  update = FALSE)
  context("datetime")
  
  aaa = rep(c(as.POSIXct("2016-09-22 02:28:39"), as.POSIXct("2016-09-22 03:28:39")), 10)
  var_lab(aaa) = "aaa"
  bbb = rep(c(as.POSIXct("2016-09-22 03:28:39"), as.POSIXct("2016-09-22 02:28:39")), 10)
  var_lab(bbb) = "bbb"
  a_total = rep("total", 20)
  
  aaa_str = as.character(aaa)
  var_lab(aaa_str) = "aaa"
  bbb_str = as.character(bbb)
  var_lab(bbb_str) = "bbb"
  
  expect_identical(fre(aaa), fre(aaa_str))
  
  expect_identical(cro(aaa, bbb), cro(aaa_str, bbb_str))
  expect_identical(cro_cpct(aaa, bbb), cro_cpct(aaa_str, bbb_str)) 
  expect_identical(cro_rpct(aaa, a_total),cro_rpct(aaa_str, a_total)) 
  expect_identical(cro_rpct(list(aaa), list(a_total)), cro_rpct(list(aaa_str), list("total"))) 
  expect_identical(cro_rpct(list(aaa), list("total")),cro_rpct(list(aaa_str), list(a_total))) 
  expect_identical(cro_tpct(a_total, bbb), cro_tpct(a_total, bbb_str)) 
  
  context("cro duplicated names")
  
  data(iris)
  ex_iris = iris[,-5]
  correct_iris = iris[,-5]
  colnames(ex_iris) = c("a", "a", "a", "a")
  colnames(correct_iris) = c("v1", "v2", "v3", "v4")
  
  var_lab(ex_iris[[1]]) = "v1"
  var_lab(ex_iris[[2]]) = "v2"
  var_lab(ex_iris[[3]]) = "v3"
  var_lab(ex_iris[[4]]) = "v4"
  
  expect_identical(cro_mean(ex_iris, iris$Species), cro_mean(correct_iris, iris$Species))
  expect_identical(cro_sum(ex_iris, iris$Species), cro_sum(correct_iris, iris$Species))
  expect_identical(cro_median(ex_iris, iris$Species), cro_median(correct_iris, iris$Species))
  expect_identical(cro_fun(ex_iris, iris$Species, fun = mean), cro_fun(correct_iris, iris$Species, fun = mean))
  expect_identical(cro_fun_df(ex_iris, iris$Species, fun = mean_col), 
                   cro_fun_df(correct_iris, iris$Species, fun = mean_col))
  
  data(iris)
  # ex_iris = iris[,-5]
  lst_iris = as.list(ex_iris)
  names(lst_iris) = NULL
  
  expect_identical(cro_mean(lst_iris, iris$Species), cro_mean(correct_iris, iris$Species))
  expect_identical(cro_sum(lst_iris, iris$Species), cro_sum(correct_iris, iris$Species))
  expect_identical(cro_median(lst_iris, iris$Species), cro_median(correct_iris, iris$Species))
  expect_identical(cro_fun(lst_iris, iris$Species, fun = mean), cro_fun(correct_iris, iris$Species, fun = mean))
  expect_identical(cro_fun_df(lst_iris, iris$Species, fun = mean_col), 
                   cro_fun_df(correct_iris, iris$Species, fun = mean_col))
  
  
  # data(iris)
  # lst_iris = as.list(iris[,-5])
  # names(lst_iris) = NULL
  # colnames(correct_iris) = c("V1", "V2", "V3", "V4")
  # expect_identical(cro_mean(lst_iris, iris$Species), cro_mean(correct_iris, iris$Species))
  # expect_identical(cro_sum(lst_iris, iris$Species), cro_sum(correct_iris, iris$Species))
  # expect_identical(cro_median(lst_iris, iris$Species), cro_median(correct_iris, iris$Species))
  # expect_identical(cro_fun(lst_iris, iris$Species, fun = mean), cro_fun(correct_iris, iris$Species, fun = mean))
  # expect_identical(cro_fun_df(lst_iris, iris$Species, fun = mean_col), 
  #                  cro_fun_df(correct_iris, iris$Species, fun = mean_col))
  
  
  ##################
  
  # multiple-choice variable
  # brands - multiple response question
  # Which brands do you use during last three months? 
  set.seed(123)
  brands = data.frame(t(replicate(20,sample(c(1:5,NA),4,replace = FALSE))))
  # score - evaluation of tested product
  score = sample(-1:1,20,replace = TRUE)
  var_lab(brands) = "Used brands"
  val_lab(brands[[3]]) = make_labels("
                              1 Brand A
                              2 Brand B
                              3 Brand C
                              4 Brand D
                              5 Brand E
                              ")
  
  var_lab(score) = "Evaluation of tested brand"
  val_lab(score) = make_labels("
                             -1 Dislike it
                             0 So-so
                             1 Like it    
                             ")
  
  expect_known_value(fre(brands), "rds/fre_ex4.rds",  update = FALSE)
  
  
  #######
  # data(mtcars)
  
  expect_error(fre(mtcars$dont_exist))
  expect_error(cro(mtcars$dont_exist, mtcars$am))
  expect_error(cro(mtcars$am, mtcars$dont_exist))
  expect_error(cro_cpct(mtcars$dont_exist, mtcars$am))
  expect_error(cro_cpct(mtcars$am, mtcars$dont_exist))
  expect_error(cro_rpct(mtcars$dont_exist, mtcars$am))
  expect_error(cro_rpct(mtcars$am, mtcars$dont_exist))
  expect_error(cro_tpct(mtcars$dont_exist, mtcars$am))
  expect_error(cro_tpct(mtcars$am, mtcars$dont_exist))
  expect_error(cro_sum(mtcars$dont_exist, mtcars$am))
  expect_error(cro_sum(mtcars$am, mtcars$dont_exist))
  expect_error(cro_mean(mtcars$dont_exist, mtcars$am))
  expect_error(cro_mean(mtcars$am, mtcars$dont_exist))
  expect_error(cro_median(mtcars$dont_exist, mtcars$am))
  expect_error(cro_median(mtcars$am, mtcars$dont_exist))
  expect_error(cro_fun(mtcars$dont_exist, mtcars$am, fun = sum))
  expect_error(cro_fun(mtcars$am, mtcars$dont_exist, fun = sum))
  expect_error(cro_fun_df(mtcars$dont_exist, mtcars$am, fun = median))
  expect_error(cro_fun_df(mtcars$am, mtcars$dont_exist, fun = sum))
  
  
  data(iris)
  expect_error(
    cro_fun(iris[,-5], iris$Species, fun = sum, weight = runif(150))
  )
  expect_error(
    cro_fun_df(iris[,-5], iris$Species, fun = sum_col, weight = runif(150))
  )
  
  
}