#' Add subtotal to a set of categories
#' 
#' 'subtotal' adds subtotal to set of categories, 'net' replaces categories with
#' their net value. If you provide named arguments then name will be used as
#' label for subtotal. In other case labels will be automatically generated
#' taking into account arguments 'new_label' and 'prefix'. Note that if you
#' provide overlapping categories then net and subtotals will also be
#' overlapping. 'subtotal' and 'net' are intended for usage with \link{cro} and
#' friends. 'tab_subtotal_*' and 'tab_net_*' are intended for usage with custom
#' tables - see \link{tables}.
#' @param x variable, list, data.frame or multiple response set
#' @param ... list of categories for grouping. It can be numeric vectors (for
#'   example, 1:2), ranges (for example, 4 %thru% hi) or criteria (for example,
#'   greater(5)). If an argument is named then this name will be used as label for
#'   subtotal.
#' @param position position of the subtotal or net relative to original
#'   categories. "below" by default. One of the "below", "above", "top",
#'   "bottom". "top" and "bottom" place nets and subtotals above or below all
#'   other categories. For nets "below" and "above" have no difference because
#'   original categories are removed.
#' @param prefix character, "TOTAL " by default. It is a prefix to automatically
#'   created labels for nets and subtotals.
#' @param new_label how we will combine original values for automatically
#'   generated subtotal labels. Possible values are "all", "range", "first",
#'   "last". "all" collapse all labels, "range" take only first and last label,
#' @param add logical. Should we add subtotal to categories or replace categories with a net? 
#' @param data intermediate table. See \link{tables}.
#' @return multiple response set or list of the multiple response sets
#' @examples 
#' ol = c(1:7, 99)
#' var_lab(ol) = "Liking"
#' val_lab(ol)  = num_lab("
#'                      1 Disgusting
#'                      2 Very Poor
#'                      3 Poor
#'                      4 So-so
#'                      5 Good
#'                      6 Very good
#'                      7 Excellent
#'                      99 Hard to say
#'                      ")
#'                      
#' cro(subtotal(ol, BOTTOM = 1:3, TOP = 6:7, position = "top"))
#' # autolabelling
#' cro(subtotal(ol, 1:3, 6:7))
#' # replace original codes and another way of autolabelling
#' cro(net(ol, 1:3, 6:7, new_label = "range", prefix = "NET "))
#' 
#' # character variable and criteria usage
#' items = c("apple", "banana", "potato", "orange", "onion", "tomato", "pineapple")
#' cro(
#'     subtotal(items, 
#'              "TOTAL FRUITS"     = like("*ap*") | like("*an*"), 
#'              "TOTAL VEGETABLES" = like("*to*") | like("*on*"), 
#'              position = "bottom")
#' )
#' 
#' # 'tab_net_*' usage
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon",
#'                       am = "Transmission",
#'                       am = c("Automatic" = 0,
#'                              "Manual"=1),
#'                       gear = "Number of forward gears",
#'                       gear = c(
#'                           One = 1,
#'                           Two = 2,
#'                           Three = 3,
#'                           Four = 4,
#'                           Five = 5
#'                       )
#' )
#' mtcars %>% 
#'     tab_cells(mpg) %>% 
#'     tab_net_cells("Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg))) %>% 
#'     tab_cols(total(), am) %>% 
#'     tab_stat_cases() %>% 
#'     tab_pivot()
#' 
#' mtcars %>% 
#'     tab_cells(mpg) %>% 
#'     tab_rows(gear) %>%
#'     tab_subtotal_rows(1:2, 3:4, "5 and more" = greater(4)) %>% 
#'     tab_stat_mean() %>% 
#'     tab_pivot()
#' @export
net = function(x, ..., 
               position = c("below", "above", "top", "bottom"), 
               prefix = "TOTAL ", 
               new_label = c("all", "range", "first", "last"),
               add = FALSE
){
    UseMethod("net")
}

#' @export
net.default = function(x, ..., 
                       position = c("below", "above", "top", "bottom"), 
                       prefix = "TOTAL ", 
                       new_label = c("all", "range", "first", "last"),
                       add = FALSE){
    

    position = match.arg(position)  
    new_label = match.arg(new_label)
    
    args = list(...)
    arg_names = names(args)
    if(is.null(arg_names)) arg_names = rep("", length(args))
    if(is.factor(x)) {
        all_values = levels(x)
    } else {
        all_values = unique(x, nmax = 1) 
    }
    subtotal_codes = lapply(args, function(curr_net){
        if(!inherits(curr_net, "criterion") && !is.atomic(curr_net)) {
            curr_net = as.criterion(curr_net)
        }  
        if(inherits(curr_net, "criterion")){
            source_codes = sort(all_values %i% curr_net)
        } else {
            # we want this to provide possibility for custom sorting.
            # all items will be in the order as declared in subtotal
            source_codes = curr_net
        } 
        source_codes
    })
    
    if(is.factor(x)){
        all_values = as.character(all_values) %u% sort(unlist(subtotal_codes, use.names = FALSE))
    } else {
        all_values = sort(all_values %u% unlist(subtotal_codes, use.names = FALSE))
    }
    if(!is.numeric(x)){
        varlab = var_lab(x)
        x = match(x, all_values, incomparables = NA)
        subtotal_codes = lapply(subtotal_codes, match, all_values, incomparables = NA)
        vallabs = as.character(all_values)
        all_values = seq_along(all_values)
        val_lab(x) = setNames(all_values, vallabs)
        var_lab(x) = varlab
        
        
    } else {
        # we need this because all values should have labels
        val_lab(x) = val_lab(x) %u% setNames(all_values, as.character(all_values))    
    }

    source_codes = create_groups(all_values, subtotal_codes)
    new_codes = renumerate_codes(source_codes)
    cat_codes = category_codes(new_codes, position = position)
    not_in_net_old = unlist(lapply(source_codes, "[[", "copy_codes"), use.names = FALSE)
    not_in_net_new = unlist(lapply(new_codes, "[[", "copy_codes"), use.names = FALSE)
    in_net_old = lapply(source_codes, "[[", "cat_codes")
    in_net_new = lapply(new_codes, "[[", "cat_codes")
    length(not_in_net_old)==length(not_in_net_new) || stop("'net' - something is going wrong. Please, report to author.")
    
    if(length(not_in_net_old)>0){
        first_col = recode(x, from_to(not_in_net_old, not_in_net_new), with_labels = TRUE)
    } else {
        first_col = recode(x, TRUE ~ NA, with_labels = TRUE)
    }
    j = 1 # for the case when we have empty subtotals
    other_cols = vector(mode = "list", length = sum(lengths(in_net_old)>0))
    for(i in seq_along(cat_codes)){
        
        if(length(in_net_old[[i]])>0){
            frm_net = in_net_old[[i]] ~ cat_codes[[i]]
            recode_args = list(x, frm_net, new_label = new_label, with_labels = TRUE)
            label = arg_names[[i]] 
            names(recode_args)[[2]] = label
            arg_names[[i]]  = "" # we need to keep only orphan labels
            res = do.call(recode, recode_args)
            
            if(!is.null(val_lab(res)) && (is.null(label) || is.na(label) || identical(label, ""))) {
                names(val_lab(res)) = paste0(prefix, names(val_lab(res)))
            }
            
            if(add){
                items = recode(x, from_to(in_net_old[[i]], in_net_new[[i]]), with_labels = TRUE) 
                other_cols[[j]] = list(items, res)
            } else {
                other_cols[[j]] = list(res)    
            }
            j = j + 1
        }
        
    }
    other_cols = unlist(other_cols, recursive = FALSE, use.names = FALSE)
    res = c(list("v1" = first_col), setNames(other_cols, paste0("v", seq_along(other_cols) + 1)))
    add_val_lab(res[[1]]) = setNames(cat_codes[arg_names!=""], arg_names[arg_names!=""])
    do.call(mrset, res)
}

# @param subtotal_codes list of vectors with codes
# @param possible_values vector with all original codes
create_groups = function(possible_values, subtotal_codes){
    res = list()
    possible_values = possible_values %d% unique(unlist(subtotal_codes, use.names = FALSE))
    for(i in seq_along(subtotal_codes)){
        min_code = suppressWarnings(min(subtotal_codes[[i]], na.rm = TRUE)) 
        if(is.finite(min_code)){
            copy_codes = possible_values[possible_values<min_code]
            possible_values = possible_values[possible_values>min_code & !(possible_values %in% subtotal_codes[[i]])]
        } else {
            copy_codes = integer(0) 
        }
        res[[i]] = list(copy_codes = copy_codes, cat_codes = subtotal_codes[[i]])
    }
    if(length(possible_values)>0) res[[i+1]] = list(copy_codes = possible_values)
    res
}

# @param code_groups - result of `create_groups`
renumerate_codes = function(code_groups){
      res = list()
      curr_max = 0
      for(i in seq_along(code_groups)){
          curr_codes = code_groups[[i]]
          res[[i]] = list()
          for(j in seq_along(curr_codes)){
              new_seq = seq_along(curr_codes[[j]])
              # if there is no codes for category we make single code as a placeholder
              # it will be usefull when we will create category codes
              if(length(new_seq)==0 & j==2) new_seq = 1 
              new_codes = new_seq + curr_max
              res[[i]][[j]] = new_codes
              new_max = suppressWarnings(max(new_codes, na.rm = TRUE))
              if(is.finite(new_max)){
                curr_max = new_max        
              }
          }
          names(res[[i]]) = names(code_groups[[i]])
      }
      res
    
}


category_codes = function(new_codes, position = c("below", "above", "bottom", "top")){
    position = match.arg(position)
    cat_codes = lapply(new_codes, "[[", "cat_codes")
    cat_codes = cat_codes[lengths(cat_codes)>0]
    
    # we shouldn't have empty cat_codes 
    switch(position, 
           above = unlist(lapply(cat_codes, min, na.rm = TRUE), use.names = FALSE) - 0.5 ,
           below = unlist(lapply(cat_codes, max, na.rm = TRUE), use.names = FALSE) + 0.5 ,
           top = seq_along(cat_codes) - length(cat_codes),
           bottom =  seq_along(cat_codes) + max(unlist(new_codes), na.rm = TRUE)
    )
}


#' @export
net.category = function(x, ..., 
                        position = c("below", "above", "top", "bottom"), 
                        prefix = "TOTAL ", 
                        new_label = c("all", "range", "first", "last"),
                        add = FALSE
){

    position = match.arg(position)
    new_label = match.arg(new_label)
    all_values = unique(unlist(lapply(x, unique, nmax = 1), use.names = FALSE))
    val_lab(x) = val_lab(x) %u% setNames(all_values, all_values) 
    res = lapply(x, net, ..., position = position, prefix = prefix, new_label = new_label, add = add)
    res = unlist(res, recursive = FALSE, use.names = FALSE)
    names(res) = paste0("v", seq_along(res))
    res = do.call(mrset, res)
    as.category(as.dichotomy(res, keep_unused = TRUE)) # to remove duplicates
}

#' @export
net.dichotomy = function(x, ..., 
                         position = c("below", "above", "top", "bottom"), 
                         prefix = "TOTAL ", 
                         new_label = c("all", "range", "first", "last"),
                         add = FALSE
){
    stop("'net': sorry, nets on dichotomy isn't currently supported.")    
}

#' @export
net.list = function(x, ..., 
                    position = c("below", "above", "top", "bottom"), 
                    prefix = "TOTAL ", 
                    new_label = c("all", "range", "first", "last"),
                    add = FALSE
){
    position = match.arg(position)
    new_label = match.arg(new_label)
    lapply(x, net, ..., position = position, prefix = prefix, new_label = new_label, add = add)
}

#' @export
net.data.frame = function(x, ..., 
                          position = c("below", "above", "top", "bottom"), 
                          prefix = "TOTAL ", 
                          new_label = c("all", "range", "first", "last"),
                          add = FALSE
){
    position = match.arg(position)
    new_label = match.arg(new_label)
    
    lapply(x, net, ..., position = position, prefix = prefix, new_label = new_label, add = add)
    
}

#' @export
#' @rdname net
subtotal = function(x, ..., 
                    position = c("below", "above", "top", "bottom"), 
                    prefix = "TOTAL ", 
                    new_label = c("all", "range", "first", "last"),
                    add = TRUE){
    position = match.arg(position)
    new_label = match.arg(new_label)
    net(x, ..., 
        position = position,
        prefix = prefix,
        new_label = new_label,
        add = add
        )
    
}


#' @rdname net
#' @export
tab_net_cells = function(data, ..., 
                         position = c("below", "above", "top", "bottom"), 
                         prefix = "TOTAL ", 
                         new_label = c("all", "range", "first", "last")
                         ){
    inherits(data, "intermediate_table") || stop(
           "'tab_net_*' - argument 'data' need to be result of 'tab_cells', 'tab_cols' or 'tab_rows'.") 
    
    curr = data[[CELL_VAR]]
    expr = substitute(
        net(x = curr, ..., position = position, prefix = prefix, new_label = new_label)
    )
    data[[CELL_VAR]] = calculate_internal(data[[DATA]], expr, parent.frame())
    data
}

#' @rdname net
#' @export
tab_net_cols = function(data, ..., 
                        position = c("below", "above", "top", "bottom"), 
                        prefix = "TOTAL ", 
                        new_label = c("all", "range", "first", "last")
){
    inherits(data, "intermediate_table") || stop(
           "'tab_net_*' - argument 'data' need to be result of 'tab_cells', 'tab_cols' or 'tab_rows'.") 
    curr = data[[COL_VAR]]
    expr = substitute(
        net(x = curr, ..., position = position, prefix = prefix, new_label = new_label)
    )
    data[[COL_VAR]] = calculate_internal(data[[DATA]], expr, parent.frame())
    data
}


#' @rdname net
#' @export
tab_net_rows = function(data, ..., 
                         position = c("below", "above", "top", "bottom"), 
                         prefix = "TOTAL ", 
                         new_label = c("all", "range", "first", "last")
){
    inherits(data, "intermediate_table") || stop( 
           "'tab_net_*' - argument 'data' need to be result of 'tab_cells', 'tab_cols' or 'tab_rows'.") 
    curr = data[[ROW_VAR]]
    expr = substitute(
        net(x = curr, ..., position = position, prefix = prefix, new_label = new_label)
    )
    data[[ROW_VAR]] = calculate_internal(data[[DATA]], expr, parent.frame())
    data
}

################

#' @rdname net
#' @export
tab_subtotal_cells = function(data, ..., 
                         position = c("below", "above", "top", "bottom"), 
                         prefix = "TOTAL ", 
                         new_label = c("all", "range", "first", "last")
){
    inherits(data, "intermediate_table") || stop(
           "'tab_subtotal_*' - argument 'data' need to be result of 'tab_cells', 'tab_cols' or 'tab_rows'.") 
    curr = data[[CELL_VAR]]
    expr = substitute(
        subtotal(x = curr, ..., position = position, prefix = prefix, new_label = new_label)
    )
    data[[CELL_VAR]] = calculate_internal(data[[DATA]], expr, parent.frame())
    data
}


#' @rdname net
#' @export
tab_subtotal_cols = function(data, ..., 
                        position = c("below", "above", "top", "bottom"), 
                        prefix = "TOTAL ", 
                        new_label = c("all", "range", "first", "last")
){
    inherits(data, "intermediate_table") || stop(
           "'tab_subtotal_*' - argument 'data' need to be result of 'tab_cells', 'tab_cols' or 'tab_rows'.") 
    curr = data[[COL_VAR]]
    expr = substitute(
        subtotal(x = curr, ..., position = position, prefix = prefix, new_label = new_label)
    )
    data[[COL_VAR]] = calculate_internal(data[[DATA]], expr, parent.frame())
    data
}


#' @rdname net
#' @export
tab_subtotal_rows = function(data, ..., 
                        position = c("below", "above", "top", "bottom"), 
                        prefix = "TOTAL ", 
                        new_label = c("all", "range", "first", "last")
){
    inherits(data, "intermediate_table") || stop(
           "'tab_subtotal_*' - argument 'data' need to be result of 'tab_cells', 'tab_cols' or 'tab_rows'.") 
    curr = data[[ROW_VAR]]
    expr = substitute(
        subtotal(x = curr, ..., position = position, prefix = prefix, new_label = new_label)
    )
    data[[ROW_VAR]] = calculate_internal(data[[DATA]], expr, parent.frame())
    data
}


