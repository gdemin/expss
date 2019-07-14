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
#' @param add logical. Auxiliary argument. Should we add subtotal to categories or replace categories with a net? 
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
#' cro(subtotal(ol, TOP = 6:7, BOTTOM = 1:3, position = "top"))
#' # autolabelling
#' cro(subtotal(ol, 6:7, 1:3))
#' # replace original codes and another way of autolabelling
#' cro(net(ol, 6:7, 1:3, new_label = "range", prefix = "NET "))
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
net.numeric = function(x, ..., 
                       position = c("below", "above", "top", "bottom"), 
                       prefix = "TOTAL ", 
                       new_label = c("all", "range", "first", "last"),
                       add = FALSE){
    
    # overlap = TRUE?
    # we need this because all values should have labels
    all_values = unique(x, nmax = 1) 
    possible_values = all_values %d% NA
    val_lab(x) = val_lab(x) %u% setNames(possible_values, possible_values)
    position = match.arg(position)  
    new_label = match.arg(new_label)
    
    args = list(...)
    if(identical(position, "top")){
        args = rev(args)
    }
    arg_names = names(args)
    if(is.null(arg_names)) arg_names = rep("", length(args))
    first_col = x
    other_cols = vector(mode = "list", length = length(args))
    for(i in seq_along(args)){
        curr_net = args[[i]]
        inherits(curr_net, "formula") && 
            stop("'net' -  manual coding for subtotals via formulas currently not supported.")
        label = arg_names[i]
        possible_values = possible_values[!is.na(possible_values)]
        if(!inherits(curr_net, "criterion") && !is.atomic(curr_net)) {
            curr_net = as.criterion(curr_net)
            
        }  
        if(inherits(curr_net, "criterion")){
            source_codes = sort(all_values %i% curr_net)
        } else {
            # we want this to provide possibility for custom sorting
            # all items will be in the order as declasred in subtotal
            source_codes = curr_net
        }  
        new_codes = transform_codes(source_codes, possible_values)
        target = find_code(new_codes, possible_values %d% source_codes, position = position)
        
        frm_net = curr_net ~ target
        
        recode_args = list(x, frm_net, new_label = new_label, with_labels = TRUE)
        names(recode_args)[[2]] = label 
        # browser()
        res = do.call(recode, recode_args)
        possible_values = union(possible_values, unique(res, nmax = 1)) 
        if(!is.null(val_lab(res)) && (is.null(label) || is.na(label) || identical(label, ""))) {
            names(val_lab(res)) = paste0(prefix, names(val_lab(res)))
        }
        
        frm_net[[3]] = NA
        recode(first_col, with_labels = TRUE) = frm_net
        if(add){
            items = recode(x, from_to(source_codes, new_codes), with_labels = TRUE) 
            other_cols[[i]] = list(items, res)
            possible_values = union(possible_values, unique(items, nmax = 1))
        } else {
            other_cols[[i]] = list(res)    
        }
        
    }
    other_cols = unlist(other_cols, recursive = FALSE, use.names = FALSE)
    res = c(list("v1" = first_col), setNames(other_cols, paste0("v", seq_along(other_cols) + 1)))
    # if(names(res)
    do.call(mrset, res)
}




#' @export
net.default = function(x, ..., 
                       position = c("below", "above", "top", "bottom"), 
                       prefix = "TOTAL ", 
                       new_label = c("all", "range", "first", "last"),
                       add = FALSE){
    
    # overlap = TRUE?
    position = match.arg(position)  
    new_label = match.arg(new_label)
    if(is.labelled(x) && is.numeric(x)){
        return(net.numeric(x, ..., position = position, prefix = prefix, new_label = new_label, add = add))
    }
    # factor has usefull property that we can recode it as character
    # but internally it have numeric codes which we can utilize to position totals 
    varlab = var_lab(x)
    if(!is.factor(x)) x = factor(x, nmax = 1)
    all_values = unique(x, nmax = 1)
    possible_values = as.numeric(all_values) %d% NA
    args = list(...)
    if(identical(position, "top")){
        args = rev(args)
    }
    arg_names = names(args)
    if(is.null(arg_names)) arg_names = rep("", length(args))
    labelled_x = as.labelled(x, label = varlab)
    first_col = labelled_x
    other_cols = vector(mode = "list", length = length(args))
    for(i in seq_along(args)){
        
        curr_net = args[[i]]
        inherits(curr_net, "formula") && 
            stop("'net' -  manual coding for subtotals via formulas currently not supported.")
        
        label = arg_names[i]
        possible_values = possible_values[!is.na(possible_values)]
        if(!inherits(curr_net, "criterion") && !is.atomic(curr_net)) {
            curr_net = as.criterion(curr_net)
            
        }  
        if(inherits(curr_net, "criterion")){
            source_codes = as.numeric(sort(all_values %i% curr_net))
        } else {
            # we want this to provide possibility for custom sorting
            # all items will be in the order as declasred in subtotal
            source_codes = match(curr_net, levels(x)) %d% NA
        }  
        new_codes = transform_codes(source_codes, possible_values)
        target = find_code(new_codes, possible_values %d% source_codes, position = position)
        
        frm_net = source_codes ~ target
        
        recode_args = list(labelled_x, frm_net, new_label = new_label, with_labels = TRUE)
        names(recode_args)[[2]] = label 
        
        res = do.call(recode, recode_args)
        possible_values = union(possible_values, unique(res, nmax = 1))  
        if(!is.null(val_lab(res)) && (is.null(label) || is.na(label) || identical(label, ""))) {
            names(val_lab(res)) = paste0(prefix, names(val_lab(res)))
        }
        frm_net[[3]] = NA
        recode(first_col, with_labels = TRUE) = frm_net
        if(add){
            items = recode(labelled_x, from_to(source_codes, new_codes), with_labels = TRUE) 
            other_cols[[i]] = list(items, res)
            possible_values = union(possible_values, unique(items, nmax = 1))
        } else {
            other_cols[[i]] = list(res)    
        }
    }
    other_cols = unlist(other_cols, recursive = FALSE, use.names = FALSE)
    res = c(list("v1" = first_col), setNames(other_cols, paste0("v", seq_along(other_cols) + 1)))
    # if(names(res)
    do.call(mrset, res)
}


#' @export
net.category = function(x, ..., 
                        position = c("below", "above", "top", "bottom"), 
                        prefix = "TOTAL ", 
                        new_label = c("all", "range", "first", "last"),
                        add = FALSE
){
    extract_all_values = function(multiple_category) {
        unique(unlist(lapply(multiple_category, unique, nmax = 1), use.names = FALSE))
    }
    all_values = extract_all_values(x)
    possible_values = all_values %d% NA
    val_lab(x) = val_lab(x) %u% setNames(possible_values, possible_values)
    position = match.arg(position)  
    new_label = match.arg(new_label)
    
    args = list(...)
    if(identical(position, "top")){
        args = rev(args)
    }
    arg_names = names(args)
    if(is.null(arg_names)) arg_names = rep("", length(args))
    first_col = x
    other_cols = vector(mode = "list", length = length(args))
    for(i in seq_along(args)){
        possible_values = possible_values[!is.na(possible_values)]
        curr_net = args[[i]]
        inherits(curr_net, "formula") && 
            stop("'net' -  manual coding for subtotals via formulas currently not supported.") 
        label = arg_names[i]
        
        if(!inherits(curr_net, "criterion") && !is.atomic(curr_net)) {
            curr_net = as.criterion(curr_net)
            
        }  
        if(inherits(curr_net, "criterion")){
            source_codes = sort(all_values %i% curr_net)
        } else {
            # we want this to provide possibility for custom sorting
            # all items will be in the order as declasred in subtotal
            source_codes = curr_net
        }  
        new_codes = transform_codes(source_codes, possible_values)
        target = find_code(new_codes, possible_values %d% source_codes, position = position)
        
        frm_net = curr_net ~ target
        
        recode_args = list(x, frm_net, new_label = new_label, with_labels = TRUE)
        names(recode_args)[[2]] = label 
        # TODO we do this only for labels
        # should be rewritten
        val_lab_res = val_lab(do.call(recode, recode_args))
        res = target*((x %has% curr_net) | NA)
        val_lab(res) = val_lab_res
        possible_values = union(possible_values, unique(res, nmax = 1)) 
        if(!is.null(val_lab(res)) && (is.null(label) || is.na(label) || identical(label, ""))) {
            names(val_lab(res)) = paste0(prefix, names(val_lab(res)))
        }
        frm_net[[3]] = NA
        recode(first_col, with_labels = TRUE) = frm_net
        if(add){
            items = recode(x, from_to(source_codes, new_codes), with_labels = TRUE) 
            other_cols[[i]] = list(items, res)
            possible_values = union(possible_values, extract_all_values(items))
        } else {
            other_cols[[i]] = list(res)    
        }
    }
    other_cols = unlist(other_cols, recursive = FALSE, use.names = FALSE)
    res = c(list("v1" = first_col), setNames(other_cols, paste0("v", seq_along(other_cols) + 1)))
    # if(names(res)
    do.call(mrset, res)  
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


find_code = function(codes, other_values, position = c("below", "above", "bottom", "top")){
    position = match.arg(position)
    switch(position, 
           above = {
               # We need number x: x < min_code and x > next_code.
               # If some of the boundaries is NA we ignore them.
               # All codes are always sorted in ascending order. So if 
               # we want position above we need new code which is less than existing codes.
               min_code = suppressWarnings(min(codes, na.rm = TRUE))
               next_code = suppressWarnings(max(other_values[other_values < min_code], na.rm = TRUE))
               res = c((min_code + next_code)/2, # no NAs 
                       min_code - 1,             # next_code is NA
                       suppressWarnings(min(other_values, na.rm = TRUE)) - 1, # min_code is NA
                       0)     # all is NA
               res[is.finite(res)][1]
               
               
           },
           below = {
               # we need number x: x > max_code and x < next_code
               # if some of the boundaries is NA we ignore them
               max_code = suppressWarnings(max(codes, na.rm = TRUE))
               next_code = suppressWarnings(min(other_values[other_values > max_code], na.rm = TRUE))
               res = c((max_code + next_code)/2, # no NAs 
                       max_code + 1,             # next_code is NA
                       suppressWarnings(max(other_values, na.rm = TRUE)) + 1, # min_code is NA
                       0)     # all is NA
               res[is.finite(res)][1]
               
           },
           top = {
               min_code = suppressWarnings(min(c(codes, other_values), na.rm = TRUE))
               if(is.finite(min_code)){
                   min_code - 1
               } else {
                   0
               }
           },
           bottom = {
               max_code = suppressWarnings(max(c(codes, other_values), na.rm = TRUE))
               if(is.finite(max_code)){
                   max_code + 1
               } else {
                   0
               }
           }
           
    )
}


transform_codes = function(codes, possible_values){
    # here we move all codes to one sequence
    n_codes = length(codes)
    if(n_codes == 1) return(codes)
    other_possible_values = possible_values %d% codes
    if(length(other_possible_values)==0) return(codes)
    min_code = suppressWarnings(min(codes, na.rm = TRUE))
    max_code = suppressWarnings(max(codes, na.rm = TRUE))
    # we have connected codes, no other codes betweeb
    if(!any(other_possible_values>min_code & other_possible_values<max_code)) return(codes)
    last_code = suppressWarnings(
        min(other_possible_values[other_possible_values>min_code & other_possible_values<max_code], na.rm = TRUE)
        )
    delta = (last_code - min_code)/n_codes
    new_max = last_code - delta/2
    seq(min_code, new_max, length.out = n_codes)
}