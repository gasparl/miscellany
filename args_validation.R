
## function parameter argument validations

validate_args = function(func_used, evaled_args) {
    feedback = ''
    for (part_feed in evaled_args) {
        feedback = paste0(feedback, part_feed)
    }
    if (feedback != '') {
        print(deparse(func_used))
        func_used = gsub("\\s+", " ", paste(deparse(func_used), collapse = " "))
        print(func_used)
        feedback = paste0(
            "Arguments are not correct in the '",
            func_used,
            "' function:",
            feedback,
            '\n... Hint: enter help(',
            strsplit(func_used, "\\(")[[1]][1],
            ') for detailed function info.'
        )
        stop(feedback, call. = F)
    }
}

val_arg = function(arg_val, req_types, req_length = 99, opts = NULL) {
    failed = FALSE
    arg_name = deparse(substitute(arg_val))
    if (length(arg_val) > 1 ) {
        if (req_length == 1) {
            failed = TRUE
        }
    } else if (req_length == 0) {
        failed = TRUE
    }
    valid_types = c('char', 'num', 'bool', 'null', 'df')
    if (!all(req_types %in% valid_types)) {
        stop(
            'invalid req_types: ',
            paste(req_types, collapse = ', '),
            '\nshould be: ',
            paste(valid_types, collapse = ', ')
        )
    }
    req_types = replace(req_types, req_types=='char', 'character')
    req_types = replace(req_types, req_types=='num', 'double')
    req_types = replace(req_types, req_types=='bool', 'logical')
    req_types = replace(req_types, req_types=='null', 'NULL')
    req_types = replace(req_types, req_types=='df', 'data.frame')
    if ((!typeof(arg_val) %in% req_types)
        && (!('data.frame' %in% req_types &&
              is.data.frame(arg_val)))) {
        failed = TRUE
    } else if (typeof(arg_val) == 'character' &&
               (!is.null(opts)) && (!arg_val %in% opts)) {
        failed = TRUE
    }
    if (failed == TRUE) {
        req_types = replace(req_types, req_types == 'character', '"character" (string)')
        req_types = replace(req_types, req_types == 'double', '"double" (numeric)')
        req_types = replace(req_types, req_types == 'logical', '"logical" (boolean)')
        req_types = replace(req_types, req_types == 'data.frame', '"data.frame"')
        if (!is.null(opts)) {
            if (suppressWarnings(all(!is.na(as.numeric(opts))))) {
                opts_add = paste0(
                    ' The only acceptable strings or numbers are "',
                    paste(opts, collapse = '", or "'),
                    '".'
                )
            } else {
                opts_add = paste0(' The only acceptable strings are "',
                                  paste(opts, collapse = '", or "'),
                                  '".')
            }
        } else {
            opts_add = ''
        }
        if (req_length == 1) {
            len_add = ' must be a single element, and'
        } else if (req_length == 0) {
            len_add = ' must be a vector, and'
        } else {
            len_add = ''
        }
        arg_msg = paste0(
            '\nThe argument "',
            arg_name,
            '"',
            len_add,
            ' must be ',
            paste(req_types, collapse = ', or '),
            '.',
            opts_add
        )
        return(arg_msg)
    } else {
        return('')
    }
}

# example (see actual examples in any neatStats functions)

func_example = function(num_a, num_b, yes_no, sometext = '') {
    validate_args(match.call(), list(
        val_arg(num_a, 'num', 1),
        val_arg(num_b, 'num'),
        val_arg(yes_no, c('bool', 'null'), 0),
        val_arg(sometext, c('char'), 1, c('hi', 'meh', ''))
    ))
    cat('Arguments checked.', fill = TRUE)
    # now the function can actually run
    print(num_a)
    print(num_b)
    print(yes_no)
    print(sometext)
}

func_example(1, c(2, 3), c(T, F), 'meh')
func_example(1, c(2, 3), c(T, F, T))
func_example(1, c(2, 3), c(T, F), 'mehh')
func_example(c(2, 3), 1, c(T, F, T, T))
func_example(c(2, 1), c(2, 3), T, 'meh')
func_example(c(2, 1), c(2, 3), c(T, T), 'x')