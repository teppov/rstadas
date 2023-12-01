#' Calculate a row-wise summary across variables.
#' Return `NA` if nothing found with the `tidy_select`.
#' To be used inside `mutate()`, for example:
#'
#' df %>%
#'     mutate(
#'         mean__ce_det = mean(
#'             c_across( any_of( c(
#'                 "num__ce_det_01", "num__ce_det_02"
#'             ) ) ),
#'             na.rm = TRUE
#'         )
#' =>
#'         mean__ce_det = bwr_summary_across(
#'             any_of( c(
#'                 "num__ce_det_01", "num__ce_det_02"
#'             ) ),
#'             na.rm = TRUE
#'         )
#'     )
#'
#' @param tidy_select A tidyselect selection of variables
#' @param summary_func A summary function to be used across the variables.
#'                     Default: `mean`
#' @param ... Additional arguments for the summary function,
#'            for example `na.rm = TRUE`
#'
#' @return The summary calculation, or `NA` if nothing found with `tidy_select`.
#' @export
#'
#' @examples
#'
td_summary_across <- function(
        tidy_select,
        summary_func = mean,
        na_ratio = 0.5,
        ...
) {

    cx <- c_across( {{ tidy_select }} )

    if( is.null( cx ) ) return( NA )

    nof_vars <- length( cx )

    if_else(
        sum( is.na( cx ) ) / nof_vars < na_ratio,
        summary_func( cx, ... ),
        NA_real_
    )
}


#' Execute a function with an object as the first argument
#' if execute is TRUE. Convenient for pipes.
#'
#' @param x The object as the first argument to the func
#' @param execute Whether or not to execute the func
#' @param func The function to be executed
#' @param ... Additional arguments to the function
#'
#' @return The result from the function, or x
#' @export
#'
#' @examples
td_if_execute <- function( x, func, execute = TRUE, ... ) {
    if( execute ) {
        func( x, ... )
    } else {
        x
    }
}


#' Reorder factor levels by sorting along another variable
#'
#' @param df a data frame with variables x and y
#' @param .f A factor (or character vector).
#' @param x The levels of .f are reordered so that the values of
#'          .fun( .x, ... ) are in ascending order.
#' @param .fun A summary function.
#' @param .desc Order in descending order?
#' @param .ordered Will the factor be an ordered factor?
#' @param ... Other arguments passed on to .fun, such as na.rm = TRUE.
#'
#' @return
#' @export
#'
#' @examples
td_fct_reorder <- function(
        df,
        .f,
        .x,
        .fun = mean,
        .desc = TRUE,
        .ordered = TRUE,
        ...
) {

    smr <- df %>%
        group_by( {{ .f }} ) %>%
        summarise(
            value = .fun( {{ .x }}, ... )
        ) %>%
        drop_na()

    if( .desc ) {
        factor_levels <- smr %>%
            arrange( desc( value ) ) %>%
            pull( {{ .f }} )
    } else {
        factor_levels <- smr %>%
            arrange( value ) %>%
            pull( {{ .f }} )
    }

    df %>%
        mutate(
            {{ .f }} := factor(
                {{ .f }},
                levels = factor_levels,
                ordered = .ordered
            )
        )
}


#' Test if x contains whole numbers (2, -1, 3.0, 46 etc.).
#'
#' The function is copied from the R Documentation: "integer".
#'
#' @param x The item to test.
#' @param tol Default: `.Machine$double.eps^0.5`.
#'
#' @return A logical scalar or vector depending on the input.
#'
#' @examples
#' is.wholenumber( 1 ) # is TRUE
#' ( x <- seq( 1, 5, by = 0.5 ) )
#' is.wholenumber( x ) # -->  TRUE FALSE TRUE ...
#'
is.wholenumber <- function(
        x,
        tol = .Machine$double.eps^0.5
) {
    abs( x - round( x ) ) < tol
}


#'
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is.td_wholenumber <- function( x ) {
    tryCatch(
        expr = { is.wholenumber( as.numeric( x ) ) },
        # If `as.numeric()` raises a warning,
        # it has introduced NAs by coercion (i.e., not a numeric)
        warning = function( w ) { FALSE }
    )
}


#' Test if x contains discrete, i.e. integer, values.
#' F.ex. "2.0" is not an integer, while "2" is.
#' x can be numeric or character.
#'
#' @param x array of objects to be tested.
#'
#' @return
#' @export
#'
#' @examples
is.discrete <- function( x ) {
    dplyr::if_else(
        is.na( x ),
        NA,
        grepl( '^0$|(^-?[1-9]+[0-9]*$)', x )
    )
}


#' Test if x contains continuous numeric values.
#' All numeric values are considered continuous.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is.continuous <- function( x ) {
    sapply(
        x,
        function( x ) {
            tryCatch(
                expr = { is.numeric( as.numeric( x ) ) },
                # If `as.numeric()` raises a warning,
                # NAs introduced by coercion (i.e., not a numeric)
                warning = function( w ) { FALSE }
            )
        }
    )
}


#' Replace values in data with a replacement value.
#'
#' @param data the data in which to replace the values
#' @param values the values to be replaces
#' @param replacement the replacement value
#'
#' @return
#' @export
#'
#' @examples
replace <- function( data, values, replacement ) {
    data[data %in% values] <- replacement
    data
}


#' Read all sheets in an Excel file as text tibbles into a named list.
#'
#' @param excel_path
#'
#' @return list of tibbles named with the Excel sheet names
#' @export
#'
#' @examples
td_read_excel_to_list <- function( excel_path ) {

    # Get the sheet names of the Excel file
    sheet_names <- readxl::excel_sheets( excel_path )

    # Read all sheets into a list
    table_list <- lapply(
        sheet_names,
        function( sheet_name ) readxl::read_excel(
            path = excel_path,
            sheet = sheet_name,
            col_types = 'text'
        )
    )

    # Name the list elements with the sheet names
    names( table_list ) <- sheet_names

    table_list
}


td_validate_df <- function(
        df,
        validator,
        msg = NULL,
        cf_raise = 'all',
        ref = NULL,
        print_summary = FALSE
) {

    checks <- validate::confront(
        df,
        validator,
        raise = cf_raise,
        ref = ref
    )

    if( print_summary ) {
        print( validate::summary( checks )[1:7] )
    }

    if( all( checks, na.rm = TRUE ) ) {
        return( df )

    } else {
        stop( paste(
            c( 'Validation failed!', msg ), collapse = '\n'
        ) )
    }
}


td_validate_df_list <- function(
        df_list,
        validator,
        msg = NULL,
        cf_raise = 'all',
        ref = NULL,
        print_summary = FALSE
) {

    check_list <- lapply(
        df_list,
        validate::confront,
        x = validator,
        raise = cf_raise,
        ref = ref
    )

    if( print_summary ) {
        sapply(
            check_list,
            function( c ) {
                print( validate::summary( c )[1:7] )
            }
        )
    }

    # If all checks are true, return `TRUE`
    if( all( sapply( check_list, all, na.rm = TRUE ) ) ) {
        return( df_list )
    }

    stop( paste(
        c( 'Validation failed!', msg ), collapse = '\n'
    ) )
}

