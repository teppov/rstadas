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


#' Test if x contains integers. F.ex. "2.0" is not an integer,
#' while "2" is. x can be numeric or character.
#'
#' @param x array of objects to be tested.
#'
#' @return
#' @export
#'
#' @examples
is.td_integer <- function( x ) {
    dplyr::if_else(
        is.na( x ),
        NA,
        grepl( '^0$|(^-?[1-9]+[0-9]*$)', x )
    )
}


is.td_decimal <- function( x ) {
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

