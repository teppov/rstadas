
#' Apply a set of operations to add new variables in a data frame.
#'
#' @param df a data frame into which the new variables are added
#' @param operations_df a data frame with at least columns
#'                      "newvarname" and "operation"
#'
#' @return the data frame with the new variables
#' @export
#'
#' @examples
td_preprocess <- function( df, operations_df, mapping_key = NULL ) {

    if( is.null( mapping_key ) ) {
        # If no mapping key, use all preprocess operations
        ops_df <- operations_df
    } else {
        # Filter preprocess operations with the mapping key
        ops_df <- operations_df %>%
            filter( mappingkey == mapping_key )
    }

    # Loop over all rows in the operations data frame
    for( i in 1:nrow( ops_df ) ) {
        # Build a text expression from the
        # new variable name and operation,
        # and evaluate the  expression within the data frame
        df <- within( df, eval( parse( text = paste0(
            ops_df[i, 'newvarname'],
            ' <- ',
            ops_df[i, 'operation']
        ) ) ) )
    }

    df
}

