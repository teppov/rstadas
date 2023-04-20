

#' Map columns to variables based on the given mapping.
#' The original raw columns are kept by default.
#'
#' @param df A data frame
#' @param mapping A named vector: c( variable_name = column_name )
#' @param keep_originals A logical whether to keep the originals or not
#'
#' @return
#' @export
#'
#' @examples
td_varmap <- function(
        df,
        mapping,
        keep_originals = TRUE
) {

    # Check that all defined columns exist in the data
    # (all non-NA names in the mapping should exist)
    defined_colnames <- na.omit( mapping )
    if( !all( defined_colnames %in% names( df ) ) ) {
        stop( paste0(
            'All defined columns do not exist in the data: ',
            paste(
                mapping[!defined_colnames %in% names( df )],
                collapse = ', '
            )
        ) )
    }

    # Loop over all defined variable names and copy the respective col
    for( name in names( mapping ) ) {
        colname <- mapping[[name]]
        if( is.na( colname ) ) {
            # The variable has no column in the data: fill with NA
            df[name] <- NA_character_
        } else {
            df <- df %>%
                mutate( !!name := .data[[colname]] )
        }
    }

    # If keeping the originals, just relocate the defined first
    if( keep_originals ) {
        df <- df %>%
            relocate( names( mapping ) )
    } else {
        df <- df %>%
            select( names( mapping ) )
    }

    df
}


#' From the given Stadas specs, get a named vector that maps
#' column names to variable names: c( variable_name = column_name )
#'
#' @param specs A valid Stadas specification
#' @param colname The name of the column in the specification
#'                that holds the desired column names
#'
#' @return
#' @export
#'
#' @examples
td_get_varmapping <- function(
        specs,
        colname
) {

    if( colname %in% names( specs$variables ) ) {
        # Map columns names to variable names
        deframe( select(
            specs$variables,
            varname,
            colname
        ) )

    } else {
        # No mapping column => use only variable names
        deframe( select(
            specs$variables,
            varname,
            varname
        ) )
    }
}


#' An example of a function for extracting metadata from
#' the path of a data file and adding the metadata into
#' the data frame.
#'
#' @param df A data frame into which metadata is added.
#' @param file_path The path of the file from which the
#'                  data frame hase been read.
#'
#' @return
#' @export
#'
#' @examples
td_metadata_year <- function(
        df,
        file_path = NULL,
        metadata_df = NULL
) {

    df %>%
        mutate(
            # Extract the year from the base name of the file
            year = as.integer(
                str_extract( basename( file_path ), '[0-9]{4}' )
            )
        ) %>%
        # Relocate the variable "year" as first
        relocate( year )
}


td_get_mapping_key <- function(
        mapping_key = NULL,
        mapping_key_string = NULL,
        mapping_key_pattern = NULL
) {
    if( is.null( mapping_key ) ) {
        if( ! is.null( mapping_key_pattern ) ) {
            # Extract the mapping key from the string
            mapping_key <- str_extract(
                mapping_key_string,
                mapping_key_pattern
            )
        }
    }
    mapping_key
}


td_get_mapping_colname <- function(
        prefix = 'colname',
        mapping_key = NULL,
        mapping_key_string = NULL,
        mapping_key_pattern = NULL
) {

    mapping_key <- td_get_mapping_key(
        mapping_key,
        mapping_key_string,
        mapping_key_pattern
    )

    paste( c( prefix, mapping_key ), collapse = '_' )
}


td_read_data_file <- function(
        file_path,
        specs,

        read_func = read_xlsx,

        drop_na_colnames = NULL,

        varmap_func = td_varmap,

        mapping_key = NULL,
        mapping_key_pattern = NULL,

        metadata_func = NULL,
        metadata_df = NULL,

        keep_originals = TRUE,

        # Additional arguments to the read_func
        ...
) {

    var_mapping_colname <- td_get_mapping_colname(
        mapping_key = mapping_key,
        mapping_key_string = file_path,
        mapping_key_pattern = mapping_key_pattern
    )
    cat_mapping_key <- td_get_mapping_key(
        mapping_key = mapping_key,
        mapping_key_string = file_path,
        mapping_key_pattern = mapping_key_pattern
    )
    na_mapping_colname <- td_get_mapping_colname(
        prefix = 'na',
        mapping_key = mapping_key,
        mapping_key_string = file_path,
        mapping_key_pattern = mapping_key_pattern
    )

    df <- read_func(
        file_path,
        # Additional arguments to the reading function
        ...
    )

    if( !is.null( drop_na_colnames ) ) {
        df <- drop_na( df, any_of( drop_na_colnames ) )
    }

    df %>%

        varmap_func(
            mapping = td_get_varmapping( specs, var_mapping_colname ),
            keep_originals = keep_originals
        ) %>%

        td_parse_all_numeric( specs, na_mapping_colname ) %>%

        td_parse_all_categorical( specs, cat_mapping_key ) %>%

        td_if_execute(
            metadata_func,
            !is.null( metadata_func ),
            file_path = file_path,
            metadata_df = metadata_df
        )
}


td_read_data_dir <- function(
        dir_path,
        specs,

        read_func = read_xlsx,

        drop_na_colnames = NULL,

        varmap_func = td_varmap,

        mapping_key = NULL,
        mapping_key_pattern = NULL,

        metadata_func = NULL,
        metadata_df = NULL,

        keep_originals = TRUE,

        # Additional arguments to the reading function
        ...
) {

    file_paths <- list.files( dir_path, full.names = TRUE )

    if( length( file_paths ) == 0 ) {
        stop( paste0(
            'No files found in the directory "', dir_path, '"!'
        ) )
    }

    lapply(

        file_paths,

        # Apply td_read_file() function to all files
        td_read_data_file,

        # Arguments for td_read_file()
        specs = specs,
        read_func = read_func,
        drop_na_colnames = drop_na_colnames,

        varmap_func = varmap_func,
        mapping_key = mapping_key,
        mapping_key_pattern = mapping_key_pattern,

        metadata_func = metadata_func,
        metadata_df = metadata_df,

        keep_originals = keep_originals,

        # Additional arguments to the read_func
        ...
    ) %>%

        bind_rows()
}



