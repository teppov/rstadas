
SETUP_TABLETYPES <- c( 'variable', 'rule' )

VAR_DATATYPES <- c( 'integer', 'decimal', 'categorical', 'text' )

SETUP_VALIDATOR <- validate::validator(
    tabletype_valid = tabletype %in% tabletypes,
    tablename_valid = tablename %in% tablenames
)

PREP_VALIDATOR <- validate::validator(
    mappingkey_valid = is.character( mappingkey ),
    newvarname_notna = !is.na( newvarname ),
    operation_notna = !is.na( operation )
)

VAR_VALIDATOR <- validate::validator(
    varname_notna = !is.na( varname ),
    varname_unique = all_unique( varname ),
    datatype_valid = datatype %in% datatypes,
    unique_valid = unique %in% c( NA, 'unique' ),
    nona_valid = nona %in% c( NA, 'nona' ),
    categorytable_valid = categorytable %in% tablenames
)

CAT_VALIDATOR <- validate::validator(
    categoryset_notna = !is.na( categoryset ),
    categoryname_notna = !is.na( categoryname ),
    colortable_valid = colortable %in% tablenames
)

COL_VALIDATOR <- validate::validator(
    colorname_notna = !is.na( colorname ),
    colorname_unique = all_unique( colorname ),
    colorhex_valid = grepl( colorhex_regex, colorhex )
)

RULE_VALIDATOR <- validate::validator(
    rulename_notna = !is.na( rulename ),
    # TODO: how to check rules?
    # https://cran.r-project.org/web/packages/validate/vignettes/cookbook.html#74_Validation_rule_syntax
    rule_valid = !is.na( rule ),
    label_valid = is.character( label ),
    description_valid = is.character( description )
)

#' Create a valid Stadas specification (a named list of tibbles)
#' from a list of data frames.
#'
#' @param table_list a list of data frames
#'
#' @return Stadas specification: a named list of tibbles
#' @export
#'
#' @examples
td_create_specs <- function( table_list ) {

    if( !'setup' %in% names( table_list ) ) {
        stop( 'Given table list does not contain "setup" table!' )
    }

    specs = list()


    # Setup
    #############################################

    # Pick the setup table
    specs$setup <- table_list$setup %>%
        # Keep only rows where tablename is not NA
        dplyr::filter( !is.na( tablename ) )

    # Validate
    td_validate_df(
        specs$setup,
        SETUP_VALIDATOR,
        'Setup table not valid.',
        ref = list(
            tabletypes = SETUP_TABLETYPES,
            tablenames = names( table_list )
        )
    )


    # Preprocess
    #############################################

    if( 'preprocess' %in% names( table_list ) ) {

        specs$preprocess <- table_list$preprocess %>%
            # Keep only rows where newvarname is not NA
            dplyr::filter( !is.na( newvarname ) )

        # Validation
        td_validate_df(
            specs$preprocess,
            PREP_VALIDATOR,
            'Preprocess table not valid.'
        )
    }


    # Variables
    #############################################

    # Get the variable table names from the setup table
    variable_tablenames <- specs$setup %>%
        filter( tabletype == 'variable' ) %>%
        pull( tablename )

    # Bind variable tables
    specs$variables <- dplyr::bind_rows(
        table_list[variable_tablenames]
    ) %>%
        # Keep only rows where varname is not NA
        dplyr::filter( !is.na( varname ) )

    # Validation
    td_validate_df(
        specs$variables,
        VAR_VALIDATOR,
        'Variable table not valid.',
        ref = list(
            datatypes = VAR_DATATYPES,
            tablenames = names( table_list )
        )
    )


    if( 'categorytable' %in% names( specs$variables ) ) {

        # Categories
        #############################################

        # Get the category table names from the variable table
        category_tablenames <- specs$variables %>%
            filter( !is.na( categorytable ) ) %>%
            pull( categorytable ) %>%
            unique()

        # The category table names must be found in the given table list
        if( !all( category_tablenames %in% names( table_list ) ) ) {
            stop( paste0(
                'All named category tables not found
                in the given table list!\n',
                'The names in `categorytable` column:\n',
                paste0( category_tablenames, collapse = ', ' ), '\n',
                'The names in table list:\n',
                paste0( names( table_list ), collapse = ', ' ), '\n'
            ) )
        }

        # Bind category tables
        specs$categories <- dplyr::bind_rows(
            table_list[category_tablenames]
        ) %>%
            # Keep only rows where categoryset is not NA
            dplyr::filter( !is.na( categoryset ) )

        # Validation
        td_validate_df(
            specs$categories,
            CAT_VALIDATOR,
            'Category table not valid.',
            ref = list( tablenames = names( table_list ) )
        )

        # Get the category set names
        var_category_setnames <- specs$variables %>%
            filter( !is.na( categoryset ) ) %>%
            pull( categoryset ) %>%
            unique()
        cat_category_setnames <- specs$categories %>%
            filter( !is.na( categoryset ) ) %>%
            pull( categoryset ) %>%
            unique()

        # The category set names in the variable tables
        # must be found in the category tables
        if( !all( var_category_setnames %in% cat_category_setnames ) ) {
            stop( paste0(
                'All category sets named in variable tables not found
                in the category tables!\n',
                'The category set names in variable tables:\n',
                paste0( var_category_setnames, collapse = ', ' ), '\n',
                'The names in category tables:\n',
                paste0( cat_category_setnames, collapse = ', ' ), '\n'
            ) )
        }


        # Colors
        #############################################

        # Get the color table names from the category table
        color_tablenames <- specs$categories %>%
            filter( !is.na( colortable ) ) %>%
            pull( colortable ) %>%
            unique()

        if( length( color_tablenames ) > 0 ) {

            if( ! all( color_tablenames %in% names( table_list ) ) ) {
                stop( paste0(
                    'All named color tables not found
                in the given table list!\n',
                    'The names in `colortable` column:\n',
                    paste0( color_tablenames, collapse = ', ' ), '\n',
                    'The names in table list:\n',
                    paste0( names( table_list ), collapse = ', ' ), '\n'
                ) )
            }

            # Bind category tables
            specs$colors <- dplyr::bind_rows(
                table_list[color_tablenames]
            ) %>%
                # Keep only rows where colorname is not NA
                dplyr::filter( !is.na( colorname ) )

            # Validation
            td_validate_df(
                specs$color,
                COL_VALIDATOR,
                'Color table not valid.'
            )
        }

    }


    # Rules
    #############################################

    # Get the rule table names from the setup table
    rules_tablenames <- specs$setup %>%
        filter( tabletype == 'rule' ) %>%
        pull( tablename )

    if( length( rules_tablenames ) > 0 ) {

        # Bind the rules tables
        specs$rules <- dplyr::bind_rows(
            table_list[rules_tablenames]
        ) %>%
            # Keep only rows where rulename is not NA
            dplyr::filter( !is.na( rulename ) )

        # Validation
        td_validate_df(
            specs$rules,
            RULE_VALIDATOR,
            'Rule table not valid.'
        )
    }

    # Return
    specs
}


td_create_var_rules <- function(
        specs_var_row,
        specs,
        use_raw_values = FALSE,
        mapping_key = NULL
) {

    # Create a tibble with only a header
    # From Nik@SO: https://stackoverflow.com/a/60495352/7002525
    # Define the header as it appears in a
    # "validator" object turned data.frame
    rules <- c(
        'name', 'label', 'description', 'origin',
        'created', 'language', 'severity', 'rule'
    ) %>%
        rlang::rep_named( list( character() ) ) %>%
        as_tibble()

    varname <- specs_var_row['varname']
    datatype <- specs_var_row['datatype']

    if( datatype == 'integer' ) {
        rules <- rules %>%
            add_row(
                name = paste0( varname, '_int' ),
                rule = paste0( 'is.td_integer( ', varname, ' )' ),
                # rule = paste0( 'is.wholenumber( ', varname, ' )' ),
                # Use regex to identify an integer
                # rule = paste0(
                #     'field_format( ',
                #     varname,
                #     ', "^-?[1-9]+[0-9]*$", ',
                #     'type = "regex" )'
                # ),
                label = paste0( 'Integer variable: ', varname ),
                description = paste0(
                    'A rule for testing if the values of ',
                    'the variable "', varname, '" are integer numbers.'
                )
            )

    } else if( datatype == 'decimal' ) {
        rules <- rules %>%
            add_row(
                name = paste0( varname, '_dec' ),
                rule = paste0( 'is.td_decimal( ', varname, ' )' ),
                # rule = paste0( 'is.numeric( ', varname, ' )' ),
                # Use a regex to identify a decimal
                # rule = paste0(
                #     'field_format( ',
                #     varname,
                #     ', "^-?[1-9]+[0-9]*$", ',
                #     'type = "regex" )'
                # ),
                label = paste0( 'Decimal variable: ', varname ),
                description = paste0(
                    'A rule for testing if the values of ',
                    'the variable "', varname, '" are decimal numbers.'
                )
            )

    } else if( datatype == 'categorical' ) {

        if( use_raw_values ) {

            # Use the un-mapped, raw data values

            # Get the category values of the current category set
            category_values <- specs$categories %>%
                filter( categoryset == specs_var_row['categoryset'] ) %>%
                pull( paste(
                    c( 'mapping', mapping_key ), collapse = '_'
                ) )

            # Add rows to the `validator_rules` data frame
            rules <- rules %>%
                add_row(
                    name = paste0( varname, '_cat' ),
                    rule = paste0(
                        varname,
                        ' %vin% c( "',
                        paste( category_values, collapse = '", "' ),
                        '" )'
                    ),
                    label = paste0( 'Categorical variable: ', varname ),
                    description = paste0(
                        'A rule for testing the values of ',
                        'the variable "', varname, '" against ',
                        'a set of predefined category values.'
                    )
                )

        } else {

            # Use the actual, defined category names

            # Get the category names of the current category set
            category_names <- specs$categories %>%
                filter( categoryset == specs_var_row['categoryset'] ) %>%
                pull( categoryname )

            # Add rows to the `validator_rules` data frame
            rules <- rules %>%
                add_row(
                    name = paste0( varname, '_cat' ),
                    rule = paste0(
                        varname,
                        ' %vin% c( "',
                        paste( category_names, collapse = '", "' ),
                        '" )'
                    ),
                    label = paste0( 'Categorical variable: ', varname ),
                    description = paste0(
                        'A rule for testing the values of ',
                        'the variable "', varname, '" against ',
                        'a set of predefined category names.'
                    )
                )
        }

    } else {

        rules <- rules %>%
            add_row(
                name = paste0( varname, '_text' ),
                rule = paste0( 'is.character( ', varname, ' )' ),
                label = paste0( 'Text variable: ', varname ),
                description = paste0(
                    'A rule for testing if the values of ',
                    'the variable "', varname, '" are text.'
                )
            )
    }

    if( !is.na( specs_var_row['unique'] )
        & specs_var_row['unique'] == 'unique' ) {
        rules <- rules %>%
            add_row(
                name = paste0( varname, '_isunique' ),
                rule = paste0( 'is_unique( ', varname, ' )' ),
                label = paste0( 'Is unique: ', varname ),
                description = paste0(
                    'A rule for testing if the values of ',
                    'the variable "', varname, '" are unique.'
                )
            )
    }

    if( !is.na( specs_var_row['nona'] )
        & specs_var_row['nona'] == 'nona' ) {
        rules <- rules %>%
            add_row(
                name = paste0( varname, '_nona' ),
                rule = paste0( '!is.na( ', varname, ' )' ),
                label = paste0( 'No NA: ', varname ),
                description = paste0(
                    'A rule for testing if there are missing ("NA")
                    values for the variable "', varname, '".'
                )
            )
    }

    rules
}


#' Create a validator from a Stadas specs.
#'
#' @param specs Stadas specs as a list of tibbles
#' @param use_raw_values a boolean whether raw, un-mapped category
#'                       values should be used instead of the actual,
#'                       defined category names (the specs "categories"
#'                       table has to contain (a) mapping column(s))
#' @param mapping_key an optional key for which mapping column to use
#'
#' @return a validator (validate package)
#' @export
#'
#' @examples
td_create_validator <- function(
        specs,
        use_raw_values = FALSE,
        mapping_key = NULL,
        return_dataframe = FALSE
) {

    # Create rules for variables
    var_rule_df_list <- apply(
        specs$variables,
        # Apply by rows
        1,
        td_create_var_rules,
        # Additional argument for the function
        specs = specs,
        use_raw_values = use_raw_values,
        mapping_key = mapping_key
    )

    # Bind all variable rule data frames into a single data frame
    rule_df <- dplyr::bind_rows( var_rule_df_list )

    # Bind other rules
    if( 'rules' %in% names( specs ) ) {
        rule_df <- specs$rules %>%
            rename( name = rulename ) %>%
            dplyr::bind_rows( rule_df )
    }

    if( return_dataframe ) {
        return( rule_df )
    }

    # Create a validator from the rule data frame and return
    validator( .data = rule_df )
}


td_get_category_names <- function(
        varname_str,
        specs
) {

    categoryset_str <- specs$variables %>%
        filter( varname == varname_str ) %>%
        pull( categoryset )

    specs$categories %>%
        filter( categoryset == categoryset_str ) %>%
        pull( categoryname )
}


#' Get the mapping between category names and the values in raw data.
#'
#' @param varname_str The name of a variable
#' @param specs Stadas specs as a list of tibbles
#' @param mapping_key An optional mapping to access the correct
#'                    mapping column in the specs
#' @param inverse If FALSE (the default), variable names are used as
#'                the names of the named mapping list.
#'                IF TRUE, the raw data values are used as
#'                the names of the named mapping list
#'                (suitable for, f.ex., `dplyr::recode()`).
#'
#' @return A named list of the category names and the values in raw data
#' @export
#'
#' @examples
td_get_category_mapping <- function(
        varname_str,
        specs,
        mapping_key = NULL,
        inverse = FALSE
) {

     categoryset_str <- specs$variables %>%
         filter( varname == varname_str ) %>%
         pull( categoryset )

     categorysets <- specs$categories %>%
         filter( categoryset == categoryset_str )

     if( inverse ) {
         categorysets %>% pull(
             'categoryname',
             name = paste( c( 'mapping', mapping_key ), collapse = '_' )
         )
     } else {
         categorysets %>% pull(
             paste( c( 'mapping', mapping_key ), collapse = '_' ),
             name = categoryname
         )
     }
}


# Set the data types of variables
td_set_var_dtype <- function(
        df,
        varname,
        specs,
        keep_raw_values = FALSE,
        mapping_key = NULL
) {

    dtype <- tibble::deframe(
        specs$variables[c( 'varname', 'datatype' )]
    )[varname]

    if( dtype == 'integer' ) {
        df <- df %>%
            dplyr::mutate( !!varname := as.integer( .data[[varname]] ) )

    } else if( dtype == 'decimal' ) {
        df <- df %>%
            dplyr::mutate( !!varname := as.numeric( .data[[varname]] ) )

    } else if( dtype == 'categorical' ) {
        if( !keep_raw_values ) {
            # Recode raw values into specified category names
            category_mapping <- td_get_category_mapping(
                varname,
                specs,
                mapping_key,
                # `recode()` requires the arguments in an inverse order
                inverse = TRUE
            )
            df <- df %>%
                dplyr::mutate( !!varname := dplyr::recode(
                    .data[[varname]],
                    !!!category_mapping
                ) )
        }
        category_names <- td_get_category_names( varname, specs )
        df <- df %>%
            dplyr::mutate( !! varname := factor(
                .data[[varname]],
                levels = category_names
            ) )
            # mutate( across(
            #     all_of( varname ),
            #     ~factor( .x, levels = category_names )
            # ) )

    } else {
        df <- df %>%
            dplyr::mutate(
                !! varname := as.character( .data[[varname]] )
            )
    }

    df
}


td_set_df_dtypes <- function(
        df,
        specs,
        keep_raw_values = FALSE,
        mapping_key = NULL
) {

    varnames <- pull( specs$variables, varname )

    # Use only names found in the data
    common_names <- varnames[varnames %in% names( df )]

    # TODO: can you use some sort of "apply" with a data frame
    # without dropping columns?
    for( name in common_names ) {
        df <- df %>%
            td_set_var_dtype(
                name,
                specs,
                keep_raw_values = keep_raw_values,
                mapping_key = mapping_key
            )
    }

    df
}


td_conform_df <- function(
        df,
        specs,
        ignore_missingvars = FALSE,
        raw_data_prefix = 'raw__',
        mapping_key = NULL,
        ignore_validation = FALSE,
        validation_summary = FALSE,
        fun_addmetadata = NULL,
        fun_arg = NULL
) {

    # Keep track of "raw" data
    df.raw <- df

    # Preprocess raw data, if specified
    if( 'preprocess' %in% names( specs ) ) {
        df.raw <- td_preprocess( df.raw, specs$preprocess, mapping_key )
    }

    # If given, paste mapping key into the mapping column name
    mapping_colname <- paste(
        c( 'mapping', mapping_key ),
        collapse = '_'
    )

    # Get the specified variable names
    specified_varnames <- specs$variables %>%
        filter( !is.na( varname ) ) %>%
        pull( varname )

    # Get the specified variables that exist in the data
    if( mapping_colname %in% colnames( specs$variables ) ) {
        # Create a names list of mapping labels, named with variable names
        mapping <- specs$variables %>%
            filter( !is.na( {{mapping_colname}} ) ) %>%
            pull( mapping_colname, name = varname )
        # Get the column labels that are common between
        # the specification and the data
        common_collabels <- mapping[mapping %in% colnames( df.raw )]
        # Get the specified variables with the common column labels
        df <- df.raw[common_collabels]
        # Set the names to the defined variable names
        df <- data.table::setnames(
            df,
            old = mapping,
            new = names( mapping )
        )
    } else {
        # Get the variable names that are common between
        # the specification and the data
        common_varnames <- specified_varnames[
            specified_varnames %in% colnames( df.raw )
        ]
        df <- df.raw[common_varnames]
    }

    if( !ignore_missingvars ) {
        # Add columns for variables not found in the data
        # and set all values to `NA`
        missing_varnames <- specified_varnames[
            !specified_varnames %in% colnames( df )
        ]
        df[missing_varnames] <- NA
    }

    # Apply metadata function, if given
    if( !is.null( fun_addmetadata ) ) {
        df <- fun_addmetadata( df, fun_arg )
    }

    # Ensure that all the variables have the right data type
    df <- td_set_df_dtypes( df, specs, mapping_key = mapping_key )

    # Validation
    checks <- validate::confront(
        df,
        x = td_create_validator( specs )
    )
    if( validation_summary ) {
        print( dplyr::arrange(
            validate::summary( checks )[1:7],
            name
        ) )
    }
    if( !ignore_validation ) {
        if( !all( checks, na.rm = TRUE ) ) {
            stop( 'Validation failed!\n' )
        }
    }

    if( !is.null( raw_data_prefix ) ) {
        # Add the given prefix to the names of the raw data col labels
        df.raw <- dplyr::rename_with(
            df.raw, ~paste0( raw_data_prefix, .x )
        )
        # Bind the raw data to the valid data
        df <- dplyr::bind_cols( df, df.raw )
    }

    df
}


td_edit_dflistitem <- function(
        nameindex,
        list,
        specs,
        keep_case = FALSE,
        ignore_missingvars = FALSE,
        raw_data_prefix = 'raw__',
        mapping_key = NULL,
        ignore_validation = FALSE,
        validation_summary = FALSE,
        fun_addmetadata = function( df, nameindex ) {
            df %>% mutate( META__dataset_name = nameindex )
        }
) {

    # Access an item in the list with the name
    list[[nameindex]] %>%

        td_conform_df(
            specs = specs,
            ignore_missingvars = ignore_missingvars,
            raw_data_prefix = raw_data_prefix,
            mapping_key = mapping_key,
            ignore_validation = ignore_validation,
            validation_summary = validation_summary,
            fun_addmetadata = fun_addmetadata,
            fun_arg = nameindex
        )
}


td_conform_dflist_to_specs <- function(
        df_list,
        specs,
        keep_case = FALSE,
        ignore_missingvars = FALSE,
        raw_data_prefix = 'raw__',
        mapping_key = NULL,
        ignore_validation = FALSE,
        validation_summary = FALSE,
        fun_addmetadata = function( df, nameindex ) {
            df %>% mutate( META__dataset_name = nameindex )
        }
) {

    # Adapted from an SO answers by
    # Gavin Simpson, https://stackoverflow.com/a/11115275/7002525
    # and Brian Diggs, https://stackoverflow.com/a/18520422/7002525

    # Get the names of the tibble list
    list_names <- names( df_list )

    lapply(

        # Use `setNames()` to apply a function for each name of the list
        setNames( list_names, list_names ),

        # The function to apply
        td_edit_dflistitem,

        # Pass parameters to the function
        list = df_list,
        specs = specs,
        keep_case = keep_case,
        ignore_missingvars = ignore_missingvars,
        raw_data_prefix = raw_data_prefix,
        mapping_key = mapping_key,
        ignore_validation = ignore_validation,
        validation_summary = validation_summary,
        fun_addmetadata = fun_addmetadata
    )
}

