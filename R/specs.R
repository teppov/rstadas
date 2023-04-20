
SETUP_TABLETYPES <- c( 'variable', 'preprocess', 'rule' )

VAR_DATATYPES <- c(
    'discrete', 'continuous', 'nominal', 'ordinal', 'text'
)

SETUP_VALIDATOR <- validate::validator(
    tabletype_valid = tabletype %in% tabletypes,
    tablename_valid = tablename %in% tablenames
)

# PREP_VALIDATOR <- validate::validator(
#     mappingkey_valid = is.character( mappingkey ),
#     newvarname_notna = !is.na( newvarname ),
#     operation_notna = !is.na( operation )
# )

VAR_VALIDATOR <- validate::validator(
    varname_notna = !is.na( varname ),
    varname_unique = all_unique( varname ),
    datatype_valid = datatype %in% datatypes,
    unique_valid = unique %in% c( NA, 'unique' ),
    nona_valid = nona %in% c( NA, 'nona' ),
    categorytable_valid = categorytable %in% tablenames,
    navaluetable_valid = navaluetable %in% tablenames
)

CAT_VALIDATOR <- validate::validator(
    categoryset_notna = !is.na( categoryset ),
    categoryname_notna = !is.na( categoryname ),
    colortable_valid = colortable %in% tablenames
)

COL_VALIDATOR <- validate::validator(
    colorname_notna = !is.na( colorname ),
    colorname_unique = all_unique( colorname ),
    colorhex_valid = grepl(
        # Regex for colour hex code
        '^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$',
        colorhex
    )
)

RULE_VALIDATOR <- validate::validator(
    rulename_notna = !is.na( rulename ),
    # TODO: how to check rules?
    # https://cran.r-project.org/web/packages/validate/vignettes/cookbook.html#74_Validation_rule_syntax
    rule_valid = !is.na( rule ),
    label_valid = is.character( label ),
    description_valid = is.character( description )
)

NA_VALIDATOR <- validate::validator(
    navalueset_notna = !is.na( navalueset )
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
td_create_specs <- function(
        table_list,
        validation_summary = FALSE
) {

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
        ),
        print_summary = validation_summary
    )


    # # Preprocess
    # #############################################
    #
    # # Get the preprocess table names from the setup table
    # prepro_tablenames <- specs$setup %>%
    #     filter( tabletype == 'preprocess' ) %>%
    #     pull( tablename )
    #
    # if( length( prepro_tablenames ) > 0 ) {
    #
    #     # Bind preprocess tables
    #     specs$preprocess <- dplyr::bind_rows(
    #         table_list[prepro_tablenames]
    #     )
    # }


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
        ),
        print_summary = validation_summary
    )


    # Categories
    #############################################

    if( 'categorytable' %in% names( specs$variables ) ) {

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
            ref = list( tablenames = names( table_list ) ),
            print_summary = validation_summary
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
                'Color table not valid.',
                print_summary = validation_summary
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


    # NA
    #############################################

    if( 'navaluetable' %in% names( specs$variables ) ) {

        # Get the na value table names from the variable table
        navalue_tablenames <- specs$variables %>%
            filter( !is.na( navaluetable ) ) %>%
            pull( navaluetable ) %>%
            unique()

        # The na value table names must be found in the given table list
        if( !all( navalue_tablenames %in% names( table_list ) ) ) {
            stop( paste0(
                'All named na value tables not found
                in the given table list!\n',
                'The names in `navaluetable` column:\n',
                paste0( navalue_tablenames, collapse = ', ' ), '\n',
                'The names in table list:\n',
                paste0( names( table_list ), collapse = ', ' ), '\n'
            ) )
        }

        # Bind na value tables
        specs$na <- dplyr::bind_rows(
            table_list[navalue_tablenames]
        ) %>%
            # Keep only rows where navalueset is not NA
            dplyr::filter( !is.na( navalueset ) )

        # Validation
        td_validate_df(
            specs$na,
            NA_VALIDATOR,
            'NA table not valid.',
            ref = list( tablenames = names( table_list ) ),
            print_summary = validation_summary
        )

        # Get the category set names
        var_navalue_setnames <- specs$variables %>%
            filter( !is.na( navalueset ) ) %>%
            pull( navalueset ) %>%
            unique()
        na_navalue_setnames <- specs$na %>%
            filter( !is.na( navalueset ) ) %>%
            pull( navalueset ) %>%
            unique()

        # The category set names in the variable tables
        # must be found in the na tables
        if( !all( var_navalue_setnames %in% na_navalue_setnames ) ) {
            stop( paste0(
                'All na value sets named in variable tables not found
                in the na tables!\n',
                'The na value set names in variable tables:\n',
                paste0( var_navalue_setnames, collapse = ', ' ), '\n',
                'The names in na tables:\n',
                paste0( na_navalue_setnames, collapse = ', ' ), '\n'
            ) )
        }
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

    if( datatype == 'discrete' ) {
        rules <- rules %>%
            add_row(
                name = paste0( varname, '_disc' ),
                rule = paste0( 'is.discrete( ', varname, ' )' ),
                label = paste0( 'Discrete variable: ', varname ),
                description = paste0(
                    'A rule for testing if the values of ',
                    'the variable "', varname, '" are discrete numbers.'
                )
            )

    } else if( datatype == 'continuous' ) {
        rules <- rules %>%
            add_row(
                name = paste0( varname, '_cont' ),
                rule = paste0( 'is.continuous( ', varname, ' )' ),
                label = paste0( 'Continuous variable: ', varname ),
                description = paste0(
                    'A rule for testing if the values of ',
                    'the variable "', varname, '" are continuous numbers.'
                )
            )

    } else if( datatype %in% c( 'nominal', 'ordinal' ) ) {

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


td_get_category_labels <- function(
        varname_str,
        specs,
        lang = 'en'
) {

    label_str <- paste( c( 'label', lang ), collapse = '_' )

    categoryset_str <- specs$variables %>%
        filter( varname == varname_str ) %>%
        pull( categoryset )

    specs$categories %>%
        filter( categoryset == categoryset_str ) %>%
        select( categoryname, label_str ) %>%
        deframe()
}


td_get_category_colors <- function(
        varname,
        specs,
        mapping_colname = NULL,
        names_as_key = TRUE,
        lang = 'en'
) {

    if( names_as_key ) {
        key = 'varname'
    } else {
        key = paste( c( 'label', lang ), collapse = '_' )
    }

    td_get_cat_spec( varname, specs, mapping_colname ) %>%
        select( all_of( c( key, 'colorhex' ) ) ) %>%
        deframe()
}


td_get_spec_vartab_value <- function( varname, specs, speccol ) {
    if( ! varname %in% specs$variables$varname ) {
        stop( paste0(
            'The variable name "', varname, '" is not defined!'
        ) )
    }
    if( ! speccol %in% names( specs$variables ) ) {
        stop( paste0(
            'The name "', speccol,
            '" is not valid specification column name!'
        ) )
    }
    pull(
        specs$variables[specs$variables[['varname']]==varname, ],
        speccol
    )
}
td_get_datatype <- function( varname, specs ) {
    td_get_spec_vartab_value( varname, specs, 'datatype' )
}
td_get_categorytable <- function( varname, specs ) {
    td_get_spec_vartab_value( varname, specs, 'categorytable' )
}
td_get_categoryset <- function( varname, specs ) {
    td_get_spec_vartab_value( varname, specs, 'categoryset' )
}
td_get_navalueset <- function( varname, specs ) {
    td_get_spec_vartab_value( varname, specs, 'navalueset' )
}


td_get_num_varnames <- function( specs ) {
    specs$variables %>%
        filter( datatype %in% c( 'discrete', 'continuous' ) ) %>%
        pull( varname )
}


td_get_na_vals <- function( varname, specs, na_colname ) {

    na_vals <- specs$na %>%
        filter(
            navalueset == td_get_navalueset( varname, specs )
        ) %>%
        pull( .data[[na_colname]] )

    if( length( na_vals ) > 0 ){
        if( !any( is.na( na_vals ) ) ) {
            return( na_vals )
        }
    }

    c( '' )
}


td_get_all_na_vals <- function( specs, na_colname ) {

    if(
        'na' %in% names( specs ) &
        'navalueset' %in% names( specs$variables) &
        na_colname %in% names( specs$na )
    ) {
        func <- td_get_na_vals

    } else {
        # Always return c( '' )
        func <- function( varname, specs, na_colname ) { c( '' ) }
    }

    na_vals <- lapply(
        # Apply td_get_cat_spec to all categoric variable names
        specs$variables$varname,
        func,
        # Arguments to td_get_na_vals
        specs = specs,
        na_colname = na_colname
    )

    names( na_vals ) <- specs$variables$varname

    na_vals

}


td_get_cat_varnames <- function( specs ) {
    specs$variables %>%
        filter( datatype %in% c( 'nominal', 'ordinal' ) ) %>%
        pull( varname )
}


td_get_cat_spec <- function( varname, specs, mapping_colname = NULL ) {

    cat_spec <- specs$categories %>%
        filter(
            categoryset == td_get_categoryset( varname, specs )
        )

    if( 'colors' %in% names( specs ) ) {
        cat_spec <- cat_spec %>%
            left_join(
                specs$colors,
                by = c( 'colorname' )
            )
    }

    # Get value mapping (use category names if no mapping defined)
    if( is.null( mapping_colname ) ) {
        cat_spec <- mutate( cat_spec, value = categoryname )
    } else {
        cat_spec <- mutate( cat_spec, value = .data[[mapping_colname]] )
    }

    cat_spec
}


td_get_cat_specs <- function( specs, mapping_colname = NULL ) {

    cat_varnames <- td_get_cat_varnames( specs )

    cat_specs <- lapply(
        # Apply td_get_cat_spec to all categoric variable names
        cat_varnames,
        td_get_cat_spec,
        # Arguments to td_get_cat_spec
        specs = specs,
        mapping_colname = mapping_colname
    )

    names( cat_specs ) <- cat_varnames

    cat_specs
}




