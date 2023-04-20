

td_parse_all_numeric <- function( df, specs, na_colname = NULL ) {

    na_values <- td_get_all_na_vals( specs, na_colname )

    for( name in td_get_num_varnames( specs ) ) {

        datatype <- td_get_datatype( name, specs )

        if( datatype == 'discrete' ) {
            df <- df %>%
                mutate(
                    !!name := parse_integer(
                        .data[[name]],
                        na = na_values[[name]],
                    )
                )

        } else if( datatype == 'continuous' ) {
            df <- df %>%
                mutate(
                    !!name := parse_number(
                        .data[[name]],
                        na = na_values[[name]],
                    )
                )

        } else {
            stop( paste0(
                'The datatype "', datatype, '" of the variable "',
                name, '" is not numeric ("discrete" or "continuous")!'
            ) )
        }
    }

    df
}


td_parse_categorical <- function(
        x,
        mapping,
        na = c( '' ),
        ordered = FALSE
) {
    x %>%
        parse_factor(
            levels = names( mapping ),
            na = na
        ) %>%
        recode_factor(
            !!!mapping,
            .default = NA_character_,
            .ordered = ordered
        )
}


td_mutate_categorical <- function(
        df,
        varname,
        mapping,
        na = c( '' ),
        ordered = FALSE
) {
    df %>%
        mutate(
            !!varname := td_parse_categorical(
                .data[[varname]],
                mapping = mapping,
                na = na,
                ordered = ordered
            )
        )
}


td_parse_all_categorical <- function(
        df,
        specs,
        mapping_key = NULL
) {

    cat_specs <- td_get_cat_specs(
        specs,
        mapping_colname = td_get_mapping_colname( 'value', mapping_key )
    )
    na_values <- td_get_all_na_vals(
        specs,
        td_get_mapping_colname(
            prefix = 'na',
            mapping_key = mapping_key
        )
    )

    for ( name in td_get_cat_varnames( specs ) ) {

        # Skip variables not found in the raw data,
        # i.e. value is `NA` in the category definition
        if( !any( is.na( pull( cat_specs[[name]], 'value' ) ) ) ) {

            df <- df %>%

                td_mutate_categorical(
                    varname = name,
                    mapping = deframe(
                        cat_specs[[name]][c( 'value', 'categoryname' )]
                    ),
                    na = na_values[[name]],
                    # Set ordered = TRUE for ordinal variables
                    ordered = pull(
                        specs$variables[specs$variables$varname==name, ],
                        datatype
                    ) == 'ordinal'
                )
        }
    }

    df
}




