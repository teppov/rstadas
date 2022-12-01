test_df <- data.frame(
    AGEP_A = as.character( c( 21, 33, 97, 45, 99 ) ),
    HEIGHTTC_A = as.character( c( 64, 65, 97, 70, 98 ) ),
    WEIGHTLBTC_A = as.character( c( 140, 999, 151, 163, 149 ) )
)

operations <- data.frame(

    'newvarname' = c(
        'prep_age',

        'prep_height',
        'prep_height',
        'prep_height',

        'prep_weight',
        'prep_weight',
        'prep_weight'
    ),

    'operation' = c(

        'replace( AGEP_A, c( 97, 98, 99 ), NA )',

        'replace( HEIGHTTC_A, c( 96, 97, 98, 99 ), NA )',
        'as.numeric( prep_height )',
        'prep_height * 0.0254',

        'replace( WEIGHTLBTC_A, c( 996, 997, 998, 999 ), NA )',
        'as.numeric( prep_weight )',
        'prep_weight * 0.4535924'
    )
)

test_df.processed <- test_df %>%
    td_preprocess( operations )

test_that(
    'Preprocessing variables created', {
        expect_named(
            test_df.processed,
            c(
                'AGEP_A', 'HEIGHTTC_A', 'WEIGHTLBTC_A',
                'prep_age', 'prep_height', 'prep_weight'
            )
        )
    }
)

