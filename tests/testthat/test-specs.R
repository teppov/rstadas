# library( validate )
# library( data.table )
# library( tidyverse )
# library( readxl )
# library( testthat )
# library( usethis )

excel_table_list <- td_read_excel_to_list(
    system.file(
        'stadas_v-0-1_nhis.xlsx',
        package = 'rstadas',
        mustWork = TRUE
    )
)

specs <- td_create_specs( excel_table_list )

test_that(
    'Specs created from Excel tables', {
        expect_named(
            specs,
            c(
                'setup',
                'preprocess',
                'variables',
                'categories',
                'rules'
            )
        )
    }
)

df.raw <- read.csv(
    file = system.file(
        'NHIS_2021/adult21_1000.csv',
        package = 'rstadas',
        mustWork = TRUE
    ),
    colClasses = 'character'
)

df <- td_conform_df(
    df.raw,
    specs,
    raw_data_prefix = NULL
)

test_that(
    'Data conforms to the specs', {
        expect_named(
            df,
            c(
                'age', 'sex', 'height', 'weight', 'edulevel',
                'nof_adults', 'nof_children', 'maritalstatus',
                'health', 'lifesatisfaction',
                'diff_walk100yd', 'diff_walk033miles', 'diff_walksteps',
                'diff_communication', 'diff_remconc', 'diff_errands',
                'diff_socactivities',
                'mh_anxiety', 'mh_depression',
                'k6_sadness', 'k6_nervous', 'k6_restless',
                'k6_hopeless', 'k6_effort', 'k6_worthless'
            )
        )
    }
)
