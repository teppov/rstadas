library( validate )
# library( data.table )
library( tidyverse )
# library( readxl )
library( testthat )
# library( usethis )


# Read Excel specification tables
excel_spec_table_list <- td_read_excel_to_list(
    system.file(
        'nhis_stadas-v-0-3.xlsx',
        package = 'rstadas',
        mustWork = TRUE
    )
)

# Turn the list into a standardized spec
specs <- td_create_specs(
    excel_spec_table_list,
    validation_summary = TRUE
)


test_that(
    'Specs created from Excel tables', {
        expect_named(
            specs,
            c(
                'setup',
                'variables',
                'categories',
                'colors',
                'rules',
                'na'
            )
        )
    }
)

