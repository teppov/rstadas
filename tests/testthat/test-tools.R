
test_that(
    'Read an Excel file', {
        expect_named(
            td_read_excel(
                system.file(
                    'SPECS_FWLB.xlsx',
                    package = 'rstadas',
                    mustWork = TRUE
                )
            ),
            c(
                'setup',
                'var_general', 'var_fwlb',
                'cat_general', 'cat_fwlb',
                'col_qua15', 'col_seq11', 'col_div11',
                'rules_general', 'rules_fwlb'
            )
        )
    }
)

