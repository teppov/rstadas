# rstadas

Stadas (Schema for Tabular Dataset Specification) is a schema for specifying a tabular dataset in a format that is easy for humans to read and write. The schema can be found in [stadas.org](https://stadas.org). rstadas is an R package for reading stadas specifications and conforming data to the specification.


## Installation

The package is not yet in CRAN. Instead, you can clone the RStudio project from the [GitHub repository](https://github.com/teppov/rstadas) and install the package with RStudio.


## Example

The package contains an example specification for [National Health Interview Survey](https://www.cdc.gov/nchs/nhis/index.htm) (NHIS) data. You can access the specification with `system.file()`:

```
excel_table_list <- td_read_excel_to_list(
    system.file(
        'stadas_v-0-1_nhis.xlsx',
        package = 'rstadas',
        mustWork = TRUE
    )
)

names( excel_table_list )
# [1] "setup"         "preprocess"    "var_general"   "cat_general"   "var_wellbeing" "cat_wellbeing" "rules_general"
```

Then you can turn the list of tables into a standardized specification:

```
specs <- td_create_specs( excel_table_list )

names( specs )
# [1] "setup"      "preprocess" "variables"  "categories" "rules"
```

In addition to the example specs, the package contains a subset of the NHIS 2021 data:

```
df.raw <- read.csv(
    file = system.file(
        'NHIS_2021/adult21_1000.csv',
        package = 'rstadas',
        mustWork = TRUE
    ),
    colClasses = 'character'
)
# 1000 obs. of 622 variables
```

Finally, you can conform the data to the specification:

```
df <- td_conform_df(
    df.raw,
    specs,
    raw_data_prefix = NULL
)
# 1000 obs. of 25 variables
```
