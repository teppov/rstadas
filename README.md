# rstadas

Stadas (Schema for Tabular Dataset Specification) is a schema for specifying a tabular dataset in a format that is easy for humans to read and write. The schema can be found in [stadas.org](https://stadas.org). **rstadas** is an R package for reading stadas specifications and conforming data to the specification.


## Installation

**rstadas** uses Tidyverse extensively:

```

install.packages( "tidyverse" )
library( tidyverse )

```

In addition, if you plan to read Excel files, **readxl** is recommended (it's part of Tidyverse but not in the core, so you need to load the package separately):

```

library( readxl )

```

The **rstadas** package is not yet in CRAN. Instead, you can clone the RStudio project from the [GitHub repository](https://github.com/teppov/rstadas) and install the package with RStudio (Build -> Install Package).


## Example

The package contains an example specification for [National Health Interview Survey](https://www.cdc.gov/nchs/nhis/index.htm) (NHIS) data. The specification is not made by NCHS, which is responsible only for the initial data.

You can access the specification with `system.file()`, and read the Excel sheets into a named list of data frames:

```
excel_spec_table_list <- td_read_excel_to_list(
    system.file(
        'nhis_stadas-v-0-3.xlsx',
        package = 'rstadas',
        mustWork = TRUE
    )
)

names( excel_spec_table_list )

 [1] "setup"         "var_general"   "na_general"    "cat_general"   "var_wellbeing" "cat_wellbeing" "rules_general" "col_qua15"     "col_seq11"     "col_div11"
```

Then you can turn the list of tables into a standardized specification:

```
specs <- td_create_specs( excel_spec_table_list )

names( specs )

# [1] "setup"      "variables"  "categories" "colors"    "rules"      "na" 
```

You can download NHIS datasets as zipped CSV files from the NHIS website:

```
# 2019
download.file(
    url = 'https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2019/adult19csv.zip',
    destfile = file.path( '.', 'nhis', 'nhis_2019.zip' )
)

# 2020
download.file(
    url = 'https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2020/adult20csv.zip',
    destfile = file.path( '.', 'nhis', 'nhis_2020.zip' )
)

# 2021
download.file(
    url = 'https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2021/adult21csv.zip',
    destfile = file.path( '.', 'nhis', 'nhis_2021.zip' )
)
```

The raw datasets have NHIS defined column names and values coded with (mostly) integers:

```
read_csv( file.path( '.', 'nhis', 'nhis_2021.zip' ) ) %>%
    head( c( 6, 6 ) )

# Multiple files in zip: reading 'adult21.csv'
# Rows: 29482 Columns: 622                                   
# ── Column specification ─────────────────────────────────────
# Delimiter: ","
# chr   (1): HHX
# dbl (599): URBRRL, RATCAT_A, IMPINCFLG_A, CVDVAC2YR_A, CV...
# lgl  (22): OGFLG_A, OPFLG_A, CHFLG_A, MAFLG_A, PRPLCOV1_C...
# 
# ℹ Use `spec()` to retrieve the full column specification for this data.
# ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
# # A tibble: 6 × 6
# URBRRL RATCAT_A IMPINCFLG_A CVDVAC2YR_A CVDVAC2MR_A
# <dbl>    <dbl>       <dbl>       <dbl>       <dbl>
#     1      4        7           0          NA          NA
# 2      4       12           0          NA          NA
# 3      4       14           0          NA          NA
# 4      3       11           0          NA          NA
# 5      1        6           1          NA          NA
# 6      1        6           1          NA          NA
# # … with 1 more variable: CVDVAC1YR_A <dbl>
# Warning message:
#     One or more parsing issues, see `problems()` for details
```

Using the specs, you can read a data file into a data frame that conforms to your specs:

```
df.2021 <- td_read_data_file(
    file_path = file.path( '.', 'nhis', 'nhis_2021.zip' ),
    read_func = read_csv,
    specs = specs,

    drop_na_colnames = NULL,  # default

    varmap_func = td_varmap,  # default

    mapping_key = NULL,  # default
    # Pick a 4-digit key from the file name for mapping names
    mapping_key_pattern = '[0-9]{4}',

    # Use rstadas function to add the survey year as metadata
    metadata_func = td_metadata_year,
    metadata_df = NULL,  # default

    # Drop original (raw) data columns
    keep_originals = FALSE,

    # Additional arguments to the read_func (read_csv())
    ## Read all as text
    col_types = cols( .default = col_character() )
)

# Multiple files in zip: reading 'adult21.csv'
```

Now, it's easier to understand what is in the data, and the data types of the variables are correct:

```
df.2021 %>%
    head( c( 6, 6 ) )

# # A tibble: 6 × 6
#    year   age  sex    height_in weight_lb edulevel 
#    <int> <int> <fct>      <dbl>     <dbl> <ord>    
# 1  2021     50 male          69       199 g11      
# 2  2021     53 male          75       205 academic 
# 3  2021     56 male          67       160 bachelors
# 4  2021     57 female        63       190 college  
# 5  2021     25 male          72       250 hschool  
# 6  2021     55 male          69       200 college  

```

If you have multiple files in the same directory (say, NHIS datasets from the years 2019, 2020 and 2021), you can read them all at once into the same data frame:

```
df <- td_read_data_dir(
    dir_path = file.path( '.', 'nhis' ),
    specs = specs,
    read_func = read_csv,

    drop_na_colnames = NULL,  # default

    varmap_func = td_varmap,  # default

    mapping_key = NULL,  # default
    mapping_key_pattern ='[0-9]{4}',

    metadata_func = td_metadata_year,
    metadata_df = NULL,  # default

    keep_originals = FALSE,

    # Read all as text
    col_types = cols( .default = col_character() )
)

# Multiple files in zip: reading 'adult21.csv'
```
