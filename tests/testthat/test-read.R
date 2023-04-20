# library( validate )
# library( tidyverse )
# library( readxl )
# library( testthat )


# Read Excel specification tables
excel_spec_table_list <- td_read_excel_to_list(
    system.file(
        'stadas_v-0-1_nhis.xlsx',
        package = 'rstadas',
        mustWork = TRUE
    )
)

# Turn the list into a standardized spec
specs <- td_create_specs(
    excel_spec_table_list,
    validation_summary = TRUE
)
names( specs )


# Download NHIS datasets
# download.file(
#     url = 'https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2019/adult19csv.zip',
#     destfile = file.path( '.', 'nhis', 'nhis_2019.zip' )
# )
# download.file(
#     url = 'https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2020/adult20csv.zip',
#     destfile = file.path( '.', 'nhis', 'nhis_2020.zip' )
# )
# download.file(
#     url = 'https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2021/adult21csv.zip',
#     destfile = file.path( '.', 'nhis', 'nhis_2021.zip' )
# )


df.2021 <- td_read_file(
    file_path = file.path( '.', 'nhis', 'nhis_2021.zip' ),
    read_func = read_csv,
    specs = specs,

    # Pick a 4-digit key from the file name for mapping names
    mapping_key_pattern = '[0-9]{4}',

    # Use rstadas function to add the survey year as metadata
    metadata_func = td_metadata_year,

    # Drop original (raw) data columns
    keep_originals = FALSE,

    # Additional arguments to the read_func (read_csv())
    ## Read all as text
    col_types = cols( .default = col_character() )
)


df <- td_read_dir(
    dir_path = file.path( '.', 'nhis' ),
    specs = specs,
    read_func = read_csv,
    mapping_key_pattern ='[0-9]{4}',
    metadata_func = td_metadata_year,
    keep_originals = FALSE,

    # Read all as text
    col_types = cols( .default = col_character() )
)

