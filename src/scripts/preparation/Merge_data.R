########################################################################
## Script name: Merge_data
## Purpose of script: Creatw a name-matching table to merge the statistic 
## data downloaded from INEI website with district shapefile in Peru
## Author: Chenyu Shen
## Date Created: 2024-1-27
########################################################################

### =========================================================================
### A - Preparation
### =========================================================================
## Install and load packages
packs <- c("sf", "dplyr", "stringi")

# Install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# # Set working directory
# setwd(Data_dir)




### =========================================================================
### B - Find common names across the Excel data and the district shapefile
### =========================================================================

# Path to the CSV file that contains the largest number of rows 
csv_file_path <- "./Preds/Raw/INEI/District/Total_income.csv"

# Load the statistic data
statistic <- read.csv(csv_file_path)

# Normalize Spanish characters for matching
statistic_data <- statistic %>%
  mutate(
    DEPARTMENT = iconv(DEPARTMENT, "UTF-8", "ASCII//TRANSLIT"),
    PROVINCE = iconv(PROVINCE, "UTF-8", "ASCII//TRANSLIT"),
    DISTRICT = iconv(DISTRICT, "UTF-8", "ASCII//TRANSLIT")
  )


# Path to the district shapefile 
shapefile_path <- "./Preds/Raw/Peru_admin_boud/distritos/DISTRITOS.shp"


# Load and process the shapefile
district_shapefile <- st_read(shapefile_path) %>%
  mutate(
    DEPARTAMEN = iconv(DEPARTAMEN, "UTF-8", "ASCII//TRANSLIT"),
    PROVINCIA = iconv(PROVINCIA, "UTF-8", "ASCII//TRANSLIT"),
    DISTRITO = iconv(DISTRITO, "UTF-8", "ASCII//TRANSLIT")
  ) %>%
  rename(
    DEPARTMENT = DEPARTAMEN,
    PROVINCE = PROVINCIA,
    DISTRICT = DISTRITO
  )



# Merge the statistic data with the shapefile
# Ensure the column names used in 'by' argument are present and correctly named in both datasets
merged_data <- merge(district_shapefile, statistic_data, by = c("DEPARTMENT", "PROVINCE", "DISTRICT"))

# View the merged data
print(merged_data)


# Find unmatched rows in statistic data 
unmatched_excel <- anti_join(statistic_data, district_shapefile, 
                             by = c("DEPARTMENT", "PROVINCE", "DISTRICT"))

# Find unmatched rows in shapefile data
unmatched_shapefile <- anti_join(district_shapefile, statistic_data, 
                                 by = c("DEPARTMENT", "PROVINCE", "DISTRICT"))


# Delete the unusual rows
unmatched_excel <- subset(unmatched_excel, PROVINCE != "MAYNAS") 


# Print unmatched district names 
print(unmatched_excel)
print(unmatched_shapefile)


### =========================================================================
### C - Create name-matching table
### =========================================================================
# Extract the district names in excel data as DISTRICT_WRONG
unmatched_excel <- unmatched_excel %>%
  select(DEPARTMENT, PROVINCE, DISTRICT) %>%
  rename(DISTRICT_WRONG = DISTRICT)

# Extract the district names in shapefile data as DISTRICT_CORRECT
unmatched_shapefile <- unmatched_shapefile %>%
  select(DEPARTMENT, PROVINCE, DISTRICT) %>%
  rename(DISTRICT_CORRECT = DISTRICT)

# Create an empty dataframe
name_matching_table <- data.frame(DEPARTMENT = character(), PROVINCE = character(), DISTRICT_WRONG = character(), DISTRICT_CORRECT = character())

# For each unmatched rows in Excel data，find the corresponding rows in Shapefile data
for(i in 1:nrow(unmatched_excel)) {
  department <- unmatched_excel$DEPARTMENT[i]
  province <- unmatched_excel$PROVINCE[i]
  district_wrong <- unmatched_excel$DISTRICT_WRONG[i]
  
  # Find identical DEPARTMENT and PROVINCE in unmatched shapefile
  matching_row <- unmatched_shapefile %>%
    filter(DEPARTMENT == department & PROVINCE == province)
  
  if(nrow(matching_row) > 0) {
    # If a match is found, add it to the table
    for(j in 1:nrow(matching_row)) {
      district_correct <- matching_row$DISTRICT_CORRECT[j]
      name_matching_table <- rbind(name_matching_table, data.frame(DEPARTMENT = department, PROVINCE = province, DISTRICT_WRONG = district_wrong, DISTRICT_CORRECT = district_correct))
    }
  }
}

# print the name-matching table and save it
print(name_matching_table)
# save_path <- paste0(Data_dir, "/Preds/Raw/INEI/District/name_matching_table.csv")
# write.csv(name_matching_table, save_path, row.names = FALSE, quote = FALSE)





### =========================================================================
### D - Process all csv files and merge them with the shapefile
### =========================================================================

# Load the Data gathering table
data_gathering_table_path <- "./Preds/Raw/Data_gathering.csv"
data_gathering_table <- read.csv(data_gathering_table_path)

# Define the modeling periods
modeling_periods <- list(
  Period_2006_2010 = 2006:2010,
  Period_2010_2014 = 2010:2014,
  Period_2014_2018 = 2014:2018,
  Period_2018_2022 = 2018:2022
)


# Path to the district shapefile
shapefile_path <- "./Preds/Raw/Utils/Peru_admin_boud/distritos/DISTRITOS.shp"

# Read the shapefile and replace Spanish characters
district_shapefile <- st_read(shapefile_path) %>%
  mutate(
    DEPARTAMEN = iconv(DEPARTAMEN, "UTF-8", "ASCII//TRANSLIT"),
    PROVINCIA = iconv(PROVINCIA, "UTF-8", "ASCII//TRANSLIT"),
    DISTRITO = iconv(DISTRITO, "UTF-8", "ASCII//TRANSLIT")
  ) %>%
  rename(
    DEPARTMENT = DEPARTAMEN,
    PROVINCE = PROVINCIA,
    DISTRICT = DISTRITO
  )

# Load the name-matching table
name_matching_table_path <- "./Preds/Raw/INEI/name_matching_table.csv"
name_matching_table <- read.csv(name_matching_table_path)

# Read the shapefile (keeping the Spanish characters this time, as you intend to normalize them later)
district_shapefile <- st_read(shapefile_path) %>%
  mutate(
    DEPARTMENT = iconv(DEPARTAMEN, "UTF-8", "ASCII//TRANSLIT"),
    PROVINCE = iconv(PROVINCIA, "UTF-8", "ASCII//TRANSLIT"),
    DISTRICT = iconv(DISTRITO, "UTF-8", "ASCII//TRANSLIT")
  )

# Load the name-matching table
name_matching_table <- read.csv(name_matching_table_path)

# Process each specified CSV file
for (i in 1:nrow(data_gathering_table)) {
  if(df$Processing_type[i] == "Join_SHP") {
    # Construct the full path to the CSV file
    csv_file_path <- data_gathering_table$Raw_data_path[i]
    variable_calc <- data_gathering_table$Variable_calculation[i]
    
    # Create the save path by replacing "Raw" with "Prepared" in the old path
    save_path <- gsub("Raw", "Prepared", csv_file_path)
    
    # Read and process the CSV file
    statistic_data <- read.csv(csv_file_path) %>%
      mutate(
        DEPARTMENT = iconv(DEPARTMENT, "UTF-8", "ASCII//TRANSLIT"),
        PROVINCE = iconv(PROVINCE, "UTF-8", "ASCII//TRANSLIT"),
        DISTRICT = iconv(DISTRICT, "UTF-8", "ASCII//TRANSLIT")
      )
    
    # Replace district names using the name-matching table
    statistic_data <- merge(statistic_data, name_matching_table, by.x = c("DEPARTMENT", "PROVINCE", "DISTRICT"), by.y = c("DEPARTMENT", "PROVINCE", "DISTRICT_WRONG"), all.x = TRUE) %>%
      mutate(DISTRICT = ifelse(is.na(DISTRICT_CORRECT), DISTRICT, DISTRICT_CORRECT)) %>%
      select(-DISTRICT_CORRECT)
    
    # Ensure all relevant columns are numeric
    for (year in 2006:2022) {
      column_name <- paste0("X", year)
      if (column_name %in% names(statistic_data)) {
        statistic_data[[column_name]] <- as.numeric(as.character(statistic_data[[column_name]]))
      }
    }
    
    # Initialize a data frame for calculated results
    calc_results <- data.frame(DEPARTMENT = statistic_data$DEPARTMENT, PROVINCE = statistic_data$PROVINCE, DISTRICT = statistic_data$DISTRICT)
    
    # Perform calculation for each period
    for (period_name in names(modeling_periods)) {
      years <- modeling_periods[[period_name]]
      relevant_columns <- names(statistic_data)[names(statistic_data) %in% paste0("X", years)]
      
      # Calculate based on 'Variable_calculation'
      if (variable_calc == "average") {
        calc_results[[period_name]] <- rowMeans(statistic_data[relevant_columns], na.rm = TRUE)
      } else if (variable_calc == "median") {
        calc_results[[period_name]] <- apply(statistic_data[relevant_columns], 1, median, na.rm = TRUE)
      } else if (variable_calc == "difference") {
        calc_results[[period_name]] <- statistic_data[[tail(relevant_columns, n = 1)]] - statistic_data[[head(relevant_columns, n = 1)]]
      }
    }
    
    # Merge the calculated results with the shapefile data
    merged_data <- merge(district_shapefile, calc_results, by = c("DEPARTMENT", "PROVINCE", "DISTRICT"))
    
    # Save the merged data
    write.csv(merged_data, save_path, row.names = FALSE, quote = FALSE)
    
    # Update the DataFrame with the new path
    data_gathering_table$Prepared_layer_path[i] <- save_path
    data_gathering_table$Prepared[i] <- "Y"
  }
}
  




### =========================================================================
### E - Create a Comprehensive Name-Matching Table
### =========================================================================

## Load the data and normalize names for matching
statistic_data <- read.csv(csv_file_path) %>%
  mutate(
    DEPARTMENT_NORM = iconv(DEPARTMENT, "UTF-8", "ASCII//TRANSLIT"),
    PROVINCE_NORM = iconv(PROVINCE, "UTF-8", "ASCII//TRANSLIT"),
    DISTRICT_NORM = iconv(DISTRICT, "UTF-8", "ASCII//TRANSLIT")
  )

district_shapefile <- st_read(shapefile_path) %>%
  st_drop_geometry() %>% # Drop the geometry column
  mutate(
    DEPARTAMEN_NORM = iconv(DEPARTAMEN, "UTF-8", "ASCII//TRANSLIT"),
    PROVINCIA_NORM = iconv(PROVINCIA, "UTF-8", "ASCII//TRANSLIT"),
    DISTRITO_NORM = iconv(DISTRITO, "UTF-8", "ASCII//TRANSLIT")
  )


# Perform matching on normalized names
matched_data <- merge(
  x = district_shapefile, y = statistic_data,
  by.x = c("DEPARTAMEN_NORM", "PROVINCIA_NORM", "DISTRITO_NORM"),
  by.y = c("DEPARTMENT_NORM", "PROVINCE_NORM", "DISTRICT_NORM")
)


# Create the name-matching table using original names
name_matching_table_full <- matched_data %>%
  select(
    UBIGEO_DEPT = IDDPTO,
    SHP_DEPT = DEPARTAMEN,
    UBIGEO_PROV = IDPROV,
    SHP_PROV = PROVINCIA,
    UBIGEO_DIST = IDDIST,
    SHP_DIST = DISTRITO,
    EXL_DEPT = DEPARTMENT,
    EXL_PROV = PROVINCE,
    EXL_DIST = DISTRICT,
    DEPT_ASCII = DEPARTAMEN_NORM,
    PROV_ASCII = PROVINCIA_NORM,
    DIST_ASCII = DISTRITO_NORM
    
  )


# Identify unmatched rows from shapefile data
unmatched_shapefile_rows <- anti_join(district_shapefile, matched_data, by = c("DEPARTAMEN", "PROVINCIA", "DISTRITO"))

# Create a dataframe for unmatched rows with the required structure
unmatched_rows_df <- unmatched_shapefile_rows %>%
  select(
    UBIGEO_DEPT = IDDPTO,
    SHP_DEPT = DEPARTAMEN,
    UBIGEO_PROV = IDPROV,
    SHP_PROV = PROVINCIA,
    UBIGEO_DIST = IDDIST,
    SHP_DIST = DISTRITO,
    DEPT_ASCII = DEPARTAMEN_NORM,
    PROV_ASCII = PROVINCIA_NORM,
    DIST_ASCII = DISTRITO_NORM
  ) %>%
  mutate(
    EXL_DEPT = NA_character_,
    EXL_PROV = NA_character_,
    EXL_DIST = NA_character_
  )

# Append the unmatched rows dataframe to the existing name_matching_table
name_matching_table_full <- rbind(name_matching_table_full, unmatched_rows_df)

print(name_matching_table_full)


# save_path <-"./src/utls/Admin_unit_matching_table.csv"
# write.csv(name_matching_table_full, save_path, row.names = FALSE, quote = FALSE)


