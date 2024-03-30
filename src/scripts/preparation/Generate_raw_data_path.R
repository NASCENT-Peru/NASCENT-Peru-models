########################################################################
## Script name: Generate_raw_data_path
## Purpose of script: This script read the Excel file, then for wach row, 
## it concatenate the base directory with the values from the "Predictor_category", 
## "Detail_category", "Title", and "Var_name" columns to form the "Raw_data_path".
## Author: Chenyu Shen
## Date Created: 2024-03-08
## Notes:
########################################################################

### =========================================================================
### A - Preparation
### =========================================================================

## Install and load packages

#vector other required packages
packs<-c("readxl","dplyr","openxlsx")

#install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# # Set working directory
# setwd(Data_dir)

# Read the Excel file
data <- read_excel("./Preds/Raw/Data_gathering.xlsx")

### =========================================================================
### B - Raw data path generation
### =========================================================================

# Generate the Raw_data_path with conditions
data <- data %>%
  mutate(Raw_data_path = ifelse(Scale_resolution == "District" & 
                                  !is.na(Predictor_category) & 
                                  !is.na(Detail_category) & 
                                  !is.na(Var_name),
                                paste0("./Preds/Raw/", 
                                       Predictor_category, "/", 
                                       Detail_category, "/", 
                                       Var_name),
                                ifelse(!is.na(Predictor_category) & 
                                         !is.na(Detail_category) & 
                                         !is.na(Title) & 
                                         !is.na(Var_name),
                                       paste0("./Preds/Raw/", 
                                              Predictor_category, "/", 
                                              Detail_category, "/", 
                                              Title, "/", 
                                              Var_name),
                                       NA)))


# Save the modified data frame back to an Excel file
write.xlsx(data, "./Preds/Raw/Data_gathering_with_paths.xlsx")
