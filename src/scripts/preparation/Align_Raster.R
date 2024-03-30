########################################################################
## Script name: Align_raster
## Purpose of script:Align all downloaded raster date with the ref_gird
## (Matching their extent, crs and resolution)
## Author: Chenyu Shen
## Date Created: 2024-03-01
## Notes:
########################################################################

### =========================================================================
### A - Preparation
### =========================================================================

## Install and load packages

#vector other required packages
packs<-c("terra")

#install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))


# # Set working directory
# setwd(Data_dir)

# Path to the reference raster
reference_raster_path <- "ref_grid.tif"

# Read the reference raster
ref_raster <- rast(reference_raster_path)

# Load the data gathering table 
data <- read.csv("./Preds/Raw/Data_gathering.csv")

# # Filter rows where Processing_type is "Align_raster"
# align_raster_data <- subset(data, Processing_type == "Align_raster")


### =========================================================================
### B - Re-projection
### =========================================================================


# Loop over the filtered file paths
for(i in 1:nrow(data)) {
  if(data$Processing_type[i] == 'Align_raster') {
    # Original raster path
    old_path <- data$Raw_data_path[i]
    
    # Create the new path by replacing "Raw" with "Prepared" in the old path
    new_path <- gsub("Raw", "Prepared", old_path)
    
    # Load the raster to be aligned
    raster_to_align <- rast(old_path)

    # Align the raster to match the reference (CRS, extent, resolution)
    aligned_raster <- project(raster_to_align, ref_raster, method="bilinear")

    # Ensure the directory exists before saving
    if(!dir.exists(dirname(new_path))) {
      dir.create(dirname(new_path), recursive = TRUE)
    }

    # If the provider is SoilGrids, round the values
    if(data$Provider[i] == "SoilGrids") {
      aligned_raster <- round(aligned_raster)
      # When saving, specify datatype as "INT2U" to save as unsigned 2-byte integer
      writeRaster(aligned_raster, new_path, overwrite=TRUE, datatype="INT2U")
    } else {
      # Save the adjusted raster to the new path without changing the datatype
      writeRaster(aligned_raster, new_path, overwrite=TRUE)
    }

    # Update the DataFrame with the new path
    data$Prepared_layer_path[i] <- new_path
    data$Prepared[i] <- "Y"
  }
  
}

# Save the updated DataFrame
write.csv(data, "./Preds/Raw/updated_data_paths.csv", row.names = FALSE)

