########################################################################
## Script name: Distance_Calculation
## Purpose of script: This script convert shapefiles to raster format within
## the spatial extent of a template raster, and compute the distance from each cell 
## in the raster to the nearest hydrological unit
## Author: Chenyu Shen
## Date Created: 2024-03-20
## Notes:
########################################################################

### =========================================================================
### A - Preparation
### =========================================================================

## Install and load packages

#vector other required packages
packs<-c("reticulate","terra","sf")

#install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))


# Load the Python script for distance calculation
source_python("./src/scripts/preparation/Python/Distance_Calculation.py")

# # Set working directory
# setwd(Data_dir)


# Define paths
template_raster_path <- "ref_grid.tif"
peru_boundary_shapefile_path <- "./Preds/Raw/Utils/Peru_admin_boud/per_admbnda_adm0_ign_20200714.shp"
df_path <- "./Preds/Raw/Data_gathering.csv"


# Load the data gathering table
df <- read.csv(df_path, encoding = 'latin1')

### =========================================================================
### B - Calculate the distance 
### =========================================================================

# Load the Peru boundary 
peru_boundary <- st_read(peru_boundary_shapefile_path) 

# Reproject the Peru boundary shapefile (projected coordinate system suitable for distance calculation) and save it as a temporary file
peru_boundary_reprojected <- st_transform(peru_boundary, crs = "EPSG:24892")
repo_peru_boundary_path <- "./Preds/Raw/repo_peru_boundary.shp"
st_write(peru_boundary_reprojected, repo_peru_boundary_path)

# Open the template raster to use its properties
template_raster <- rast(template_raster_path)

#Reproject the template raster and save it as a temporary file
reprojected_raster <- terra::project(template_raster, "EPSG:24892")
reprojected_raster_path <- "./Preds/Raw/reprojected_raster.tif"
writeRaster(reprojected_raster, reprojected_raster_path, overwrite = TRUE)

# Loop through the dataframe for processing
for(i in 1:nrow(df)) {
  if(df$Processing_type[i] == 'Cal_Dist') {
    # Define paths for the current iteration
    shapefile_path <- df$Raw_data_path[i]
    new_path <- gsub("Raw", "Prepared", dirname(shapefile_path))
    output_path <- sub("\\.shp$", "_distance.tif", sub("Raw", "Prepared", shapefile_path))
    
    # Check if the new path already exist
    if (!dir.exists(new_path)) {
      # if not, create the path
      dir.create(new_path, recursive = TRUE)
    }

    # Call the Python function to calculate distances
    calculate_distances(shapefile_path, reprojected_raster_path, repo_peru_boundary_path, output_path)

    # Reproject the distances raster back to WGS 84
    distances_raster <- rast(output_path)
    distances_raster_wgs84 <- project(distances_raster, "EPSG:4326")

    # # Clip the distances raster with the Peru boundary
    # peru_boundary_vect <- vect(peru_boundary)
    # clipped_distances <- mask(distances_raster_wgs84, peru_boundary_vect)

    # Save the clipped distances raster
    writeRaster(distances_raster_wgs84, output_path, overwrite=TRUE)
    
    # Update the DataFrame with the new path
    df$Prepared_layer_path[i] <- output_path
    df$Prepared[i] <- "Y"
  }
}

# Save the updated DataFrame
write.csv(df, "./Preds/Raw/updated_data_paths.csv", row.names = FALSE)
