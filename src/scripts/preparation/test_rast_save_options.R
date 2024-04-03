install.packages("terra")
library(terra)

#load test rast
test_rast <- terra::rast("X:/NASCENT-Peru/03_workspaces/02_modelling/Data_collab/Preds/Prepared/Suitability/Biophysical/Clay_content/clay_0-5cm_mean.tif")

#check min/max values
minmax(test_rast)

#test if rats is integer
is.int(test_rast)

### =========================================================================
### Convert to integer
### =========================================================================

#multiple by 1000 to convert to integer
test_rast_int <- test_rast * 10000

#check min/max values
minmax(test_rast_int)

#test if rats is integer
is.int(test_rast_int)

### =========================================================================
### Check size using Geotiff
### =========================================================================

#save using INT2U datatype
writeRaster(test_rast,
            datatype= "INT2U",
            filename = "X:/NASCENT-Peru/03_workspaces/02_modelling/Data_collab/Preds/Prepared/Suitability/Biophysical/Clay_content/test_int_rast.tif")


#re-read
test_int2U <- rast("X:/NASCENT-Peru/03_workspaces/02_modelling/Data_collab/Preds/Prepared/Suitability/Biophysical/Clay_content/test_int_rast.tif") 
is.int(test_int2U)

#attempt to force rast values into memory
test_rds <- test_int2U *1

### =========================================================================
### Check size using RDS
### =========================================================================

# Convert back to raster as terra::saveRDS only saves reference
test_rds <- raster::raster(test_int2U)

# Use raster::readall to ensure values are in memory
# THIS FAILS BECAUSE OF RAM LIMITS
test_rds <- raster::readAll(test_rds)

#save as RDS instead
terra::saveRDS(test_rds, "X:/NASCENT-Peru/03_workspaces/02_modelling/Data_collab/Preds/Prepared/Suitability/Biophysical/Bulk_density/test_rast.rds")

