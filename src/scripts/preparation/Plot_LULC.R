########################################################################
## Script name: Plot_LULC
## Purpose of script:Plot LULC data
## Author: Chenyu Shen
## Date Created: 2023-10-23
## Notes:
########################################################################

### =========================================================================
### A - Preparation
### =========================================================================

## Install and load packages

#vector other required packages
packs<-c("terra","raster")

#install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# # Set working directory
# setwd(Data_dir)

# Load all the LULC files
years <- 1985:1990
LULC_list <- list()

for(year in years){
  path <- paste0("LULC/peru_coverage_",year,".tif")
  LULC_list[[as.character(year)]] <- rast(path)
}



my_colors <- c(
  "3" = "#006400",
  "4" = "#32CD32",
  "5" = "#687537",
  "6" = "#76A5AF",
  "9" = "#935132",
  "11" = "#45C2A5",
  "12" = "#B8AF4F",
  "13" = "#F1C232",
  "15" = "#FFD966",
  "18" = "#E974ED",
  "21" = "#FFEFC3",
  "24" = "#AA0000",
  "30" = "#FF0000",
  "25" = "#FF8585",
  "33" = "#0000FF",
  "34" = "#4FD3FF",
  "27" = "#D5D5E5"
)

labels <- c(
  "3" = "Forest",
  "4" = "Dry forest",
  "5" = "Mangrove",
  "6" = "Flooded forest",
  "9" = "Forest plantation",
  "11" = "Wetland",
  "12" = "Grasslands/herbaceous",
  "13" = "Scrubland and other non forest formations",
  "15" = "Pasture",
  "18" = "Agriculture",
  "21" = "Agricultural mosaic",
  "24" = "Infrastructure",
  "30" = "Mining",
  "25" = "Other non vegetated area",
  "33" = "River,lake and ocean",
  "34" = "Glacier",
  "27" = "Not observed"
)

land_cover_map <- LULC_list[["1985"]]

# Get unique values using freq
unique_vals <- freq(land_cover_map)[, 2]

# Match unique values with the predefined colors and labels
plot_colors <- my_colors[as.character(unique_vals)]
plot_labels <- labels[as.character(unique_vals)]




layout(matrix(1:2, nrow=1), widths=c(4, 1))


par(mar=c(5, 5, 4, 2)) 
plot(land_cover_map, col=plot_colors, main=paste("Land Cover Map", year), axes=TRUE, legend=FALSE)


par(mar=c(5, 2, 4, 2)) 
plot.new()
legend("center", legend=plot_labels, fill=plot_colors, title="Land Cover Types", cex=0.5, bty="n")
