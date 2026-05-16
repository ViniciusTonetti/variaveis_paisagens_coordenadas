# Code used to calculate the age of forest fragments (time elapsed since the last deforestation) 
# for the analysis used in Ballarin et al - Network-based assessment of habitat source–sink dynamics.
# 
# Written by Vinicius Tonetti* and reviewed by the other authors
# Contact email: vrtonetti@gmail.com


# Loading packages and cleaning environment ------------------------------------

library(terra)
rm(list = ls())


# Loading annual land cover mappings for Brazil and cropping data --------------
# for the São Paulo and Paraná states, where field samplings were conducted ----

# Annual land-cover maps for Brazil were downloaded for the years 1985–2015.
# The data were obtained from MapBiomas Collection 10 on 28/04/2026
# through the direct download link: https://brasil.mapbiomas.org/en/colecoes-mapbiomas/

raster_anual <- list.files("data/mapbiomas/anual",
                           pattern = "brazil_coverage_", full.names = T)

stack_br <- terra::rast(raster_anual)


# Load a vector polygon covering the states of São Paulo and Paraná ------------
# 

sp_pr <- terra::vect("data/vector/SP_PR/sp_pr_merge.shp")
sp_pr <- terra::project(sp_pr, "EPSG:4326") # Ensure the polygon has the same Coordinate Reference System (CRS)
                                            # as the MapBiomas data (EPSG:4326)


# Cropping and binarizing rasters ----------------------------------------------
# forest pixels = 1; non-forest pixels = 0

output <- "data/mapbiomas/anual/"

year <- 1985:2015

for (i in 1:length(raster_anual)) {
rast <- terra::mask(crop(stack_br[[i]], sp_pr), sp_pr)
rast_bin <- ifel(rast == 3, 1, 0)
writeRaster(rast_bin, paste0(output, "sp_pr_forest_", year[i], ".tif"), 
            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
}


# Calculating forest age -------------------------------------------------------

## Stack of cropped rasters

raster_anual_sp_pr <- list.files("data/mapbiomas/anual",
                                 pattern = "^sp_pr_forest.*\\.tif$",
                                 full.names = TRUE)

stack_sp_pr <- terra::rast(raster_anual_sp_pr)



forest_age_2015 <- app(stack_sp_pr, fun = function(x) {
  
  # Keep NA if the pixel is NA throughout the entire time series
  if (all(is.na(x))) return(NA)
  
  # If the pixel is not forest in 2015, age = 0
  if (is.na(x[length(x)]) || x[length(x)] == 0) return(0)
  
  # Count consecutive years of forest cover backwards from 2015
  reversed_series <- rev(x)
  first_zero <- which(reversed_series == 0)[1]
  
  if (is.na(primeiro_zero)) {
    return(length(x))
  } else {
    return(primeiro_zero - 1)
  }
})


plot(forest_age_2015) # Visualizing raster


# Saving raster to the output folder

output <- "data/mapbiomas/anual/"

writeRaster(forest_age_2015,
            paste0(output, "idade_floresta_2015.tif"), 
            gdal=c("COMPRESS=DEFLATE", "TFW=YES"),
            overwrite = T)

