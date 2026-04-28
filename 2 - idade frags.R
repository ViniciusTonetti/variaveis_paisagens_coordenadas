## Calculando idade frags paisagens

# pacote e limpando ambiente ---------------------------------------------------

rm(list = ls())

library(terra)


# Carregando rasters -----------------------------------------------------------


raster_anual <- list.files("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/anual",
                           pattern = "brazil_coverage_", full.names = T)

stack_br <- terra::rast(raster_anual)

# Carregando polígono ----------------------------------------------------------

sp_pr <- terra::vect("E:/_PESSOAL/ViniciusT/camadas Delano/br_uf/SP_PR/sp_pr_merge.shp")
sp_pr <- terra::project(sp_pr, "EPSG:4326")

# Cortando e binarizando rasters -----------------------------------------------

output <- "E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/anual/"

ano <- 1985:2015

for (i in 1:length(raster_anual)) {
rast <- terra::mask(crop(stack_br[[i]], sp_pr), sp_pr)
rast_bin <- ifel(rast == 3, 1, 0)
writeRaster(rast_bin, paste0(output, "sp_pr_forest_", ano[i], ".tif"), 
            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
}







