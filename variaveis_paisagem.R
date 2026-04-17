# Extraindo vaiárveis paisagem para coordenadas específicas


# pacotes e limpando ambiente --------------------------------------------------

rm(list = ls())

#remotes::install_github("mauriciovancine/atlanticr")

library(terra)
library(landscapemetrics)
library(dplyr)



# pontos e buffers -------------------------------------------------------------

pts <- terra::vect("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/pontos/pts_paisagens.shp")


# convertendopara sirgas para criar os buffers
pts_sirgas <- terra::project(pts, "EPSG:5641")

# criando os buffers de 2 km

buf_500m <- terra::buffer(pts_sirgas, width = 500)
buf_1km <- terra::buffer(pts_sirgas, width = 1000)
buf_2km <- terra::buffer(pts_sirgas, width = 2000)
buf_3km <- terra::buffer(pts_sirgas, width = 3000)
buf_5km <- terra::buffer(pts_sirgas, width = 5000)




# Cobertura da terra ano 2022 coleção 10 MapBiomas -----------------------------

mb_br_22 <- terra::rast("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/brazil_coverage_2022.tif")

mb_br_22_SIRGAS <- project(mb_br_22, "EPSG:5641", method = "near")

output <- "E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/"
  
writeRaster(mb_br_22_SIRGAS, paste0(output, "mb_br_22_SIRGAS.tif"),     
            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


# cortando a extensão do mapbiomas para a extensão dos polígonos

mb_br_22_SIRGAS_crop <- mask(mb_br_22_SIRGAS, vect(as.polygons(ext(buf_5km))))


# garantir que o raster é categórico

mb_m <- as.int(mb_m)






