# Extraindo vaiárveis paisagem para coordenadas específicas


# pacotes e limpando ambiente --------------------------------------------------

rm(list = ls())

#remotes::install_github("mauriciovancine/atlanticr")

library(terra)
library(atlanticr)

unique(atlanticr::atlantic_spatial$metric_group)



# pontos e buffers -------------------------------------------------------------

pts <- terra::vect("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/pontos/pts_paisagens.shp")


# cobvertendo para sirgas para criar os buffers
pts_sirgas <- terra::project(pts, "EPSG:5641")

# criando os buffers de 2 km

buf_500m <- terra::buffer(pts_sirgas, width = 500)
buf_1km <- terra::buffer(pts_sirgas, width = 1000)
buf_2km <- terra::buffer(pts_sirgas, width = 2000)
buf_3km <- terra::buffer(pts_sirgas, width = 3000)
buf_5km <- terra::buffer(pts_sirgas, width = 5000)





