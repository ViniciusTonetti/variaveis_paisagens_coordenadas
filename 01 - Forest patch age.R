# Code used to calculate the age of forest fragments (time elapsed since the last deforestation) 
# for the analysis used in Ballarin et al - Network-based assessment of habitat source–sink dynamics.
# 
# Written by Vinicius Tonetti* and reviewed by the other authors
# Contact email: vrtonetti@gmail.com


# Loading packages and cleaning environment ------------------------------------

library(terra)
rm(list = ls())


# Loading annual land cover mappings for Brazil --------------------------------

# Annual land-cover maps for Brazil were downloaded for the years 1985–2015.
# The data were obtained from MapBiomas Collection 10 on 28/04/2026
# through the direct download link: https://brasil.mapbiomas.org/en/colecoes-mapbiomas/

raster_anual <- list.files("data/mapbiomas/anual",
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


# Calculando idade da floresta -------------------------------------------------
################################################################################

## stack dos rasters cortados 

raster_anual_sp_pr <- list.files("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/anual",
                           pattern = "^sp_pr_forest.*\\.tif$", full.names = T)

stack_sp_pr <- terra::rast(raster_anual_sp_pr)



idade_floresta_2015 <- app(stack_sp_pr, fun = function(x) {
  
  # mantém NA se o pixel for NA em toda a série
  if (all(is.na(x))) return(NA)
  
  # se em 2015 não é floresta, idade = 0
  if (is.na(x[length(x)]) || x[length(x)] == 0) return(0)
  
  # conta anos consecutivos com floresta a partir de 2015 para trás
  r <- rev(x)
  primeiro_zero <- which(r == 0)[1]
  
  if (is.na(primeiro_zero)) {
    return(length(x))
  } else {
    return(primeiro_zero - 1)
  }
})


plot(idade_floresta_2015)

output <- "E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/anual/"

writeRaster(idade_floresta_2015, paste0(output, "idade_floresta_2015.tif"), 
            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)

