# Extraindo vaiárveis paisagem para coordenadas específicas

# pacotes e limpando ambiente --------------------------------------------------

rm(list = ls())

#remotes::install_github("mauriciovancine/atlanticr")

library(terra)
library(landscapemetrics)
library(dplyr)
library(tidyr)


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

#mb_br_15 <- terra::rast("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/brazil_coverage_2015.tif")

#mb_br_15_SIRGAS <- project(mb_br_15, "EPSG:5641", method = "near")

#output <- "E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/"
  
#writeRaster(mb_br_15_SIRGAS, paste0(output, "mb_br_15_SIRGAS.tif"),     
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


# cortando a extensão do mapbiomas para a extensão dos polígonos

#mb_br_15_SIRGAS_crop <- crop(mb_br_15_SIRGAS, buf_5km)

#plot(mb_br_15_SIRGAS_crop)# garantir que o raster é categórico

#mb_br_15_SIRGAS_crop <- as.int(mb_br_15_SIRGAS_crop)


#writeRaster(mb_br_15_SIRGAS_crop, paste0(output, "mb_br_15_SIRGAS_crop_ext.tif"),     
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)



## Métricas para buffer 5km ----------------------------------------------------

mb_br_15_SIRGAS_crop <- terra::rast("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/mb_br_15_SIRGAS_crop_ext.tif")

# garantir que há um ID único por polígono
buf_5km$sample_id <- 1:nrow(buf_5km)

# calcular métricas por polígono
mets <- sample_lsm(
  landscape = mb_br_15_SIRGAS_crop,
  y = buf_5km,
  what = c("lsm_c_pland", #% de floresta no buffer
           "lsm_c_ed", # densidade de borda da floresta
           "lsm_c_lsi", # landscaoe shape index - complexidade das manchas, formas mais irregulares, mais borda
           "lsm_c_area_mn", # tamanho médio dos fragmentos de floresta
           "lsm_c_np", # número de fragmentos
           "lsm_c_enn_mn", # distância média ao fragmento mais próximo
           "lsm_c_clumpy", # agregação dos fragmentos; alto = floresta mais contínua, baixo = fragmentada
           "lsm_l_shdi", # índice de diversidade de shannon
           "lsm_l_pr", # número de tipos de uso da terra
           "lsm_c_core_mn"), # área média de interior de floresta
  edge_depth = 1 # profundidade de borda = 1 célula, ~30m
)


# manter apenas a classe floresta = 3
mets_forest <- mets %>%
  filter(class == 3) %>%
  rename(sample_id = plot_id) %>%
  select(sample_id, metric, value)


# converter para formato largo: uma linha por buffer
mets_forest_wide <- mets_forest %>%
  pivot_wider(
    names_from = metric,
    values_from = value
  )


buf_att <- as.data.frame(buf_5km)

# juntar métricas aos municípios
result <- buf_att %>%
  left_join(mets_forest_wide, by = "sample_id")

result


