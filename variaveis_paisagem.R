# Extraindo vaiárveis paisagem para coordenadas específicas

# pacotes e limpando ambiente --------------------------------------------------

rm(list = ls())

#remotes::install_github("mauriciovancine/atlanticr")

library(terra)
library(landscapemetrics)
library(dplyr)
library(tidyr)
library(usdm)
library(openxlsx)


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
################################################################################

mb_br_15_SIRGAS_crop <- terra::rast("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/mb_br_15_SIRGAS_crop_ext.tif")

# garantir que há um ID único por polígono
buf_5km$sample_id <- 1:nrow(buf_5km)

# calcular métricas por polígono
mets_5km <- sample_lsm(
  landscape = mb_br_15_SIRGAS_crop,
  y = buf_5km,
  what = c("lsm_c_pland", #% de floresta na paisagem
           "lsm_c_ed", # densidade de borda da floresta
           "lsm_c_lsi", # landscaoe shape index - complexidade das manchas, formas mais irregulares, mais borda
           "lsm_c_area_mn", # tamanho médio dos fragmentos de floresta
           "lsm_c_np", # número de fragmentos
           #"lsm_c_enn_mn", # distância média ao fragmento mais próximo # desconsiderei pq para buf500 m alguns valores apareceram como NA, muita floresta
           #"lsm_c_clumpy", # agregação dos fragmentos; alto = floresta mais contínua, baixo = fragmentada # desconsiderei pq para buf500 m alguns valores apareceram como NA
           "lsm_l_shdi", # índice de diversidade de shannon
           "lsm_l_pr", # número de tipos de uso da terra
           "lsm_c_core_mn"), # área média de interior de floresta
  edge_depth = 1 # profundidade de borda = 1 célula, ~30m
)


# manter apenas a classe floresta = 3
mets_forest_5km <- mets_5km %>%
  filter(class == 3) %>%
  rename(sample_id = plot_id) %>%
  select(sample_id, metric, value)


# converter para formato largo: uma linha por buffer
mets_forest_wide <- mets_forest_5km %>%
  pivot_wider(
    names_from = metric,
    values_from = value
  )


buf_att <- as.data.frame(buf_5km)

# juntar métricas aos municípios
result_5km <- buf_att %>%
  left_join(mets_forest_wide, by = "sample_id")

result_5km


# Calculando o VIF para o buffer de 5km ----------------------------------------

df_vif_5km <- result_5km %>% 
  select(area_mn, core_mn, ed, lsi, np, pland)

vif_5km <- vifstep(df_vif_5km, th = 5)
vif_5km

df_selected <- exclude(df_vif_5km, vif_5km)

# Variáveis mantidas após exclusão da correlação

# clumpy - agregação dos fragmentos; alto = floresta mais contínua, baixo = fragmentada
# ed - densidade de borda da floresta
# enm_mn - distância média ao fragmento mais próximo
# np - número de fragmentos
# pland - % de floresta na paisagem

variaveis_mantidas <- colnames(df_selected )


## Métricas para buffer 3km ----------------------------------------------------
################################################################################

# garantir que há um ID único por polígono
buf_3km$sample_id <- 1:nrow(buf_3km)

# calcular métricas por polígono
mets_3km <- sample_lsm(
  landscape = mb_br_15_SIRGAS_crop,
  y = buf_3km,
  what = c("lsm_c_pland", #% de floresta na paisagem
           "lsm_c_ed", # densidade de borda da floresta
           "lsm_c_lsi", # landscaoe shape index - complexidade das manchas, formas mais irregulares, mais borda
           "lsm_c_area_mn", # tamanho médio dos fragmentos de floresta
           "lsm_c_np", # número de fragmentos
           #"lsm_c_enn_mn", # distância média ao fragmento mais próximo # desconsiderei pq para buf500 m alguns valores apareceram como NA, muita floresta
           #"lsm_c_clumpy", # agregação dos fragmentos; alto = floresta mais contínua, baixo = fragmentada # desconsiderei pq para buf500 m alguns valores apareceram como NA
           "lsm_l_shdi", # índice de diversidade de shannon
           "lsm_l_pr", # número de tipos de uso da terra
           "lsm_c_core_mn"), # área média de interior de floresta
  edge_depth = 1 # profundidade de borda = 1 célula, ~30m
)


# manter apenas a classe floresta = 3
mets_forest_3km <- mets_3km %>%
  filter(class == 3) %>%
  rename(sample_id = plot_id) %>%
  select(sample_id, metric, value)


# converter para formato largo: uma linha por buffer
mets_forest_wide <- mets_forest_3km %>%
  pivot_wider(
    names_from = metric,
    values_from = value
  )


buf_att <- as.data.frame(buf_3km)

# juntar métricas aos municípios
result_3km <- buf_att %>%
  left_join(mets_forest_wide, by = "sample_id")

result_3km


## Métricas para buffer 2km ----------------------------------------------------
################################################################################

# garantir que há um ID único por polígono
buf_2km$sample_id <- 1:nrow(buf_2km)

# calcular métricas por polígono
mets_2km <- sample_lsm(
  landscape = mb_br_15_SIRGAS_crop,
  y = buf_2km,
  what = c("lsm_c_pland", #% de floresta na paisagem
           "lsm_c_ed", # densidade de borda da floresta
           "lsm_c_lsi", # landscaoe shape index - complexidade das manchas, formas mais irregulares, mais borda
           "lsm_c_area_mn", # tamanho médio dos fragmentos de floresta
           "lsm_c_np", # número de fragmentos
           #"lsm_c_enn_mn", # distância média ao fragmento mais próximo # desconsiderei pq para buf500 m alguns valores apareceram como NA, muita floresta
           #"lsm_c_clumpy", # agregação dos fragmentos; alto = floresta mais contínua, baixo = fragmentada # desconsiderei pq para buf500 m alguns valores apareceram como NA
           "lsm_l_shdi", # índice de diversidade de shannon
           "lsm_l_pr", # número de tipos de uso da terra
           "lsm_c_core_mn"), # área média de interior de floresta
  edge_depth = 1 # profundidade de borda = 1 célula, ~30m
)


# manter apenas a classe floresta = 3
mets_forest_2km <- mets_2km %>%
  filter(class == 3) %>%
  rename(sample_id = plot_id) %>%
  select(sample_id, metric, value)


# converter para formato largo: uma linha por buffer
mets_forest_wide <- mets_forest_2km %>%
  pivot_wider(
    names_from = metric,
    values_from = value
  )


buf_att <- as.data.frame(buf_2km)

# juntar métricas aos municípios
result_2km <- buf_att %>%
  left_join(mets_forest_wide, by = "sample_id")

head(result_2km)
head(result_3km)
head(result_5km)


## Métricas para buffer 1km ----------------------------------------------------
################################################################################

# garantir que há um ID único por polígono
buf_1km$sample_id <- 1:nrow(buf_1km)

# calcular métricas por polígono
mets_1km <- sample_lsm(
  landscape = mb_br_15_SIRGAS_crop,
  y = buf_1km,
  what = c("lsm_c_pland", #% de floresta na paisagem
           "lsm_c_ed", # densidade de borda da floresta
           "lsm_c_lsi", # landscaoe shape index - complexidade das manchas, formas mais irregulares, mais borda
           "lsm_c_area_mn", # tamanho médio dos fragmentos de floresta
           "lsm_c_np", # número de fragmentos
           #"lsm_c_enn_mn", # distância média ao fragmento mais próximo # desconsiderei pq para buf500 m alguns valores apareceram como NA, muita floresta
           #"lsm_c_clumpy", # agregação dos fragmentos; alto = floresta mais contínua, baixo = fragmentada # desconsiderei pq para buf500 m alguns valores apareceram como NA
           "lsm_l_shdi", # índice de diversidade de shannon
           "lsm_l_pr", # número de tipos de uso da terra
           "lsm_c_core_mn"), # área média de interior de floresta
  edge_depth = 1 # profundidade de borda = 1 célula, ~30m
)


# manter apenas a classe floresta = 3
mets_forest_1km <- mets_1km %>%
  filter(class == 3) %>%
  rename(sample_id = plot_id) %>%
  select(sample_id, metric, value)


# converter para formato largo: uma linha por buffer
mets_forest_wide <- mets_forest_1km %>%
  pivot_wider(
    names_from = metric,
    values_from = value
  )


buf_att <- as.data.frame(buf_1km)

# juntar métricas aos municípios
result_1km <- buf_att %>%
  left_join(mets_forest_wide, by = "sample_id")

head(result_1km)
head(result_2km)
head(result_3km)
head(result_5km)


## Métricas para buffer 500 m --------------------------------------------------
################################################################################

# garantir que há um ID único por polígono
buf_500m$sample_id <- 1:nrow(buf_500m)

# calcular métricas por polígono
mets_500m <- sample_lsm(
  landscape = mb_br_15_SIRGAS_crop,
  y = buf_500m,
  what = c("lsm_c_pland", #% de floresta na paisagem
           "lsm_c_ed", # densidade de borda da floresta
           "lsm_c_lsi", # landscaoe shape index - complexidade das manchas, formas mais irregulares, mais borda
           "lsm_c_area_mn", # tamanho médio dos fragmentos de floresta
           "lsm_c_np", # número de fragmentos
           #"lsm_c_enn_mn", # distância média ao fragmento mais próximo # desconsiderei pq para buf500 m alguns valores apareceram como NA, muita floresta
           #"lsm_c_clumpy", # agregação dos fragmentos; alto = floresta mais contínua, baixo = fragmentada # desconsiderei pq para buf500 m alguns valores apareceram como NA
           "lsm_l_shdi", # índice de diversidade de shannon
           "lsm_l_pr", # número de tipos de uso da terra
           "lsm_c_core_mn"), # área média de interior de floresta
  edge_depth = 1 # profundidade de borda = 1 célula, ~30m
)


# manter apenas a classe floresta = 3
mets_forest_500m <- mets_500m %>%
  filter(class == 3) %>%
  rename(sample_id = plot_id) %>%
  select(sample_id, metric, value)


# converter para formato largo: uma linha por buffer
mets_forest_wide <- mets_forest_500m %>%
  pivot_wider(
    names_from = metric,
    values_from = value
  )


buf_att <- as.data.frame(buf_500m)

# juntar métricas aos municípios
result_500m <- buf_att %>%
  left_join(mets_forest_wide, by = "sample_id")

head(result_500m)
head(result_1km)
head(result_2km)
head(result_3km)
head(result_5km)


## Exportando os resultados para excel -----------------------------------------
################################################################################

variaveis_mantidas

result_500m <- result_500m[, c("mun", "latitude", "longitude", "sample_id", variaveis_mantidas)]
result_1km <- result_1km[, c("mun", "latitude", "longitude", "sample_id", variaveis_mantidas)]
result_2km <- result_2km[, c("mun", "latitude", "longitude", "sample_id", variaveis_mantidas)]
result_3km <- result_3km[, c("mun", "latitude", "longitude", "sample_id", variaveis_mantidas)]
result_5km <- result_5km[, c("mun", "latitude", "longitude", "sample_id", variaveis_mantidas)]


# criar workbook
wb <- createWorkbook()

# adicionar abas com os nomes desejados
addWorksheet(wb, "result_500m")
addWorksheet(wb, "result_1km")
addWorksheet(wb, "result_2km")
addWorksheet(wb, "result_3km")
addWorksheet(wb, "result_5km")

# escrever dados em cada aba
writeData(wb, "result_500m", result_500m)
writeData(wb, "result_1km", result_1km)
writeData(wb, "result_2km", result_2km)
writeData(wb, "result_3km", result_3km)
writeData(wb, "result_5km", result_5km)

# salvar arquivo
output <- "E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/planilha_resultados/"
saveWorkbook(wb, paste0(output, "landscape_metrics.xlsx"), overwrite = TRUE)




