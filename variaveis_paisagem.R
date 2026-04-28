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


# Considerando apenas os pontos que foram usados nas análises

pts$mun


pts_filtered <- pts[!pts$mun %in% c("Campo Mour�o", "Cruzeiro", "Piraju", "Santa Helena"), ]

pts_filtered$mun


# convertendopara sirgas para criar os buffers
pts_sirgas <- terra::project(pts_filtered, "EPSG:5641")


# criando os buffers de 2 km

buf_500m <- terra::buffer(pts_sirgas, width = 500)
buf_1km <- terra::buffer(pts_sirgas, width = 1000)
buf_2km <- terra::buffer(pts_sirgas, width = 2000)
buf_3km <- terra::buffer(pts_sirgas, width = 3000)
buf_5km <- terra::buffer(pts_sirgas, width = 5000)


# reconvertendo para WGS84

buf_500m <- terra::project(buf_500m, "EPSG:4326")
buf_1km <- terra::project(buf_1km, "EPSG:4326")
buf_2km <- terra::project(buf_2km, "EPSG:4326")
buf_3km <- terra::project(buf_3km, "EPSG:4326")
buf_5km <- terra::project(buf_5km, "EPSG:4326")




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


## Cortando raster 2015 para a extensão dos buffers ano 2015

#mb_br_15 <- terra::rast("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/brazil_coverage_2015.tif")

#mb_br_15_crop <- crop(mb_br_15, buf_5km)

#output <- "E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/"


#writeRaster(mb_br_15_crop, paste0(output, "mb_br_15_crop_WGS84.tif"),     
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


## Métricas para buffer 5km ----------------------------------------------------
################################################################################

# calculando idade dos frags ---------------------------------------------------

idade_floresta_2015 <- terra::rast("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/anual/idade_floresta_2015.tif")


idade_por_paisagem <- terra::extract(idade_floresta_2015, buf_5km, df = TRUE) %>%
  rename(idade = lyr.1) %>%
  group_by(ID) %>%
  summarise(soma_idade_floresta = sum(idade[idade > 0], na.rm = TRUE),
            n_pixels_floresta = sum(idade > 0, na.rm = TRUE),
            idade_media_floresta = ifelse(n_pixels_floresta > 0,
                                          soma_idade_floresta / n_pixels_floresta,
                                          NA_real_), .groups = "drop") %>% 
  select(ID, idade_media_floresta) %>% 
  rename(plot_id = ID)


# Calculando demais métricas da paisagem ---------------------------------------

mb_br_15_WGS84_crop <- terra::rast("E:/_PESSOAL/ViniciusT/variaveis paisagem coordenadas/mapbiomas/mb_br_15_crop_WGS84.tif")


# calcular métricas por polígono

mets_5km <- sample_lsm(
  landscape = mb_br_15_WGS84_crop ,
  y = buf_5km,
  what = c("lsm_c_pland", #% de floresta na paisagem
           "lsm_c_ed", # densidade de borda da floresta
           "lsm_c_lsi", # landscaoe shape index - complexidade das manchas, formas mais irregulares, mais borda
           "lsm_c_area_mn", # tamanho médio dos fragmentos de floresta
           "lsm_c_np", # número de fragmentos
           "lsm_l_shdi", # índice de diversidade de shannon
           "lsm_l_pr", # número de tipos de uso da terra
           "lsm_c_core_mn"), # área média de interior de floresta
  edge_depth = 1) # profundidade de borda = 1 célula, ~30m


# métricas de paisagem, shannon e número de tipos de cobertura da terra

mets_landscape <- mets_5km %>% 
  filter(level == "landscape",
         metric %in% c("shdi", "pr"))

# métricas de classe 3, floresta

mets_forest <- mets_5km %>% 
  filter(level == "class",
         class == 3)

# Unindo os tibbles baseado em plot id

mets_comb <- mets_landscape %>%
  select(plot_id, metric, value) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  left_join(mets_forest %>%
            select(plot_id, metric, value) %>%
            tidyr::pivot_wider(names_from = metric, values_from = value),
      by = "plot_id") %>%
  left_join(idade_por_paisagem,
            by = "plot_id")


mets_comb$mun_name <- buf_5km$mun


# Calculando o VIF para o buffer de 5km ----------------------------------------

mets_comb_df <- data.frame(mets_comb[,2:10])

vif_5km <- vifstep(mets_comb_df, th = 3)
vif_5km

df_selected <- exclude(mets_comb_df, vif_5km)

# Variáveis mantidas após exclusão da correlação

# pr - número de tipos de uso da terra
# shdi - índice de diversidade de shannon
# core_mn - área média de interior de floresta
# np - número de fragmentos
# idade dos pixels de floresta

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




