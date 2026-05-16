# Code used to calculate landscape variables for the analysis used in
# Ballarin et al - Network-based assessment of habitat source–sink dynamics.
# 
# Written by Vinicius Tonetti* and reviewed by the other authors
# Contact email: vrtonetti@gmail.com


# Loading packages and cleaning environment ------------------------------------

library(terra)
library(landscapemetrics)
library(tidyverse)
library(usdm)
library(openxlsx)

rm(list = ls())


# Sampling locations and landscape buffers -------------------------------------

pts <- terra::vect("data/points/pts_paisagens.shp")


# Retain only the sampling points used in the analyses --------------------------

pts_filtered <- pts[!pts$mun %in% c("Campo Mour�o", "Cruzeiro", "Piraju", "Santa Helena"), ]
pts_filtered$mun


# Converting to the Coordinate Reference System to SIRGAS 2000 / Brazil Mercator (EPSG 5641) 
# to create different buffer sizes around sampling locations

pts_sirgas <- terra::project(pts_filtered, "EPSG:5641")


# Creating buffers

buf_500m <- terra::buffer(pts_sirgas, width = 500)
buf_1km <- terra::buffer(pts_sirgas, width = 1000)
buf_2km <- terra::buffer(pts_sirgas, width = 2000)
buf_3km <- terra::buffer(pts_sirgas, width = 3000)
buf_5km <- terra::buffer(pts_sirgas, width = 5000)


# # Reprojecting to WGS84 to extract MapBiomas data 

buf_500m <- terra::project(buf_500m, "EPSG:4326")
buf_1km <- terra::project(buf_1km, "EPSG:4326")
buf_2km <- terra::project(buf_2km, "EPSG:4326")
buf_3km <- terra::project(buf_3km, "EPSG:4326")
buf_5km <- terra::project(buf_5km, "EPSG:4326")


# Land-cover map for 2015 from MapBiomas Collection 10 -------------------------

mb_br_15 <- terra::rast("data/mapbiomas/brazil_coverage_2015.tif")


# Crop the 2015 raster to the extent of the 5-km buffers -----------------------

mb_br_15 <- terra::rast("data/mapbiomas/brazil_coverage_2015.tif") # path for the output folder where MapBiomas data
                                                                   # for the year 2015 is saved

mb_br_15_crop <- terra::crop(mb_br_15, buf_5km)

output <- "data/mapbiomas/" # path for the output folder where cropped layers will be saved


writeRaster(mb_br_15_crop,
            paste0(output, "mb_br_15_crop_WGS84.tif"),     
            gdal=c("COMPRESS=DEFLATE", "TFW=YES"),
            overwrite = T)


################################################################################
## Extracting landscape metrics for the 5-km buffer ----------------------------
################################################################################

# Loading forest patch age raster ----------------------------------------------

forest_age_2015 <- terra::rast("data/mapbiomas/anual/forest_age_2015.tif")


forest_age_by_landscape <- terra::extract(forest_age_2015, buf_5km, df = TRUE) %>%
  rename(age = lyr.1) %>%
  group_by(ID) %>%
  summarise(sum_forest_age_2015 = sum(idade[age > 0], na.rm = TRUE),
            n_pixels_florest_age = sum(age > 0, na.rm = TRUE),
            mean_forest_age = ifelse(n_pixels_florest_age > 0,
                                     sum_forest_age_2015 / n_pixels_florest_age,
                                     NA_real_), .groups = "drop") %>% 
  select(ID, mean_forest_age) %>% 
  rename(plot_id = ID)


# Calculating additional landscape metrics -------------------------------------

mb_br_15_WGS84_crop <- terra::rast("data/mapbiomas/mb_br_15_crop_WGS84.tif")

mets_5km <- sample_lsm(
  landscape = mb_br_15_WGS84_crop,
  y = buf_5km,
  what = c(
    "lsm_c_pland",    # percentage of forest cover in the landscape
    "lsm_c_ed",       # forest edge density
    "lsm_c_lsi",      # Landscape Shape Index: patch shape complexity; more irregular shapes indicate greater edge influence
    "lsm_c_area_mn",  # mean forest patch area
    "lsm_c_np",       # number of forest patches
    "lsm_l_shdi",     # Shannon diversity index
    "lsm_l_pr",       # number of land-cover classes
    "lsm_c_core_mn"   # mean forest core area
  ),
  edge_depth = 1      # edge depth = 1 cell (~30 m)
)


# Landscape-level metrics: Shannon diversity index and number of land-cover classes
mets_landscape <- mets_5km %>% 
  filter(
    level == "landscape",
    metric %in% c("shdi", "pr")
  )

# Metrics calculated for pixels classified as forest (class 3 in MapBiomas)
mets_forest <- mets_5km %>% 
  filter(
    level == "class",
    class == 3
  )

# Joining tibbles based on plot ID
mets_comb_5km <- mets_landscape %>%
  select(plot_id, metric, value) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  left_join(mets_forest %>%
            select(plot_id, metric, value) %>%
            tidyr::pivot_wider(names_from = metric, values_from = value),
      by = "plot_id") %>%
  left_join(idade_por_paisagem,
            by = "plot_id")

mets_comb_5km$mun_name <- buf_5km$mun


# Calculating VIF for the 5-km buffer ------------------------------------------

mets_comb_df <- data.frame(mets_comb_5km[, 2:10])

vif_5km <- vifstep(mets_comb_df, th = 3)
vif_5km

df_selected <- exclude(mets_comb_df, vif_5km)


# Variables retained after removing collinearity -------------------------------

# pr          - number of land-cover classes
# shdi        - Shannon diversity index
# core_mn     - mean forest core area
# np          - number of forest patches
# forest_age  - age of forest pixels

selected_variables <- colnames(df_selected)


################################################################################
## Extracting landscape metrics for the 3-km buffer ----------------------------
################################################################################

# Loading forest patch age raster ----------------------------------------------

forest_age_2015 <- terra::rast("data/mapbiomas/anual/forest_age_2015.tif")


forest_age_by_landscape <- terra::extract(forest_age_2015, buf_3km, df = TRUE) %>%
  rename(age = lyr.1) %>%
  group_by(ID) %>%
  summarise(sum_forest_age_2015 = sum(idade[age > 0], na.rm = TRUE),
            n_pixels_florest_age = sum(age > 0, na.rm = TRUE),
            mean_forest_age = ifelse(n_pixels_florest_age > 0,
                                     sum_forest_age_2015 / n_pixels_florest_age,
                                     NA_real_), .groups = "drop") %>% 
  select(ID, mean_forest_age) %>% 
  rename(plot_id = ID)


# Calculating additional landscape metrics -------------------------------------

mb_br_15_WGS84_crop <- terra::rast("data/mapbiomas/mb_br_15_crop_WGS84.tif")

mets_3km <- sample_lsm(
  landscape = mb_br_15_WGS84_crop,
  y = buf_3km,
  what = c(
    "lsm_c_pland",    # percentage of forest cover in the landscape
    "lsm_c_ed",       # forest edge density
    "lsm_c_lsi",      # Landscape Shape Index: patch shape complexity; more irregular shapes indicate greater edge influence
    "lsm_c_area_mn",  # mean forest patch area
    "lsm_c_np",       # number of forest patches
    "lsm_l_shdi",     # Shannon diversity index
    "lsm_l_pr",       # number of land-cover classes
    "lsm_c_core_mn"   # mean forest core area
  ),
  edge_depth = 1      # edge depth = 1 cell (~30 m)
)


# Landscape-level metrics: Shannon diversity index and number of land-cover classes
mets_landscape <- mets_3km %>% 
  filter(
    level == "landscape",
    metric %in% c("shdi", "pr")
  )

# Metrics calculated for pixels classified as forest (class 3 in MapBiomas)
mets_forest <- mets_3km %>% 
  filter(
    level == "class",
    class == 3
  )

# Joining tibbles based on plot ID
mets_comb_3km <- mets_landscape %>%
  select(plot_id, metric, value) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  left_join(mets_forest %>%
              select(plot_id, metric, value) %>%
              tidyr::pivot_wider(names_from = metric, values_from = value),
            by = "plot_id") %>%
  left_join(idade_por_paisagem,
            by = "plot_id")

mets_comb_3km$mun_name <- buf_3km$mun


################################################################################
## Extracting landscape metrics for the 2-km buffer ----------------------------
################################################################################

# Loading forest patch age raster ----------------------------------------------

forest_age_2015 <- terra::rast("data/mapbiomas/anual/forest_age_2015.tif")


forest_age_by_landscape <- terra::extract(forest_age_2015, buf_2km, df = TRUE) %>%
  rename(age = lyr.1) %>%
  group_by(ID) %>%
  summarise(sum_forest_age_2015 = sum(idade[age > 0], na.rm = TRUE),
            n_pixels_florest_age = sum(age > 0, na.rm = TRUE),
            mean_forest_age = ifelse(n_pixels_florest_age > 0,
                                     sum_forest_age_2015 / n_pixels_florest_age,
                                     NA_real_), .groups = "drop") %>% 
  select(ID, mean_forest_age) %>% 
  rename(plot_id = ID)


# Calculating additional landscape metrics -------------------------------------

mb_br_15_WGS84_crop <- terra::rast("data/mapbiomas/mb_br_15_crop_WGS84.tif")

mets_2km <- sample_lsm(
  landscape = mb_br_15_WGS84_crop,
  y = buf_2km,
  what = c(
    "lsm_c_pland",    # percentage of forest cover in the landscape
    "lsm_c_ed",       # forest edge density
    "lsm_c_lsi",      # Landscape Shape Index: patch shape complexity; more irregular shapes indicate greater edge influence
    "lsm_c_area_mn",  # mean forest patch area
    "lsm_c_np",       # number of forest patches
    "lsm_l_shdi",     # Shannon diversity index
    "lsm_l_pr",       # number of land-cover classes
    "lsm_c_core_mn"   # mean forest core area
  ),
  edge_depth = 1      # edge depth = 1 cell (~30 m)
)


# Landscape-level metrics: Shannon diversity index and number of land-cover classes
mets_landscape <- mets_2km %>% 
  filter(
    level == "landscape",
    metric %in% c("shdi", "pr")
  )

# Metrics calculated for pixels classified as forest (class 3 in MapBiomas)
mets_forest <- mets_2km %>% 
  filter(
    level == "class",
    class == 3
  )

# Joining tibbles based on plot ID
mets_comb_2km <- mets_landscape %>%
  select(plot_id, metric, value) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  left_join(mets_forest %>%
              select(plot_id, metric, value) %>%
              tidyr::pivot_wider(names_from = metric, values_from = value),
            by = "plot_id") %>%
  left_join(idade_por_paisagem,
            by = "plot_id")

mets_comb_2km$mun_name <- buf_2km$mun


################################################################################
## Extracting landscape metrics for the 1-km buffer ----------------------------
################################################################################

# Loading forest patch age raster ----------------------------------------------

forest_age_2015 <- terra::rast("data/mapbiomas/anual/forest_age_2015.tif")


forest_age_by_landscape <- terra::extract(forest_age_2015, buf_1km, df = TRUE) %>%
  rename(age = lyr.1) %>%
  group_by(ID) %>%
  summarise(sum_forest_age_2015 = sum(idade[age > 0], na.rm = TRUE),
            n_pixels_florest_age = sum(age > 0, na.rm = TRUE),
            mean_forest_age = ifelse(n_pixels_florest_age > 0,
                                     sum_forest_age_2015 / n_pixels_florest_age,
                                     NA_real_), .groups = "drop") %>% 
  select(ID, mean_forest_age) %>% 
  rename(plot_id = ID)


# Calculating additional landscape metrics -------------------------------------

mb_br_15_WGS84_crop <- terra::rast("data/mapbiomas/mb_br_15_crop_WGS84.tif")

mets_1km <- sample_lsm(
  landscape = mb_br_15_WGS84_crop,
  y = buf_1km,
  what = c(
    "lsm_c_pland",    # percentage of forest cover in the landscape
    "lsm_c_ed",       # forest edge density
    "lsm_c_lsi",      # Landscape Shape Index: patch shape complexity; more irregular shapes indicate greater edge influence
    "lsm_c_area_mn",  # mean forest patch area
    "lsm_c_np",       # number of forest patches
    "lsm_l_shdi",     # Shannon diversity index
    "lsm_l_pr",       # number of land-cover classes
    "lsm_c_core_mn"   # mean forest core area
  ),
  edge_depth = 1      # edge depth = 1 cell (~30 m)
)


# Landscape-level metrics: Shannon diversity index and number of land-cover classes
mets_landscape <- mets_1km %>% 
  filter(
    level == "landscape",
    metric %in% c("shdi", "pr")
  )

# Metrics calculated for pixels classified as forest (class 3 in MapBiomas)
mets_forest <- mets_1km %>% 
  filter(
    level == "class",
    class == 3
  )

# Joining tibbles based on plot ID
mets_comb_1km <- mets_landscape %>%
  select(plot_id, metric, value) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  left_join(mets_forest %>%
              select(plot_id, metric, value) %>%
              tidyr::pivot_wider(names_from = metric, values_from = value),
            by = "plot_id") %>%
  left_join(idade_por_paisagem,
            by = "plot_id")

mets_comb_1km$mun_name <- buf_1km$mun


################################################################################
## Extracting landscape metrics for the 500-m buffer ---------------------------
################################################################################

# Loading forest patch age raster ----------------------------------------------

forest_age_2015 <- terra::rast("data/mapbiomas/anual/forest_age_2015.tif")


forest_age_by_landscape <- terra::extract(forest_age_2015, buf_500m, df = TRUE) %>%
  rename(age = lyr.1) %>%
  group_by(ID) %>%
  summarise(sum_forest_age_2015 = sum(idade[age > 0], na.rm = TRUE),
            n_pixels_florest_age = sum(age > 0, na.rm = TRUE),
            mean_forest_age = ifelse(n_pixels_florest_age > 0,
                                     sum_forest_age_2015 / n_pixels_florest_age,
                                     NA_real_), .groups = "drop") %>% 
  select(ID, mean_forest_age) %>% 
  rename(plot_id = ID)


# Calculating additional landscape metrics -------------------------------------

mb_br_15_WGS84_crop <- terra::rast("data/mapbiomas/mb_br_15_crop_WGS84.tif")

mets_500m <- sample_lsm(
  landscape = mb_br_15_WGS84_crop,
  y = buf_500m,
  what = c(
    "lsm_c_pland",    # percentage of forest cover in the landscape
    "lsm_c_ed",       # forest edge density
    "lsm_c_lsi",      # Landscape Shape Index: patch shape complexity; more irregular shapes indicate greater edge influence
    "lsm_c_area_mn",  # mean forest patch area
    "lsm_c_np",       # number of forest patches
    "lsm_l_shdi",     # Shannon diversity index
    "lsm_l_pr",       # number of land-cover classes
    "lsm_c_core_mn"   # mean forest core area
  ),
  edge_depth = 1      # edge depth = 1 cell (~30 m)
)


# Landscape-level metrics: Shannon diversity index and number of land-cover classes
mets_landscape <- mets_500m %>% 
  filter(
    level == "landscape",
    metric %in% c("shdi", "pr")
  )

# Metrics calculated for pixels classified as forest (class 3 in MapBiomas)
mets_forest <- mets_500m %>% 
  filter(
    level == "class",
    class == 3
  )

# Joining tibbles based on plot ID
mets_comb_500m <- mets_landscape %>%
  select(plot_id, metric, value) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  left_join(mets_forest %>%
              select(plot_id, metric, value) %>%
              tidyr::pivot_wider(names_from = metric, values_from = value),
            by = "plot_id") %>%
  left_join(idade_por_paisagem,
            by = "plot_id")

mets_comb_500m$mun_name <- buf_500m$mun


## Exporting results to Excel --------------------------------------------------
################################################################################

results_5km <- mets_comb_5km[, c('mun_name', selected_variables)]
results_3km <- mets_comb_3km[, c('mun_name', selected_variables)]
results_2km <- mets_comb_2km[, c('mun_name', selected_variables)]
results_1km <- mets_comb_1km[, c('mun_name', selected_variables)]
results_500m <- mets_comb_500m[, c('mun_name', selected_variables)]

head(results_500m)
head(results_1km)
head(results_2km)
head(results_3km)
head(results_5km)

# Create workbook
wb <- createWorkbook()

# Add worksheets naming them with the results for each buffer
addWorksheet(wb, "results_500m")
addWorksheet(wb, "results_1km")
addWorksheet(wb, "results_2km")
addWorksheet(wb, "results_3km")
addWorksheet(wb, "results_5km")

# Saving data in each tab
writeData(wb, "results_500m", results_500m)
writeData(wb, "results_1km", results_1km)
writeData(wb, "results_2km", results_2km)
writeData(wb, "results_3km", results_3km)
writeData(wb, "results_5km", results_5km)

# Save workbook
output <- "data/results/"

saveWorkbook(
  wb,
  paste0(output, "landscape_metrics.xlsx"),
  overwrite = TRUE
)




