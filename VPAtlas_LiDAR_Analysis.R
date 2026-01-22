library(tidyverse)
library(reshape2)
library(gghalves)
library(whitebox)
library(RColorBrewer)
library(ggbeeswarm)
library(tidyterra)
library(sf)
library(mapview)
library(terra)
library(arcpullr)
library(nhdR)
library(mapview)
library(raster)
library(sf)
library(purrr)
library(tibble)

# library(progressr)
# handlers(global = TRUE)


vt_towns <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1/FeatureServer/0") %>%
  st_transform(crs = 32145)

# aoi <- vt_towns[vt_towns$CNTY == "11",]
aoi <- vt_towns[vt_towns$TOWNNAME %in% c("BERKSHIRE","RICHFORD"),]

vt_hydro_poly <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Water_VHDCARTO_poly_SP_v1/FeatureServer/0") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>% st_buffer(10)

vt_buildings <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_STRUCTURES_POLY_SP_v1/FeatureServer/0/") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>% st_buffer(5)

vt_hydro_lines <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Water_VHDCARTO_line_SP_v1/FeatureServer/0")  %>%
  st_transform(crs = 32145) %>% st_make_valid() %>% st_buffer(10)

vt_wetlands <- get_spatial_layer("https://services5.arcgis.com/Uzks6LSde6r23wwG/arcgis/rest/services/Vermont_Significant_Wetland_Inventory/FeatureServer/0") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>% st_buffer(10)

dem_cog_url <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/_Other/Projects/2023_Lidar/PreliminaryData/Statewide_2023_35cm_DEMHF.tif")
satband_cog_url_21_21 <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/Imagery/STATEWIDE_2021-2022_30cm_LeafOFF_4Band.tif")
lc_cog_url <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/STATEWIDE_2022_50cm_LANDCOVER_TreeCanopy.tif")
lc_imperv_url <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/STATEWIDE_2022_50cm_LANDCOVER_Impervious.tif")


out_dir <- "~/R/VPAtlas_LiDAR/FranklinCo"

wbt_init()
# wbt_max_procs(max_procs = NULL)


for (i in unique(aoi$TOWNNAME)) {

  message("Processing town: ", i)

  tryCatch({

    townbound <- aoi[aoi$TOWNNAME == i, ]

    # -------------------------
    # Data prep (crop -> project)
    # -------------------------
    dem_cog <- crop(dem_cog_url, townbound) %>%
      project("EPSG:32145", method = "bilinear")

    satband_cog_21_22 <- crop(satband_cog_url_21_21, townbound) %>%
      project("EPSG:32145", method = "bilinear")
    names(satband_cog_21_22) <- c("Red_Band", "Green_Band", "Blue_Band", "NIR_Band")

    lc_cog <- crop(lc_cog_url, townbound) %>%
      project("EPSG:32145", method = "bilinear")

    lc_imperv_cog <- crop(lc_imperv_url, townbound) %>%
      project("EPSG:32145", method = "bilinear")

    vt_hydro_poly_int <- st_intersection(vt_hydro_poly, townbound) %>%
      st_transform(crs = 32145)

    vt_hydro_lines_int <- st_intersection(vt_hydro_lines, townbound) %>%
      st_transform(crs = 32145) %>%
      st_buffer(5)

    vt_wetlands_int <- st_intersection(vt_wetlands, townbound) %>%
      st_transform(crs = 32145)

    vt_buildings_int <- st_intersection(vt_buildings, townbound) %>%
      st_transform(crs = 32145)

    # -------------------------
    # DEM depression analysis
    # -------------------------
    tmp_smooth  <- tempfile(fileext = ".tif")
    tmp_output  <- tempfile(fileext = ".tif")


    dem3 <- focal(dem_cog, matrix(1, nrow = 3, ncol = 3), fun = median, na.rm = TRUE)
    writeRaster(dem3, tmp_smooth, progress = TRUE, overwrite = TRUE)


    wbt_stochastic_depression_analysis(
      dem = tmp_smooth,
      output = tmp_output,
      rmse = 0.095,
      range = 1.05,
      iterations = 5,
      wd = NULL)

    output <- rast(tmp_output) %>%
      mask(vt_hydro_poly_int, inverse = TRUE) %>%
      mask(vt_wetlands_int, inverse = TRUE) %>%
      mask(vt_hydro_lines_int, inverse = TRUE) %>%
      mask(vt_buildings_int, inverse = TRUE) %>%
      mask(resample(lc_cog, rast(tmp_output), method = "near"), inverse = FALSE) %>%
      mask(resample(lc_imperv_cog, rast(tmp_output), method = "near"), inverse = TRUE) %>%
      ifel(. < 0.8, NA, .)

    output_focal <- focal(output, matrix(1, nrow = 3, ncol = 3), fun = median, na.rm = TRUE)

    depressions <- as.polygons(output_focal) %>%
      st_as_sf() %>%
      st_cast("POLYGON")

    depressions$area <- as.numeric(st_area(depressions))

    # -------------------------
    # NDWI analysis
    # -------------------------
    ndwi_raster <- (satband_cog_21_22$Green_Band - satband_cog_21_22$NIR_Band) /
      (satband_cog_21_22$Green_Band + satband_cog_21_22$NIR_Band)

    ndwi_raster <- ndwi_raster %>%
      mask(vt_hydro_poly_int, inverse = TRUE) %>%
      mask(vt_wetlands_int, inverse = TRUE) %>%
      mask(vt_hydro_lines_int, inverse = TRUE) %>%
      mask(vt_buildings_int, inverse = TRUE) %>%
      mask(resample(lc_cog, ndwi_raster, method = "near"), inverse = FALSE) %>%
      mask(resample(lc_imperv_cog, ndwi_raster, method = "near"), inverse = TRUE) %>%
      ifel(. < 0, NA, .)

    ndwi_focal <- focal(ndwi_raster, matrix(1, nrow = 9, ncol = 9), fun = max, na.rm = TRUE)

    standing_water <- as.polygons(ndwi_focal) %>%
      st_as_sf() %>%
      st_cast("POLYGON")

    standing_water$area <- as.numeric(st_area(standing_water))

    # -------------------------
    # Join depressions & water, output polygons
    # -------------------------
    dep_filtered <- depressions %>%
      filter(area > 50) %>%
      mutate(dep_id = row_number()) %>%
      dplyr::select(dep_id, area_dep = area)

    water_filtered <- standing_water %>%
      filter(area > 50) %>%
      mutate(water_id = row_number()) %>%
      dplyr::select(water_id, area_water = area)

    dep_side <- st_join(dep_filtered, water_filtered, join = st_intersects)

    water_only <- st_join(water_filtered, dep_filtered, join = st_intersects) %>%
      filter(is.na(dep_id))

    combined_sf <- bind_rows(dep_side, water_only) %>%
      mutate(status = case_when(
                  !is.na(dep_id) & !is.na(water_id) ~ "Water_Depress",
                  !is.na(dep_id) & is.na(water_id)  ~ "Depress",
                  is.na(dep_id)  & !is.na(water_id) ~ "Water"),
        final_area = coalesce(area_dep, area_water),
        statusf = as.factor(status)) %>%
      dplyr::select(statusf, area = final_area, geometry)

    out_file <- paste0("~/R/VPAtlas_LiDAR/FranklinCo/combined_sf_", i, ".geojson")
    st_write(combined_sf, out_file, append = FALSE, delete_dsn = TRUE)

    message("Finished town: ", i)

  }, error = function(e) {
    message("FAILED town: ", i, " | ", e$message)
  })
}



