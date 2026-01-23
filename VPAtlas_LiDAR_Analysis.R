library(tidyverse) 
library(padr)
library(readxl)
library(reshape2)
library(MASS)
library(sf)
library(terra)
library(whitebox)
library(arcpullr)
library(nhdR)
library(mapview)
library(tidyterra)
library(patchwork)
library(gghalves)
library(ggbeeswarm)
library(RColorBrewer)

terraOptions(progress = 0)


vt_towns <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1/FeatureServer/0") %>%
  st_transform(crs = 32145)

# aoi <- vt_towns[vt_towns$CNTY == "11",]
aoi <- vt_towns[vt_towns$TOWNNAME %in% c("BERKSHIRE","RICHFORD","JAY","WESTFIELD","MONTGOMERY","ENOSBURG"),]

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


out_dir <- "~/R/VPAtlas_LiDAR/FranklinCo" #

wbt_init()
# wbt_max_procs(max_procs = NULL)


for (i in unique(aoi$TOWNNAME)) {

  start_time <- Sys.time()

  message("Processing town: ", i," ", Sys.time())

  # out_file <- paste0("~/R/VPAtlas_LiDAR/FranklinCo/combined_sf_", i, ".geojson")
  out_file <- paste0(out_dir,"/combined_sf_", i, ".geojson")

            if (file.exists(out_file)) { #skip town if it already exits in directory
              message("**SKIPPING** town: ", i, " (file already exists)")
              next }

  tryCatch({ #tryCatch to reduce errors during loop

    townbound <- aoi[aoi$TOWNNAME == i, ]

# ====================
# Data prep (crop -> project)
# ====================

    # Crop and download rasters in loop,
    # or else the statewide layer will be downloaded
    dem_cog <- crop(dem_cog_url, townbound) %>% project("EPSG:32145", method = "bilinear")

    satband_cog_21_22 <- crop(satband_cog_url_21_21, townbound) %>% project("EPSG:32145", method = "bilinear")
    names(satband_cog_21_22) <- c("Red_Band", "Green_Band", "Blue_Band", "NIR_Band") # name imagery color bands

    lc_cog <- crop(lc_cog_url, townbound) %>% project("EPSG:32145", method = "bilinear")

    lc_imperv_cog <- crop(lc_imperv_url, townbound) %>% project("EPSG:32145", method = "bilinear")

    vt_hydro_poly_int <- st_intersection(vt_hydro_poly, townbound)

    vt_hydro_lines_int <- st_intersection(vt_hydro_lines, townbound)

    vt_wetlands_int <- st_intersection(vt_wetlands, townbound)

    vt_buildings_int <- st_intersection(vt_buildings, townbound) %>% st_transform(crs = 32145)

# ====================
# DEM depression analysis
# ====================
    tmp_smooth  <- tempfile(fileext = ".tif") # create temp files; wbt functions must run off file path
    tmp_output  <- tempfile(fileext = ".tif")


    dem3 <- focal(dem_cog, matrix(1, nrow = 3, ncol = 3), fun = median, na.rm = TRUE) # smooth DEM, from Wu et al. 2014
    writeRaster(dem3, tmp_smooth, progress = TRUE, overwrite = TRUE) # write smoothed DEM to temp file

    message("Depression analysis begun: ", i," ", Sys.time())

    wbt_stochastic_depression_analysis(
      dem = tmp_smooth, # load smoothed DEM from temp file
      output = tmp_output, # save depression raster to temp file
      rmse = 0.095, # from DEM metadata
      range = 1.05, # 3*DEM resolution
      iterations = 50, # based on Wu et al. 2014
      wd = NULL)

    output <- rast(tmp_output) %>% # load depression raster to temp file
      mask(vt_hydro_poly_int, inverse = TRUE) %>%
      mask(vt_wetlands_int, inverse = TRUE) %>%
      mask(vt_hydro_lines_int, inverse = TRUE) %>%
      mask(vt_buildings_int, inverse = TRUE) %>%
      mask(resample(lc_cog, rast(tmp_output), method = "near"), inverse = FALSE) %>%
      mask(resample(lc_imperv_cog, rast(tmp_output), method = "near"), inverse = TRUE) %>%
      ifel(. < 0.8, NA, .) # remove depressions with < 80% chance of being a depression

    output_focal <- focal(output, matrix(1, nrow = 3, ncol = 3), fun = median, na.rm = TRUE) # remove gaps in pixel-dense areas

    depressions <- as.polygons(output_focal) %>% # turn into vector polygons
      st_as_sf() %>%
      st_cast("POLYGON")

    depressions$area <- as.numeric(st_area(depressions)) # calculate area (m)

    message("DEM analysis finished: ", i," ", Sys.time())

# ====================
# NDWI analysis
# ====================

    ndwi_raster <- (satband_cog_21_22$Green_Band - satband_cog_21_22$NIR_Band) / (satband_cog_21_22$Green_Band + satband_cog_21_22$NIR_Band) # calculate NDWI

    ndwi_raster <- ndwi_raster %>%
      mask(vt_hydro_poly_int, inverse = TRUE) %>%
      mask(vt_wetlands_int, inverse = TRUE) %>%
      mask(vt_hydro_lines_int, inverse = TRUE) %>%
      mask(vt_buildings_int, inverse = TRUE) %>%
      mask(resample(lc_cog, ndwi_raster, method = "near"), inverse = FALSE) %>%
      mask(resample(lc_imperv_cog, ndwi_raster, method = "near"), inverse = TRUE) %>%
      ifel(. < 0, NA, .) # Remove negative values indicating no standing water

    ndwi_focal <- focal(ndwi_raster, matrix(1, nrow = 9, ncol = 9), fun = max, na.rm = TRUE)

    standing_water <- as.polygons(ndwi_focal) %>%  # turn into vector polygons
      st_as_sf() %>%
      st_cast("POLYGON")

    standing_water$area <- as.numeric(st_area(standing_water)) # calculate area (m)

# ====================
# Join depressions & water, output polygons
# ====================
    dep_filtered <- depressions %>%
      filter(area > 50) %>% # filter out depressions < 50 m^2
      mutate(dep_id = row_number()) %>%
      dplyr::select(dep_id, area_dep = area)

    water_filtered <- standing_water %>%
      filter(area > 50) %>% # filter out standing water < 50 m^2
      mutate(water_id = row_number()) %>%
      dplyr::select(water_id, area_water = area)

    dep_side <- st_join(dep_filtered, water_filtered, join = st_intersects)

    water_only <- st_join(water_filtered, dep_filtered, join = st_intersects) %>%
      filter(is.na(dep_id))

    combined_sf <- bind_rows(dep_side, water_only) %>%
      mutate(status = case_when(
        !is.na(dep_id) & !is.na(water_id) ~ "Water_Depression",
        !is.na(dep_id) & is.na(water_id)  ~ "Depression",
        is.na(dep_id)  & !is.na(water_id) ~ "Water"),
        area_m = coalesce(area_dep, area_water),
        pred_status = as.factor(status)) %>%
      dplyr::select(final_area, area = area_m, geometry)

    st_write(combined_sf, out_file, append = FALSE, delete_dsn = TRUE)

    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")

    message("Finished town: ", i, " | Time elapsed: ", round(elapsed, 2), " minutes @", end_time)

              }, error = function(e) {
                message("**FAILED** town: ", i, " | ", e$message, "@", Sys.time())
              })
}
