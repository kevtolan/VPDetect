library(tidyverse) # data manipulation, workflow
library(sf) # vector data
library(terra) # raster data
library(whitebox) # follow install directions here https://www.whiteboxgeo.com/manual/wbt_book/r_interface.html
library(arcpullr) # download ESRI-hosted data
# library(tidyterra)
library(cli) # color outputs
library(beepr)

terraOptions(progress = 0) # prevent progress bars during raster functions

# ====================
# Define area of interest
# ====================
# vt_towns <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1/FeatureServer/0") %>%
  # st_transform(crs = 32145) # town boundaries
# aoi <- vt_towns[vt_towns$CNTY == "11",] # by county
# aoi <- vt_towns[vt_towns$TOWNNAME %in% c("BERKSHIRE","RICHFORD","JAY","WESTFIELD","MONTGOMERY","ENOSBURG"),] # by town
# aoi <- vt_towns[vt_towns$TOWNNAME %in% c("WINOOSKI","VERGENNES","SAINT ALBANS CITY"),] # by town

# ====================
# Load vectors
# These are ESRI-hosted vector layers
# ====================
vt_blocks <- get_spatial_layer("https://services1.arcgis.com/d3OaJoSAh2eh6OA9/ArcGIS/rest/services/Vermont_Wildlife_Atlasing_Blocks/FeatureServer/0") %>%
  st_transform(crs = 32145)
aoi <- vt_blocks[vt_blocks$GEOUNITDES == "Franklin County",] # by block

vt_hydro_poly <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Water_VHDCARTO_poly_SP_v1/FeatureServer/0") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>% st_buffer(10) #hydrology polygons

vt_buildings <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_STRUCTURES_POLY_SP_v1/FeatureServer/0/") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>% st_buffer(5) # building footprints

vt_hydro_lines <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Water_VHDCARTO_line_SP_v1/FeatureServer/0")  %>%
  st_transform(crs = 32145) %>% st_make_valid() %>% st_buffer(10) # hydrology lines

vt_wetlands <- get_spatial_layer("https://services5.arcgis.com/Uzks6LSde6r23wwG/arcgis/rest/services/Vermont_Significant_Wetland_Inventory/FeatureServer/0") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>% st_buffer(10) # wetlands

# ====================
# Load cloud-hosted geotiffs https://vcgi.vermont.gov/resources/how-and-education-resources/how-use-cloud-optimized-geotiffs-cogs
# This isn't downloading the rasters, just creating a link to the raster download.
# During the loop, each raster will be clipped to a town boundary, then downloaded to a temp object
# If a computer can't handle these larger chunks of land, use a grid system
# ====================
dem_cog_url <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/_Other/Projects/2023_Lidar/PreliminaryData/Statewide_2023_35cm_DEMHF.tif")
satband_cog_url_21_21 <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/Imagery/STATEWIDE_2021-2022_30cm_LeafOFF_4Band.tif")
lc_cog_url <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/STATEWIDE_2022_50cm_LANDCOVER_TreeCanopy.tif")
lc_imperv_url <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/STATEWIDE_2022_50cm_LANDCOVER_Impervious.tif")

out_dir <- "~/R/VPAtlas_LiDAR/FranklinCo" #

wbt_init() # must initiate whitebox each session

for (i in unique(aoi$BLOCKNAME)) {
  start_time <- Sys.time()

  cli_alert("Analysis begun: {i} @ {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")

    out_file <- paste0(out_dir,"/combined_sf_", i, ".geojson") # out_file <- paste0("~/R/VPAtlas_LiDAR/FranklinCo/combined_sf_", i, ".geojson")

            if (file.exists(out_file)) { # skip town if it already exits in directory
              cli_alert_info("Skipping block: {i} (file already exists)")
              next }

  tryCatch({ # reduce the chance of the whole loop stopping should one town fail

    townbound <- aoi[aoi$BLOCKNAME == i, ]

# ====================
# Data prep (crop -> project)
# ====================

    # Crop and download rasters in loop or else the statewide layer will be downloaded
    dem_cog <- crop(dem_cog_url, townbound) %>% project("EPSG:32145", method = "bilinear")
    satband_cog_21_22 <- crop(satband_cog_url_21_21, townbound) %>% project("EPSG:32145", method = "bilinear")
        names(satband_cog_21_22) <- c("Red_Band", "Green_Band", "Blue_Band", "NIR_Band") # name imagery color bands
    lc_cog <- crop(lc_cog_url, townbound) %>% project("EPSG:32145", method = "bilinear")
    lc_imperv_cog <- crop(lc_imperv_url, townbound) %>% project("EPSG:32145", method = "bilinear")
    vt_hydro_poly_int <- st_intersection(vt_hydro_poly, townbound) # the vectors were downloaded and reprojected already
    vt_hydro_lines_int <- st_intersection(vt_hydro_lines, townbound) # here we're just clipping to the town boundaries
    vt_wetlands_int <- st_intersection(vt_wetlands, townbound)
    vt_buildings_int <- st_intersection(vt_buildings, townbound)

# ====================
# DEM depression analysis
# ====================
    tmp_smooth  <- tempfile(fileext = ".tif") # create temp files; wbt functions must run off file path
    tmp_output  <- tempfile(fileext = ".tif")

    dem3 <- focal(dem_cog, matrix(1, nrow = 3, ncol = 3), fun = median, na.rm = T) # smooth DEM, from Wu et al. 2014
    writeRaster(dem3, tmp_smooth, progress = T, overwrite = T) # write smoothed DEM to temp file

    wbt_stochastic_depression_analysis(
      dem = tmp_smooth, # load smoothed DEM from temp file
      output = tmp_output, # save depression raster to temp file
      rmse = 0.095, # from DEM metadata
      range = 1.05, # 3*DEM resolution
      iterations = 50, # based on Wu et al. 2014
      wd = NULL,
      verbose_mode = F)

    depressions <- rast(tmp_output) %>% # load depression raster to temp file
      mask(vt_hydro_poly_int, inverse = T) %>%
      mask(vt_wetlands_int, inverse = T) %>%
      mask(vt_hydro_lines_int, inverse = T) %>%
      mask(vt_buildings_int, inverse = T) %>%
      mask(resample(lc_cog, rast(tmp_output), method = "near"), inverse = F) %>%
      mask(resample(lc_imperv_cog, rast(tmp_output), method = "near"), inverse = T) %>%
      ifel(. < 0.8, NA, .) %>% # remove depressions with < 80% chance of being a depression
      focal(matrix(1, nrow = 5, ncol = 5), fun = median, na.rm = T) %>%
      as.polygons() %>%  # turn into vector polygons
      st_as_sf() %>% st_cast("POLYGON") %>%
      mutate(area = as.numeric(st_area(.)))

# ====================
# NDWI analysis
# ====================
    ndwi_raster <- (satband_cog_21_22$Green_Band - satband_cog_21_22$NIR_Band) / (satband_cog_21_22$Green_Band + satband_cog_21_22$NIR_Band) # calculate NDWI

    standing_water <- ndwi_raster %>%
      mask(vt_hydro_poly_int, inverse = T) %>%
      mask(vt_wetlands_int, inverse = T) %>%
      mask(vt_hydro_lines_int, inverse = T) %>%
      mask(vt_buildings_int, inverse = T) %>%
      mask(resample(lc_cog, ndwi_raster, method = "near"), inverse = F) %>%
      mask(resample(lc_imperv_cog, ndwi_raster, method = "near"), inverse = T) %>%
      ifel(. < 0, NA, .) %>%# Remove negative values indicating no standing water
      focal(matrix(1, nrow = 5, ncol = 5), fun = max, na.rm = T) %>% #use max function
      as.polygons() %>%  # turn into vector polygons
      st_as_sf() %>% st_cast("POLYGON") %>%
      mutate(area = as.numeric(st_area(.)))


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

    water_only <- st_join(water_filtered, dep_filtered, join = st_intersects) %>% filter(is.na(dep_id))

    combined_sf <- bind_rows(dep_side, water_only) %>%
      mutate(status = case_when(!is.na(dep_id) & !is.na(water_id) ~ "Water_Depression",
                                !is.na(dep_id) & is.na(water_id)  ~ "Depression",
                                is.na(dep_id)  & !is.na(water_id) ~ "Water"),
             area_m = coalesce(area_dep, area_water),
             pred_status = as.factor(status)) %>%
      dplyr::select(area_m, pred_status, area = area_m, geometry)

    st_write(combined_sf, out_file, append = F, overwrite = T)

    end_time <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    elapsed <- difftime(end_time, start_time, units = "mins")

    # message("Finished block: ", i, " | Time elapsed: ", round(elapsed, 2), " minutes @", end_time)
    cli_alert_success("Finished block: {i} | Time elapsed: {round(elapsed, 2)} minutes @ {end_time}")
    beep(sound = 1, expr = NULL)

          }, error = function(e) {
                # message("**FAILED** block: ", i, " | ", e$message, "@ ", Sys.time())
            cli_alert_danger("***** FAILED ***** block: {i} | {e$message} @ {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")
            })
}




# ====================
# Bind geojson outputs, compile into single shapefile
# ====================
path <- "~/R/VPAtlas_LiDAR/FranklinCo"

files <- list.files(path, pattern = "\\.geojson$", full.names = TRUE)

vpatlas_combined <- map_dfr(files, ~st_read(.x, quiet = TRUE))

st_write(vp_lidar_combined, paste0(path,"vp_lidar_combined.shp"))



