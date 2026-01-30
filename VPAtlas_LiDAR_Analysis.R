library(tidyverse) # data manipulation, workflow
library(sf) # vector data
library(terra) # raster data
library(whitebox) # follow install directions here https://www.whiteboxgeo.com/manual/wbt_book/r_interface.html
library(arcpullr) # download ESRI-hosted data
library(cli) # color outputs
library(beepr) # beep when error
terraOptions(progress = 0) # prevent progress bars during raster functions

target_counties <- c("Franklin County", "Windsor County", "Orleans County") # in order that you want to run

# ====================
# Define area of interest
# ====================
vt_blocks <- get_spatial_layer("https://services1.arcgis.com/d3OaJoSAh2eh6OA9/ArcGIS/rest/services/Vermont_Wildlife_Atlasing_Blocks/FeatureServer/0",
                               out_fields = c("BLOCKNAME","QUADNAME","GEOUNITDES")) %>% st_transform(crs = 32145) # download grid
# sort by county
# aoi <- vt_blocks %>%
  # filter(GEOUNITDES %in% target_counties) %>%
  # mutate(GEOUNITDES = factor(GEOUNITDES, levels = target_counties)) %>%
  # arrange(GEOUNITDES)

# run all blocks
aoi <- vt_blocks
files <- list.files("~/R/VPAtlas_LiDAR/RanBlocks", pattern = "\\.geojson$")
existing_blocks <- gsub("combined_sf_|\\.geojson", "", files)
aoi <- aoi[order(!(aoi$BLOCKNAME %in% existing_blocks)), ] # sort existing files to top

vt_hydro_poly <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Water_VHDCARTO_poly_SP_v1/FeatureServer/0",
                                   out_fields = c("OBJECTID")) %>% st_transform(crs = 32145) %>% st_make_valid()#%>% st_buffer(10) #hydrology polygons

vt_buildings <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_STRUCTURES_POLY_SP_v1/FeatureServer/0/",
                                   out_fields = c("OBJECTID")) %>% st_transform(crs = 32145) %>% st_make_valid() #%>% st_buffer(5) # building footprints

vt_hydro_lines <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/arcgis/rest/services/FS_VCGI_OPENDATA_Water_VHDCARTO_line_SP_v1/FeatureServer/0",
                                    out_fields = c("OBJECTID")) %>% st_transform(crs = 32145) %>% st_make_valid() #%>% st_buffer(10) # hydrology lines

vt_wetlands <- get_spatial_layer("https://services5.arcgis.com/Uzks6LSde6r23wwG/arcgis/rest/services/Vermont_Significant_Wetland_Inventory/FeatureServer/0",
                                   out_fields = c("OBJECTID")) %>% st_transform(crs = 32145) %>% st_make_valid() #%>% st_buffer(10) # wetlands

vt_hydro_poly <- st_intersection(vt_hydro_poly, st_union(aoi)) %>% st_buffer(10)
vt_hydro_lines <- st_intersection(vt_hydro_lines, st_union(aoi)) %>% st_buffer(10)
vt_wetlands <- st_intersection(vt_wetlands, st_union(aoi)) %>% st_buffer(10)
vt_buildings <- st_intersection(vt_buildings, st_union(aoi)) %>% st_buffer(5)

gc()


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

out_dir <- "~/R/VPAtlas_LiDAR/RanBlocks" #

wbt_init() # must initiate whitebox each session
wbt_verbose(F) # stop whitebox from printing

blocks <- unique(aoi$BLOCKNAME)
total_blocks <- length(blocks)
script_start_time <- Sys.time() # Master timer
processed_count <- 0  # Track actual work done
skipped_count <- 0    # Track skipped files
tmp_smooth  <- tempfile(fileext = ".tif") # create temp files; whitebox functions must run off file path
tmp_output  <- tempfile(fileext = ".tif")
set.seed(67)

# for (i in unique(aoi$BLOCKNAME)) {
for (counter in seq_along(blocks)) {
  i <- blocks[counter]
  iteration_start_time <- Sys.time() # Timer for just this iteration

    out_file <- paste0(out_dir,"/combined_sf_", i, ".geojson")

            if (file.exists(out_file)) { # skip spatial object if its file already exits in directory
              cli_alert_info("Skipping block: {.val {i}} (file already exists)")
              skipped_count <- skipped_count + 1
              next }


  tryCatch({ # reduce the chance of the whole loop stopping should one town fail
    townbound <- aoi[aoi$BLOCKNAME == i, ]

# ====================
# Data prep (cdownload -> crop -> project)
# ====================
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
    dem3 <- focal(dem_cog, matrix(1, nrow = 3, ncol = 3), fun = median, na.rm = T) # smooth DEM, from Wu et al. 2014
    writeRaster(dem3, tmp_smooth, progress = T, overwrite = T) # write smoothed DEM to temp file

    wbt_stochastic_depression_analysis(
      dem = tmp_smooth, # load smoothed DEM from temp file
      output = tmp_output, # save depression raster to temp file
      rmse = 0.095, # from DEM metadata
      range = 1.05, # 3*DEM resolution
      iterations = 50, # based on Wu et al. 2014. Fewer iterations = faster, less memory use
      verbose_mode = F)

    depressions <- rast(tmp_output) %>% # load depression raster from temp file
      terra::deepcopy() %>%
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
      ifel(. < 0, NA, .) %>% # Remove negative values indicating no standing water
      focal(matrix(1, nrow = 5, ncol = 5), fun = max, na.rm = T) %>% # use max function
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

    dep_join <- st_join(dep_filtered, water_filtered, join = st_intersects)

    water_only <- st_join(water_filtered, dep_filtered, join = st_intersects) %>% filter(is.na(dep_id))

    combined_sf <- bind_rows(dep_join, water_only) %>%
      mutate(status = case_when(!is.na(dep_id) & !is.na(water_id) ~ "Water_Depression",
                                !is.na(dep_id) & is.na(water_id)  ~ "Depression",
                                is.na(dep_id)  & !is.na(water_id) ~ "Water"),
             area_m = coalesce(area_dep, area_water),
             pred_status = as.factor(status)) %>%
      dplyr::select(area_m, pred_status, area = area_m, geometry)

    st_write(combined_sf, out_file)

    processed_count <- processed_count + 1
    now <- Sys.time()

    elapsed_iteration <- difftime(now, iteration_start_time, units = "mins")
    elapsed_session <- difftime(now, script_start_time, units = "mins")
    avg_time_per_block <- elapsed_session / processed_count
    remaining_blocks <- total_blocks - counter
    eta_mins <- as.numeric(avg_time_per_block) * remaining_blocks

    eta_label <- if (eta_mins > 60) {
      paste(round(eta_mins / 60, 1), "hours")
    } else {
      paste(round(eta_mins, 1), "mins") }

    total_label <- if(elapsed_session > 60) {
         paste(round(as.numeric(elapsed_session)/60, 2), "hours")
      } else {
         paste(round(elapsed_session, 2), "mins") }

    cli_alert_success(
      "Finished block {.val {i}} {.val {counter}}/{.val {total_blocks}} @ {.val {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}} | \\
       This block took {.val {round(elapsed_iteration, 2)}} mins")

    cli_alert_success(
      "Total time: {.val {total_label}} | \\
       Time remaining: {.val {eta_label}}")

    rm(ndwi_raster, vt_hydro_poly_int, vt_hydro_lines_int,
       vt_wetlands_int, vt_buildings_int, dep_filtered, combined_sf,
       water_filtered, dep_join, water_only, lc_imperv_cog, townbound,
       dem_cog, dem3, satband_cog_21_22, lc_cog, depressions, standing_water)

    gc(full = TRUE) # clear removed object memory
    unlink(c(tmp_smooth, tmp_output))  # ensure temp files from previous iteration are removed to not accumulate memory usage
    tmpFiles(remove = TRUE)

          }, error = function(e) {
            cli_alert_danger("***** FAILED ***** block: {.val {i}}  @ {.val {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}} {e$message}")
            beep(sound = 1, expr = NULL) }) # beep after error; optional
}


# ====================
# Inport geojsons, export one combined shapefile
# ====================
library(tidyverse)
library(sf)
path <- "~/R/VPAtlas_LiDAR/RanBlocks"

geojsonfiles <- list.files(path, pattern = "\\.geojson$", full.names = TRUE)

vp_lidar_combined <- map_dfr(geojsonfiles, ~st_read(.x, quiet = TRUE))

st_write(vp_lidar_combined, paste0(path,"vp_lidar_combined.shp"), append=FALSE) # now load into QGIS or ArcGIS to assess polygons


