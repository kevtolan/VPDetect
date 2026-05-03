library(tidyverse) # data manipulation, workflow
library(sf) # vector data
library(terra) # raster data
library(whitebox) # follow install directions here https://www.whiteboxgeo.com/manual/wbt_book/r_interface.html
library(arcpullr) # download ESRI-hosted data
library(cli) # color outputs
library(beepr) # beep when error
library(arcgislayers)
library(arcgis)
library(mapview)
library(FedData)

terraOptions(progress = 0) # prevent progress bars during raster functions

# target_counties <- c("Addison County", "Rutland County","Bennington County") # in order that you want to run

# ====================
# Define area of interest
# ====================
nh_bound <- get_spatial_layer("https://nhgeodata.unh.edu/hosting/rest/services/Hosted/GV_BaseLayers/FeatureServer/6") %>%
  st_transform(crs = 26919) %>%
  st_union() %>%
  st_polygonize() %>%
  st_as_sf()

nh_blocks <- get_spatial_layer("https://nhgeodata.unh.edu/hosting/rest/services/Hosted/GV_BaseLayers/FeatureServer/6") %>%
  st_transform(crs = 26919) %>%
  st_make_grid(cellsize = 2000) %>%
  st_sf() %>%
  mutate(y_coord = st_coordinates(st_centroid(geometry))[,2]) %>%
  mutate(EIGHTH = ntile(y_coord, 8)) %>%
  mutate(BLOCKNAME = paste0("NH_E", EIGHTH, "_", row_number())) %>%
  select(-y_coord)


nh_blocks <- st_filter(nh_blocks,nh_bound)

# run all blocks
aoi <- nh_blocks[nh_blocks$EIGHTH %in% c(1,2),]
files <- list.files("~/R/VPAtlas_LiDAR/NH_RanBlocks", pattern = "\\.geojson$")
existing_blocks <- gsub("combined_sf_|\\.geojson", "", files)
aoi <- aoi[order(!(aoi$BLOCKNAME %in% existing_blocks)), ] # sort existing files to top



# ====================
# Load vectors
# These are ESRI-hosted vector layers
# ====================

nh_hydro_poly <- get_spatial_layer("https://nhgeodata.unh.edu/hosting/rest/services/Hosted/IWR_WaterResources/FeatureServer/9/",
                                   out_fields = c("permanent_identifier")) %>% st_transform(crs = 26919) %>% st_make_valid()#%>% st_buffer(10) #hydrology polygons

nh_hydro_lines <- get_spatial_layer("https://nhgeodata.unh.edu/hosting/rest/services/Hosted/IWR_WaterResources/FeatureServer/29/",
                                    out_fields = c("permanent_identifier")) %>% st_transform(crs = 26919) %>% st_make_valid() #%>% st_buffer(10) # hydrology lines

nh_wetlands <- get_spatial_layer("https://nhgeodata.unh.edu/hosting/rest/services/Hosted/IWR_WaterResources/FeatureServer/28/",
                                   out_fields = c("permanent_identifier")) %>% st_transform(crs = 26919) %>% st_make_valid() #%>% st_buffer(10) # wetlands


nh_hydro_poly <- nh_hydro_poly %>% st_transform(st_crs(aoi)) %>% st_intersection(st_union(aoi)) %>% st_buffer(10)
nh_hydro_lines <- nh_hydro_lines %>% st_transform(st_crs(aoi)) %>% st_intersection(st_union(aoi)) %>% st_buffer(10)
nh_wetlands <- nh_wetlands %>% st_transform(st_crs(aoi)) %>% st_intersection(st_union(aoi)) %>% st_buffer(10)

gc()
# ====================
# Load cloud-hosted geotiffs https://vcgi.vermont.gov/resources/how-and-education-resources/how-use-cloud-optimized-geotiffs-cogs
# This isn't downloading the rasters, just creating a link to the raster download.
# During the loop, each raster will be clipped to a town boundary, then downloaded to a temp object
# If a computer can't handle these larger chunks of land, use a grid system
# ====================
dem_url <- arc_open('https://granit24a.sr.unh.edu/image/rest/services/ImageServices/LiDAR_Bare_Earth_DEM_NH_2022/ImageServer')
satband_cog_url_21_21 <- arc_open('https://granit24a.sr.unh.edu/image/rest/services/ImageServices/NH_2021_2022_6in_RGB/ImageServer')


out_dir <- "~/R/VPAtlas_LiDAR/NH_RanBlocks" #

wbt_init() # must initiate whitebox each session
wbt_verbose(F) # stop whitebox from printing

blocks <- unique(aoi$BLOCKNAME)
total_blocks <- length(blocks)
script_start_time <- Sys.time() # Master timer
processed_count <- 0  # Track actual file ran
skipped_count <- 0    # Track skipped files that already exist
tmp_smooth  <- tempfile(fileext = ".tif") # Create temp files; whitebox functions must run off file path
tmp_output  <- tempfile(fileext = ".tif") # You can also use actual file paths to save if included in the loop
set.seed(67)

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
    bounds <- st_bbox(townbound)
    bounds_proj <- st_transform(townbound, 26919) %>% st_bbox()
    x_range <- bounds_proj["xmax"] - bounds_proj["xmin"]
    y_range <- bounds_proj["ymax"] - bounds_proj["ymin"]

    bounds_3445 <- st_transform(townbound, 3445) %>% st_bbox()
    x_range_ft <- bounds_3445["xmax"] - bounds_3445["xmin"]
    y_range_ft <- bounds_3445["ymax"] - bounds_3445["ymin"]

    native_res <- 0.76

    width_px  <- round(x_range / native_res)
    height_px <- round(y_range / native_res)


 # ====================
# Data prep (cdownload -> crop -> project)
# ====================

    dem_cog <- arc_raster(
      dem_url,
      xmin = bounds["xmin"],
      ymin = bounds["ymin"],
      xmax = bounds["xmax"],
      ymax = bounds["ymax"],
      bbox_crs = st_crs(townbound),
      width  = width_px,
      height = height_px,
      format = "tiff"
    ) %>% project("EPSG:26919", method = "bilinear")

    native_res <- 2  # feet

    width_px  <- round(x_range_ft / native_res)
    height_px <- round(y_range_ft / native_res)


    satband_cog_21_22 <- arc_raster(
      satband_cog_url_21_21,
      xmin = bounds["xmin"],
      ymin = bounds["ymin"],
      xmax = bounds["xmax"],
      ymax = bounds["ymax"],
      bbox_crs = st_crs(townbound),
      width  = width_px,
      height = height_px,
      format = "tiff") %>%
      project("EPSG:26919", method = "bilinear")

    names(satband_cog_21_22) <- c("Red_Band", "Green_Band", "Blue_Band", "NIR_Band") # name imagery color bands


# nlcd_raw <- get_nlcd(
#   template       = townbound,
#   label          = paste0("nlcd_", i),
#   year           = 2019,          # try 2019 or 2016
#   dataset        = "landcover",
#   extraction.dir = file.path(out_dir, "nlcd_raw"),
#   force.redo     = FALSE
# )
# nh_forest <- get_nlcd(
#       template = townbound,
#       label = meve,
#       year = 2021,
#       dataset = "landcover",
#       landmass = "L48")

    # lc_cog <- crop(lc_cog_url, townbound) %>% project("EPSG:26919", method = "bilinear")
    # lc_imperv_cog <- crop(lc_imperv_url, townbound) %>% project("EPSG:26919", method = "bilinear")
    nh_hydro_poly_int <- st_intersection(nh_hydro_poly, townbound) # the vectors were downloaded and reprojected already
    nh_hydro_lines_int <- st_intersection(nh_hydro_lines, townbound) # here we're just clipping to the town boundaries
    nh_wetlands_int <- st_intersection(nh_wetlands, townbound)

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
      mask(nh_hydro_poly_int, inverse = T) %>%
       mask(nh_wetlands_int, inverse = T) %>%
       mask(nh_hydro_lines_int, inverse = T) %>%
       # mask(resample(lc_cog, rast(tmp_output), method = "near"), inverse = F) %>%
       # mask(resample(lc_imperv_cog, rast(tmp_output), method = "near"), inverse = T) %>%
      ifel(. < 0.8, NA, .) %>% # remove depressions with < 80% chance of being a depression
      focal(matrix(1, nrow = 5, ncol = 5), fun = median, na.rm = T) %>%
      as.polygons() %>%  # turn into vector polygons
      st_as_sf() %>% st_cast("POLYGON") %>%
      mutate(area = as.numeric(st_area(.)))


    ndwi_raster <- (satband_cog_21_22$Green_Band - satband_cog_21_22$NIR_Band) / (satband_cog_21_22$Green_Band + satband_cog_21_22$NIR_Band) # calculate NDWI

    standing_water <- ndwi_raster %>%
      mask(nh_hydro_poly_int, inverse = T) %>%
      mask(nh_wetlands_int, inverse = T) %>%
      mask(nh_hydro_lines_int, inverse = T) %>%
      # mask(resample(lc_cog, ndwi_raster, method = "near"), inverse = F) %>%
      # mask(resample(lc_imperv_cog, ndwi_raster, method = "near"), inverse = T) %>%
      ifel(. < 0, NA, .) %>% # Remove negative values indicating no standing water
      focal(matrix(1, nrow = 5, ncol = 5), fun = max, na.rm = T) %>% # use max function
      as.polygons() %>%  # turn into vector polygons
      st_as_sf() %>% st_cast("POLYGON") %>%
      mutate(area = as.numeric(st_area(.)))

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
      paste(round(eta_mins, 1), "mins")
    }

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

    rm(ndwi_raster, nh_hydro_poly_int, nh_hydro_lines_int,
       nh_wetlands_int, dep_filtered, combined_sf,
       water_filtered, dep_join, water_only, townbound,
       dem_cog, dem3, satband_cog_21_22, depressions, standing_water)

    gc(full = TRUE) # clear removed object memory
    unlink(c(tmp_smooth, tmp_output))  # ensure temp files from previous iteration are removed to not accumulate memory usage
    tmpFiles(remove = TRUE)

          }, error = function(e) {
            cli_alert_danger("***** FAILED ***** block: {.val {i}}  @ {.val {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}} {e$message}")
            beep(sound = 1, expr = NULL) }) # beep after error; optional
}


# ====================z
# Inport geojsons, export one combined shapefile
# ====================
library(tidyverse)
library(sf)
path <- "~/R/VPAtlas_LiDAR/NH_RanBlocks"

geojsonfiles <- list.files(path, pattern = "\\.geojson$", full.names = TRUE)

vp_lidar_combined <- map_dfr(geojsonfiles, ~st_read(.x, quiet = TRUE)) %>%
                        st_simplify(preserveTopology = T, dTolerance = 0.5)



st_write(vp_lidar_combined, paste0(path,"nh_vp_lidar_combined.shp"), append=FALSE) # now load into QGIS or ArcGIS to assess polygons

# vp_lidar_combined_simp <- st_simplify(vp_lidar_combined, dTolerance = 0.25)

# st_write(vp_lidar_combined_simp, paste0(path,"vp_lidar_combined_simp.shp"), append=FALSE) # now load into QGIS or ArcGIS to assess polygons

