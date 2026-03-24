library(tidyverse) # data manipulation, workflow
library(sf) # vector data
library(terra) # raster data
library(arcpullr) # download ESRI-hosted data
library(ggspatial)
library(cowplot)
library(patchwork)
library(ggpubr)
library(grid)
library(mapview)
library(gridExtra)
library(units)
library(elevatr)





proland <- st_read('~/R/R_Spatial/spat/FS_VCGI_OPENDATA_Cadastral_PROTECTEDLND_poly_SP_v2_4663873866724723700.geojson') %>%
  st_transform(crs = 32145) #%>%
  # filter(!(PTYPE1 %in% c("EASEMENT", "DEED", "DZONE")))

mapview(proland[proland$NAME == 'Billings Park',])

public_land <- proland %>%
  filter(
    str_detect(NAME, "WMA|Wma|State Park|Town Forest|National Forest|Wildlife Management Area|Town Park|State Forest") |
      PAGENCY1 %in% c('5001936325', '5000170075', '5002346000', '2000032000',
                      '5000357025', '3000045100', '5002785975', '4000052010'))
mapview(public_land)

depressions <- st_read('/Users/kevintolan/R/VPAtlas_LiDAR/RanBlocksvp_lidar_combined.shp') %>%
  st_make_valid()

bioph <- st_read('~/R/AMMonitor_VPMon/VPMon_AMM/spatials/Biophysical_Regions.shp')

towns <- st_read('/Users/kevintolan/R/R_Spatial/spat/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1.shp') %>%
  st_transform(crs = 32145)




monitoredpools <- st_read('/Users/kevintolan/R/VPAtlas_LiDAR/vp_survey.geojson') %>%
  st_transform(crs = 32145) %>%
  select(poolId) %>%
  group_by(poolId) %>%
  slice_head(n = 1) #%>%
  # get_elev_point()
# mapview(monitoredpools, zcol = "elevation")
# hist(monitoredpools$elevation)

mappedpools <- st_read('/Users/kevintolan/R/VPAtlas_LiDAR/vp_mapped.geojson') %>%
  st_transform(crs = 32145) %>%
  add_count(poolStatus) %>%
  mutate(
    poolStatus = paste0(poolStatus, " (n = ", format(n, big.mark = ",", trim = TRUE), ")"),
    `Pool Status` = factor(poolStatus)
  ) %>%
  mutate(`Pool Status` = forcats::fct_reorder(`Pool Status`, n, .desc = TRUE)) %>%
  select(-n)

conf_lab <- unique(mappedpools$poolStatus[grepl("Confirmed", mappedpools$poolStatus)])
prob_lab <- unique(mappedpools$poolStatus[grepl("Probable", mappedpools$poolStatus)])
pote_lab <- unique(mappedpools$poolStatus[grepl("Potential", mappedpools$poolStatus)])

mappedpools$`Pool Status` <- factor(mappedpools$poolStatus,
                                    levels = c(conf_lab, prob_lab, pote_lab))

interp_grid <- st_read('/Users/kevintolan/R/VPAtlas_LiDAR/LiDAR_Grid.shp') %>%
  st_transform(crs = 32145) %>%
  mutate(Checked = case_when(
    Checked == "Yes" ~ "Yes",
    Checked == "Yes2" ~ "Yes",
    TRUE ~ "No"),
    Checked = factor(Checked,
                           levels = c("Yes",
                                      "No")))


checkedgrid <- interp_grid[interp_grid$Checked == "Yes",]

total_area <- sum(st_area(interp_grid))
total_area_checked <- sum(st_area(checkedgrid))
total_area_checked/total_area
area_sq_miles_checked <- set_units(total_area, mi^2)

newpools <- st_read('/Users/kevintolan/R/VPAtlas_LiDAR/2026_LiDAR_New_VPs.shp') %>%
  st_transform(crs = 32145) %>%
  group_by(geometry) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(poolId = paste0("LDR",row_number()))



grid_counts <- interp_grid %>%
  filter(Checked == 'Yes') %>%
  st_transform(crs = 32145) %>%
  st_join(newpools) %>%
  group_by(geometry) %>%
  summarise(
    pool_count = sum(!is.na(poolId)),
    Checked = first(Checked)) %>%
  ungroup()

newpoolsexport <- newpools %>%
                      st_join(depressions, left = TRUE) %>%
                      st_transform(crs = 4326) %>%
                      mutate(MapMethod = case_when(
                        prd_stt == "Water_Depression" ~ "LiDAR_NDWI",
                        prd_stt == "Water" ~ "NDWI",
                        prd_stt == "Depression"  ~ "LiDAR",
                        TRUE ~ "CIR")) %>%
                        dplyr::select(poolId,MapMethod) %>%
                      group_by(poolId) %>%
                      slice_head(n = 1) %>% ungroup()
nrow(newpoolsexport)
table(newpoolsexport$MapMethod)

# st_write(newpoolsexport,"New_VPs_March17_2026.geojson")

mappedpools_sel <- mappedpools %>% mutate(long = st_coordinates(.)[,1],
                                         lat = st_coordinates(.)[,2]) %>%
                                  dplyr::select(poolId,long,lat,poolStatus) %>%
  mutate(poolStatus = case_when(
    poolStatus == "Confirmed (n = 965)" ~ "Confirmed",
    poolStatus == "Potential (n = 3,306)" ~ "Potential",
    poolStatus == "Probable (n = 1,074)"  ~ "Probable",
    TRUE ~ as.character(poolStatus)))


pool_town_joined <- newpools %>% mutate(long = st_coordinates(.)[,1],
                                         lat = st_coordinates(.)[,2],
                                     poolStatus = "New_potential") %>%
                                  dplyr::select(poolId,long,lat,poolStatus) %>%
                                  bind_rows(mappedpools_sel) %>%
                                  st_intersection(towns)


# pool_town_joined84 <- pool_town_joined %>% st_transform(4326) %>% get_elev_point()
# saveRDS(pool_town_joined84, "pool_town_joined84.rds")


# mappedpools_sel84 <- pool_town_joined %>% st_transform(4326) %>% select(c(poolId,poolStatus)) %>%
  # rename(name = poolId,
         # desc = poolStatus)#,
         # ele = elevation)


# st_write(mappedpools_sel84, dsn = "vernalpools_Mar14.gpx", driver = "GPX",
         # dataset_options = "GPX_USE_EXTENSIONS=YES", delete_dsn = TRUE)







hist(pool_town_joined84$elevation)

mapview(pool_town_joined84, zcol = "elevation") + mapview(proland, alpha = 0.5)

mapview(pool_town_joined84 %>% filter(elevation > 650,
                                    elevation < 1000), zcol = "elevation") +
  mapview(proland, alpha = 0.5)


pro_visits_wmas <- st_intersection(proland,pool_town_joined) %>%
  filter(str_detect(NAME, "WMA"))
  # filter(poolStatus != "Confirmed")
mapview(pro_visits_wmas[pro_visits_wmas$elevation >= 700,], zcol = "elevation")
mapview(pro_visits_wmas, zcol = "elevation")

mapview(pro_visits_wmas)


# pool_town_joined_hart <- pool_town_joined[pool_town_joined$TOWNNAMEMC == "Hartland",]
# pool_town_joined_hart <- pool_town_joined[pool_town_joined$TOWNNAMEMC %in% c('Weybridge','Middlebury',
#                                                                              'Cornwall','New Haven',
#                                                                              'Addison','Ripton',
#                                                                              'Whiting','Salisbury'),]
pool_town_joined_hart <- pool_town_joined[pool_town_joined$TOWNNAMEMC == 'Woodstock',]

mapview(pool_town_joined_hart)
# write.csv(pool_town_joined_hart,"All_Pools_Middlebury.csv")
table(pool_town_joined_hart$poolStatus)



pro_visits <- st_intersection(proland,pool_town_joined) %>%
                      filter(poolStatus != "Confirmed")

mapview(pro_visits)

# write.csv(pro_visits,"Protected_lands_Pools_Middlebury.csv")
#


depressions_int <- depressions %>% st_join(pool_town_joined, left = FALSE) %>%
                                  st_transform(crs = 4326) %>%
                                  mutate(DetectionMethod = case_when(
                                    prd_stt == "Water_Depression" ~ "LiDAR_NDWI",
                                    prd_stt == "Water" ~ "NDWI",
                                    prd_stt == "Depression"  ~ "LiDAR",
                                    TRUE ~ NA)) %>%
                                    dplyr::select(!(c(area,lat,long,prd_stt))) %>%
                                  group_by(poolId) %>%
                                  slice_head(n = 1) %>% ungroup()
table(depressions_int$DetectionMethod)




mappedpoolsdetect <- mappedpools_sel %>% st_join(depressions)
mapview(mappedpoolsdetect[is.na(mappedpoolsdetect$prd_stt), ])
# mapview(depressions_int, zcol = "DetectionMethod") + mapview(all_pools_sel)
# mapview(depressions_int[depressions_int$poolId == "LDR1836",])
# st_write(depressions_int,"LiDAR_Polygons_March17_2026.geojson")

## maps

vt_water <- st_read("/Users/kevintolan/R/EAME_Report_Scripts/FS_VCGI_OPENDATA_Water_VHDCARTO_poly_SP_v1_-4286233864636686690.geojson") %>%
  st_transform(crs = 32145) %>%
  st_simplify(dTolerance = 1)


p1 <- ggplot() +
  geom_sf(data = towns, fill = "white", color = 'gray', linewidth = .5) +
  geom_sf(data = vt_water %>% filter(FTYPE == "LakePond",
                                     AREASQKM >= 2), fill = 'lightblue', color = "black", linewidth = .25) +
  geom_sf(data = newpools, aes(fill = "New = 8,116"), size = 2,
          linewidth = 0.1, color = "#fbaca7", alpha = 0.9, shape = 21) +
  scale_fill_manual(
    name = "Pool Status",
    values = c("New = 8,116" = "black")) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +
  geom_sf(data = bioph, fill = NA, color = "black", linewidth = .5) +
  labs(x = "Longitude", y = 'Latitude') +
  annotation_scale(aes(unit_category = "imperial",
                       text_col = 'black',
                       line_col = 'black',
                       width_hint = .3), height = unit(0.7, "cm"),
                   text_cex = 1.5,
                   pad_x = unit(3.4, "in"), pad_y = unit(.6, "in")) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "in"), width = unit(1, "in"),
    pad_x = unit(3.8, "in"), pad_y = unit(1.1, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book",
      text_size = 25)) +
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.text = element_text(size=12),
        legend.title=element_text(size=15, face = "bold"),
        legend.position = c(.8, .39),
        legend.background = element_rect(fill="white",
                                         size=.5, linetype="solid",
                                         colour ="black"),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        # axis.text.y=element_blank(),
        panel.background = element_rect(fill = '#F6F6F6'))



p2 <- ggplot() +
  geom_sf(data = towns, fill = "white", color = 'gray', linewidth = .5) +
  geom_sf(data = vt_water %>% filter(FTYPE == "LakePond",
                                     AREASQKM >= 2), fill = 'lightblue', color = "black", linewidth = .25) +
  geom_sf(data = mappedpools, aes(fill = `Pool Status`), size = 2,
          linewidth = 0.25, color = "black", alpha = 0.9, shape = 21) +
  scale_fill_manual(values = c("#011480", "#02fdff", "#ffa501")) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +
  geom_sf(data = bioph, fill = NA, color = "black", linewidth = .5) +
  labs(x = "Longitude", y = 'Latitude') +
  annotation_scale(aes(unit_category = "imperial",
                       text_col = 'black',
                       line_col = 'black',
                       width_hint = .3), height = unit(0.7, "cm"),
                   text_cex = 1.5,
                   pad_x = unit(3.4, "in"), pad_y = unit(.6, "in")) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "in"), width = unit(1, "in"),
    pad_x = unit(3.8, "in"), pad_y = unit(1.1, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book",
      text_size = 25)) +
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.text = element_text(size=12),
        legend.title=element_text(size=15, face = "bold"),
        legend.position = c(.8, .34),
        legend.background = element_rect(fill="white",
                                         size=.5, linetype="solid",
                                         colour ="black"),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        # axis.text.y=element_blank(),
        panel.background = element_rect(fill = '#F6F6F6'))




p3 <- ggplot() +
  geom_sf(data = towns, fill = "white", color = 'gray', linewidth = .5) +
  # Map alpha to the condition (TRUE if 0, FALSE if >0)
  geom_sf(data = grid_counts,
          aes(fill = pool_count, alpha = pool_count == 0),
          color = "black", linewidth = 0.0) +
  geom_sf(data = vt_water %>% filter(FTYPE == "LakePond", AREASQKM >= 2),
          fill = 'lightblue', color = "black", linewidth = .25) +
  scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 1), guide = "none") +
  scale_fill_viridis_c(option = "turbo",
                       direction = 1,
                       name = expression(atop("New Pools", paste("per 6.25 ", km^2)))) +
  geom_sf(data = bioph, fill = NA, color = "black", linewidth = .5) +
  labs(x = "Longitude", y = 'Latitude') +
  annotation_scale(aes(unit_category = "imperial",
                       text_col = 'black',
                       line_col = 'black',
                       width_hint = .3), height = unit(0.7, "cm"),
                   text_cex = 1.5,
                   pad_x = unit(3.4, "in"), pad_y = unit(.6, "in")) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "in"), width = unit(1, "in"),
    pad_x = unit(3.8, "in"), pad_y = unit(1.1, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "grey20",
      # text_family = "ArcherPro Book", # Ensure this font is loaded or comment out
      text_size = 25)) +
  guides(fill = guide_colorbar(barwidth = 2, barheight = 10)) +
  theme(
    legend.key.size = unit(1, 'cm'),
    legend.key.height= unit(1, 'cm'),
    legend.key.width= unit(1, 'cm'),
    legend.text = element_text(size=12),
    legend.title=element_text(size=15, face = "bold"),
    legend.position = c(.81, .39),
    legend.background = element_rect(fill="white",
                                     linewidth=.5, linetype="solid", # 'size' is deprecated for rect
                                     colour ="black"),
    axis.text=element_blank(),
    axis.title=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = '#F6F6F6'))





plot <- plot_grid(
  p3, p1, p2,
  ncol = 3,  align = "hv",
  labels="AUTO", label_size = 40, label_y = 1)

plot





