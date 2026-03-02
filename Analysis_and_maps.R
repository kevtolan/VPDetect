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

depressions <- st_read('~/R/VPAtlas_LiDAR/RanBlocksvp_lidar_combined.shp') %>%
  st_make_valid()

bioph <- st_read('~/R/AMMonitor_VPMon/VPMon_AMM/spatials/Biophysical_Regions.shp')

towns <- st_read('~/R/R_Spatial/spat/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1.shp') %>%
  st_transform(crs = 32145)

vt_water <- st_read("~/R/EAME_Report_Scripts/FS_VCGI_OPENDATA_Water_VHDCARTO_poly_SP_v1_-4286233864636686690.geojson") %>%
  st_transform(crs = 32145) %>%
  st_simplify(dTolerance = 1)

mappedpools <- st_read('~/R/VPAtlas_LiDAR/vp_mapped.geojson') %>%
  st_transform(crs = 32145) %>%
  mutate(poolStatus = case_when(
    poolStatus == "Confirmed" ~ "Confirmed (n = 965)",
    poolStatus == "Potential" ~ "Potential (n = 3,306)",
    poolStatus == "Probable"  ~ "Probable (n = 1,074)",
    TRUE ~ as.character(poolStatus)),
    `Pool Status` = factor(poolStatus,
                                levels = c("Confirmed (n = 965)",
                                           "Probable (n = 1,074)",
                                           "Potential (n = 3,306)")))

interp_grid <- st_read('~/R/VPAtlas_LiDAR/LiDAR_Grid.shp') %>%
  st_transform(crs = 32145) %>%
  mutate(Checked = case_when(
    Checked == "Yes" ~ "Yes",
    Checked == "Yes2" ~ "Yes",
    # poolStatus == "Probable"  ~ "Probable (n = 1,074)",
    TRUE ~ "No"),
    Checked = factor(Checked,
                           levels = c("Yes",
                                      "No")))

# checkedgrid <- interp_grid[interp_grid$Checked == "Yes",]
# total_area <- sum(st_area(checkedgrid))
# area_sq_miles <- set_units(total_area, mi^2)

newpools <- st_read('/Users/kevintolan/R/VPAtlas_LiDAR/2026_LiDAR_New_VPs.shp') %>%
  st_transform(crs = 32145) %>%
  group_by(geometry) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(poolId = paste0("LDR",row_number()))

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

# st_write(newpoolsexport,"New_VPs_March2_2026.geojson")
mappedpools_sel <- mappedpools %>% mutate(long = st_coordinates(.)[,1],
                                         lat = st_coordinates(.)[,2]) %>%
                                  dplyr::select(poolId,long,lat)


all_pools_sel <- newpools %>% mutate(long = st_coordinates(.)[,1],
                                         lat = st_coordinates(.)[,2]) %>%
                                  dplyr::select(poolId,long,lat) %>%
                                  bind_rows(mappedpools_sel)


# allpools_int <- all_pools_sel %>% st_join(depressions, left = TRUE)

depressions_int <- depressions %>% st_join(all_pools_sel, left = FALSE) %>%
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

mapview(depressions_int, zcol = "DetectionMethod") + mapview(all_pools_sel)
mapview(depressions_int[depressions_int$poolId == "LDR1836",])
# st_write(depressions_int,"LiDAR_Polygons_March2_2026.geojson")

## maps



p1 <- ggplot() +
  geom_sf(data = towns, fill = "white", color = 'gray', linewidth = .5) +
  geom_sf(data = vt_water %>% filter(FTYPE == "LakePond",
                                     AREASQKM >= 2), fill = 'lightblue', color = "black", linewidth = .25) +
  geom_sf(data = newpools, aes(fill = "New = 7,272"), size = 2,
          linewidth = 0.1, color = "#fbaca7", alpha = 0.9, shape = 21) +
  scale_fill_manual(
    name = "Pool Status",
    values = c("New = 7,272" = "black")) +
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
  geom_sf(data = interp_grid, aes(fill = Checked), color = "black", linewidth = 0.05) +
  geom_sf(data = vt_water %>% filter(FTYPE == "LakePond", AREASQKM >= 2),
          fill = 'lightblue', color = "black", linewidth = .25) +
  scale_fill_manual(
    name = "Interpreted?",
    values = c("Yes" = "cyan",
               "No" = "transparent")) +
  guides(fill = guide_legend(override.aes = list(alpha = .8)),
         color = guide_legend(override.aes = list(size = 6))) +
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
  theme(
        legend.key.size = unit(1, 'cm'),
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
        # axis.text.y=element_text(color = "black",size=15),
        panel.background = element_rect(fill = '#F6F6F6'))






plot <- plot_grid(
  p3, p1, p2,
  ncol = 3,  align = "hv",
  labels="AUTO", label_size = 40, label_y = 1)

plot





