library(tidyverse) 
library(sf) 
library(terra)
library(ggspatial)
library(cowplot)
library(patchwork)
library(ggpubr)
library(grid)
library(mapview)
library(gridExtra)
library(units)
library(elevatr)


depressions <- st_read('~/R/VPAtlas_LiDAR/RanBlocksvp_lidar_combined.shp') %>% st_make_valid() 

bioph <- st_read('~/R/AMMonitor_VPMon/VPMon_AMM/spatials/Biophysical_Regions.shp')

towns <- st_read('~/R/R_Spatial/spat/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1.shp') %>% st_transform(crs = 32145)

vt_water <- st_read("~/R/EAME_Report_Scripts/FS_VCGI_OPENDATA_Water_VHDCARTO_poly_SP_v1_-4286233864636686690.geojson") %>%
  st_transform(crs = 32145) %>%
  st_simplify(dTolerance = 1)


# new pools
newpools <- st_read('~/R/VPAtlas_LiDAR/2026_LiDAR_New_VPs.shp') %>%
  st_transform(crs = 32145) %>%
  group_by(geometry) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(poolId = paste0("LDR",row_number()))

# existing VPAtlas mapped pools
mappedpools <- st_read('~/R/VPAtlas_LiDAR/vp_mapped.geojson') %>%
  st_transform(crs = 32145) %>%
  add_count(poolStatus) %>%
  mutate(
    poolStatus = paste0(poolStatus, " (n = ", format(n, big.mark = ",", trim = TRUE), ")"),
    `Pool Status` = factor(poolStatus)) %>%
  mutate(`Pool Status` = forcats::fct_reorder(`Pool Status`, n, .desc = TRUE)) %>%
  select(-n)

conf_lab <- unique(mappedpools$poolStatus[grepl("Confirmed", mappedpools$poolStatus)])
prob_lab <- unique(mappedpools$poolStatus[grepl("Probable", mappedpools$poolStatus)])
pote_lab <- unique(mappedpools$poolStatus[grepl("Potential", mappedpools$poolStatus)])

mappedpools$`Pool Status` <- factor(mappedpools$poolStatus,
                                    levels = c(conf_lab, prob_lab, pote_lab))

# map grid
interp_grid <- st_read('~/R/VPAtlas_LiDAR/LiDAR_Grid.shp') %>%
  st_transform(crs = 32145) %>%
  mutate(Checked = case_when(
    Checked == "Yes" ~ "Yes",
    Checked == "Yes2" ~ "Yes",
    TRUE ~ "No"),
    Checked = factor(Checked,
                           levels = c("Yes",
                                      "No")))
grid_counts <- interp_grid %>%
  filter(Checked == 'Yes') %>%
  st_transform(crs = 32145) %>%
  st_join(newpools) %>%
  group_by(geometry) %>%
  summarise(
    pool_count = sum(!is.na(poolId)),
    Checked = first(Checked)) %>%
  ungroup()

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
        legend.background = element_rect(fill="white", size=.5,
                                         linetype="solid", colour ="black"),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
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
                       text_col = 'black', line_col = 'black',
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
        panel.background = element_rect(fill = '#F6F6F6'))


p3 <- ggplot() +
  geom_sf(data = towns, fill = "white", color = 'gray', linewidth = .5) +
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
      text_size = 25)) +
  guides(fill = guide_colorbar(barwidth = 2, barheight = 10)) +
  theme(
    legend.key.size = unit(1, 'cm'),
    legend.key.height= unit(1, 'cm'),
    legend.key.width= unit(1, 'cm'),
    legend.text = element_text(size=12),
    legend.title=element_text(size=15, face = "bold"),
    legend.position = c(.81, .39),
    legend.background = element_rect(fill="white", linewidth=.5,
                                     linetype="solid", colour ="black"),
    axis.text=element_blank(),
    axis.title=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = '#F6F6F6'))


plot <- plot_grid(
  p3, p1, p2,
  ncol = 3,  align = "hv",
  labels="AUTO", label_size = 40, label_y = 1)

plot

