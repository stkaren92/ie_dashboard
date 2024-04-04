library(dplyr)
library(tidyverse)
library(ggplot2)
library(terra)
library(tidyterra)
library(leaflet)
library(sf)

anp_name <- "Arrecifes de Xcalak"

anp <- vect('data/anp/186ANP_ITRF08_19012023.shp')
ie <- rast('data/ie/ie_xgb_slic_2017.tif')

sort(anp$NOMBRE)
sort(unique(anp$REGION))

anp_selected <- subset(anp, anp$NOMBRE == anp_name)
anp_selected <- project(anp_selected, crs(ie))

periphery <- buffer(anp_selected, 25000)

ie_anp <- crop(ie, anp_selected, mask=TRUE)
ie_anp_periphery <- crop(ie, periphery, mask=TRUE)
ie_periphery <- mask(ie_anp_periphery,
                         anp_selected,
                         inverse = TRUE)

df_area <- data.frame(Location = c('ANP','Periferia'),
                      Area = c(expanse(ie_anp, unit="km")$area, 
                              expanse(ie_periphery, unit="km")$area))

ggplot() +
  geom_spatraster(data = ie_anp_periphery) +
  scale_fill_gradient2(low = "darkgreen",
                       mid = "beige",
                       high="red",
                       midpoint = 9.0) +
  geom_sf(data = anp_selected,
          inherit.aes = FALSE,
          color = "white", 
          fill = "transparent",
          linewidth = 1)


pal <- colorNumeric(c("darkgreen", "#FFFFCC", "red"), 
                    c(0,18),
                    na.color = "transparent")
anp_selected_sf <- sf::st_as_sf(anp_selected)
anp_selected_sf <- st_transform(anp_selected_sf, 
                                crs = '+proj=longlat +datum=WGS84')
leaflet() %>% addTiles() %>%
  addPolygons(data = anp_selected_sf, weight = 2,
              fillOpacity = 0.001, color = 'white') %>%
  addRasterImage(ie_anp_periphery, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = c(0,18),
            title = "IE")


######### bar chart ############
df_ie_anp <- as.data.frame(ie_anp, xy = TRUE)
df_ie_periphery <- as.data.frame(ie_periphery, xy = TRUE)

df_hist <- rbind(data.frame(ie = df_ie_anp$prediction, 
                            location = 'anp'),
                 data.frame(ie = df_ie_periphery$prediction, 
                            location = 'periphery'))
df_hist <- df_hist %>% 
  group_by(location) %>% 
  slice_sample(n=1000)

ggplot(df_hist, aes(ie,  
                    fill = location)) +
  geom_bar(stat="count", position = position_dodge(preserve = "single"),
           width = .1) +
  scale_fill_manual(values = c("red",
                               "gray")) +
  geom_vline(xintercept=4, linetype="dashed", color = "red") +
  theme_classic()


df_hist <- df_hist %>%
  mutate(ie = ifelse(ie > 6, 'Degradado', 'Integro'))

ggplot(df_hist, aes(ie,  
                    fill = location)) +
  geom_bar(stat="count", position = position_dodge(preserve = "single")) +
  theme_classic()



df_ie_anp <- as.data.frame(ie_anp, xy = TRUE)
df_ie_periphery <- as.data.frame(ie_periphery, xy = TRUE)

df_hist <- rbind(data.frame(ie = df_ie_anp$prediction,
                            location = 'anp'),
                 data.frame(ie = df_ie_periphery$prediction,
                            location = 'periphery'))

ggplot(df_hist,aes(x=ie,fill=location))+
  geom_histogram(aes(y=0.8*..density..),
                 position=position_dodge(preserve = "single"),
                 binwidth=0.8) +
  scale_fill_manual(values = c("orange",
                               "gray")) +
  theme_classic()

df_hist <- df_hist %>%
  mutate(ie = ifelse(ie > 6, 'Degradado', 'Integro'))
df_hist <- df_hist %>%
  group_by(location,ie) %>%
  summarise(pct = n()) %>%
  group_by(location) %>%
  mutate(pct = pct/sum(pct))

ggplot(df_hist, aes(x = ie, y = pct,
                    fill = location)) +
  geom_bar(stat="identity", 
           position = position_dodge(preserve = "single")) +
  theme_classic()





########## time series ###########
raster_files <- list.files('data/ie/', ".tif$", full.names = TRUE)
raster_list <- rast(raster_files)

col_names <- gsub('.tif','',raster_files)
col_names <- gsub('data/ie//ie_xgb_slic_','',col_names)

names(raster_list) <- col_names

raster_list <- crop(raster_list, anp_selected, mask=TRUE)
df_ie_years <- as.data.frame(raster_list, xy = FALSE)

df_ie_years <- df_ie_years %>% 
  pivot_longer(everything(), 
               names_to = 'year',
               values_to = "ie") %>% 
  drop_na()

df_ie_years <- df_ie_years %>%
  mutate(ie = ifelse(ie > 6, 'Degradado', 'Integro'))

df_ie_years <- df_ie_years %>%
  group_by(year,ie) %>%
  summarise(pct = n()) %>%
  group_by(year) %>%
  mutate(pct = pct/sum(pct))


ggplot(df_ie_years, aes(x = year, y = pct,
                    group = ie)) +
  geom_line(aes(color=ie)) +
  geom_point(aes(color=ie)) +
  scale_color_manual(values = c("red",
                               "darkgreen")) +
  theme_classic()

qplot(x = year, y = pct, fill = ie, 
      data = df_ie_years, geom = "col") +
  scale_fill_manual(values = c("red",
                                "darkgreen")) +
  theme_classic()
