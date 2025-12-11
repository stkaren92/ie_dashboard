library(dplyr)
library(tidyverse)
library(ggplot2)
library(terra)
library(tidyterra)
library(leaflet)
library(sf)
library(RColorBrewer)

anp_name <- "Sierra de Huautla"

anp <- vect('data/anp/186ANP_ITRF08_19012023.shp')
ie <- rast('data/ie/ie_xgb_2017.tif')
cosmos_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')

sort(anp$NOMBRE)
sort(unique(anp$REGION))

anp_cosmos <- subset(anp, anp$NOMBRE %in% cosmos_anps$ANP_name)
setdiff(cosmos_anps$ANP_name,anp_cosmos$NOMBRE)
# writeVector(anp_cosmos, "data/anp_cosmos/anp_cosmos.shp")

anp_selected <- subset(anp, anp$NOMBRE == anp_name)
anp_selected <- project(anp_selected, crs(ie))

periphery <- buffer(anp_selected, 25000)

ie_anp <- crop(ie, anp_selected, mask=TRUE)
ie_anp_periphery <- crop(ie, periphery, mask=TRUE)
ie_periphery <- mask(ie_anp_periphery,
                         anp_selected,
                         inverse = TRUE)

plot(ie_periphery)

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


r_ie <- ifel(ie_anp_periphery <= 3, 1,
             ifel(ie_anp_periphery <= 8, 2,
                  ifel(ie_anp_periphery <= 14, 3, 4)))
plot(r_ie)

pal_domain <- c(1,2,3,4)
pal_labels <- c("IE Alta","IE Media","IE Baja","IE Muy Baja")
label_map <- setNames(pal_labels, pal_domain)
pal <- colorFactor(c("#1a9641", "#c4e687", "#fec981", "#d7191c"), 
                   domain=pal_domain,
                   na.color = "transparent")

r_ie <- project(r_ie, "EPSG:3857", method='near')
leaflet() %>% addTiles() %>%
  addRasterImage(r_ie,
                 colors = pal, opacity = 1) %>%
  addLegend(pal =  pal, 
            values =pal_domain,
            title = "", 
            position = "bottomright", 
            labFormat = function(type, cuts) {
              label_map[as.character(cuts)]
            })

hist(r_ie)

brewer.pal(5, "RdYlGn")

######### bar chart ############
df_ie_anp <- as.data.frame(ie_anp, xy = TRUE)
df_ie_periphery <- as.data.frame(ie_periphery, xy = TRUE)

df_hist <- rbind(data.frame(ie = df_ie_anp$prediction, 
                            location = 'anp'),
                 data.frame(ie = df_ie_periphery[,3], 
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
                 data.frame(ie = df_ie_periphery$layer,
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

ggplot(df_hist, aes(x = ie, y = pct,
                    fill = ie, alpha = location)) +
  geom_bar(stat = "identity",
           position = position_dodge(preserve = "single")) +
  scale_fill_manual(
    values = c( "#1a9850","#d73027")
  ) +
  scale_alpha_manual(
    values = c("anp" = 1, "periphery" = 0.3)
  ) +
  labs(x = "Integridad ecológica", y = "%", fill = "IE", alpha = "Ubicación") +
  theme_classic()


ggplot(df_hist, aes(x = location, y = pct, fill = ie)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Ubicación",
                    values = c("orange", "gray"),
                    labels = c("ANP", "Periferia")) +
  labs(x = "Integridad ecológica", y = "Proporción") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic()


########## time series ###########
buffer_folder <- "data/Zonas de Influencia_Vectores/"
buffer_files <- list.files(buffer_folder, pattern = "\\.shp$", full.names = TRUE)
buffer_list <- lapply(buffer_files, vect)
names(buffer_list) <- tools::file_path_sans_ext(basename(buffer_files))
ie <- rast('data/ie/ie_xgb_2017.tif')

anp_name <- "Mariposa Monarca"

anp <- vect('data/anp/186ANP_ITRF08_19012023.shp')
cosmos_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')

anp_selected <- subset(anp, anp$NOMBRE == anp_name)
anp_selected <- project(anp_selected, crs(ie))
anp_selected_buffer <- cosmos_anps[cosmos_anps$ANP_name == anp_name, "Buffer_file"]

periphery <- buffer_list[[anp_selected_buffer]]
periphery <- project(periphery, crs(ie))

raster_files <- list.files('data/ie/', ".tif$", full.names = TRUE)
raster_list <- rast(raster_files)

col_names <- gsub('.tif','',raster_files)
col_names <- gsub('data/ie//ie_xgb_','',col_names)

names(raster_list) <- col_names

raster_list_anp <- crop(raster_list, anp_selected, mask=TRUE)
raster_list_periphery <- crop(raster_list, periphery, mask=TRUE)

raster_list_periphery <- crop(raster_list,
                              buffer(anp_selected, 20000),
                              mask=TRUE)
raster_list_periphery <- mask(raster_list_periphery,
                              anp_selected,
                              inverse = TRUE)
names(raster_list_periphery) <- col_names
plot(raster_list_periphery)

df_ie_years_anp <- as.data.frame(raster_list_anp, xy = FALSE)
df_ie_years_periphery <- as.data.frame(raster_list_periphery, xy = FALSE)

df_ie_years_anp <- df_ie_years_anp %>% 
  pivot_longer(everything(), 
               names_to = 'year',
               values_to = "ie") %>% 
  drop_na()

df_ie_years_anp <- df_ie_years_anp %>%
  mutate(ie = ifelse(ie > 6, 'Degradado', 'Integro'))

df_ie_years_anp <- df_ie_years_anp %>%
  group_by(year,ie) %>%
  summarise(pct = n()) %>%
  group_by(year) %>%
  mutate(pct = pct/sum(pct))

df_ie_years_anp$location <- "anp"

df_ie_years_periphery <- df_ie_years_periphery %>% 
  pivot_longer(everything(), 
               names_to = 'year',
               values_to = "ie") %>% 
  drop_na()

df_ie_years_periphery <- df_ie_years_periphery %>%
  mutate(ie = ifelse(ie > 6, 'Degradado', 'Integro'))

df_ie_years_periphery <- df_ie_years_periphery %>%
  group_by(year,ie) %>%
  summarise(pct = n()) %>%
  group_by(year) %>%
  mutate(pct = pct/sum(pct))

df_ie_years_periphery$location <- "periphery"

df_ie_years_total <- rbind(df_ie_years_anp, df_ie_years_periphery)

ggplot(df_ie_years_total, aes(x = year, y = pct,  shape = location,
                    group = interaction(ie,location))) +
  geom_line(aes(color=ie)) +
  geom_point(aes(color=ie)) +
  scale_color_manual(values = c("red",
                               "darkgreen")) +
  theme_classic()

qplot(x = year, y = pct, fill = ie, 
      data = df_ie_years_anp, geom = "col") +
  scale_fill_manual(values = c("red",
                                "darkgreen")) +
  theme_classic()

df_efectividad <- df_ie_years_total %>% 
  filter(ie == "Integro") %>% 
  pivot_wider(names_from = location, values_from = pct) %>% 
  ungroup() %>% 
  mutate(efectividad = anp/periphery,
         year = as.numeric(year))

ggplot(df_efectividad, aes(x = year, y = efectividad)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 10)) +
  theme_classic()

############ replace 25 km buffer by influence zone shapefiles ##########
buffer_folder <- "data/Zonas de Influencia_Vectores/"
buffer_files <- list.files(buffer_folder, pattern = "\\.shp$", full.names = TRUE)
buffer_list <- lapply(buffer_files, vect)
names(buffer_list) <- tools::file_path_sans_ext(basename(buffer_files))

anp_name <- "Corredor Biológico Chichinautzin"

anp <- vect('data/anp/186ANP_ITRF08_19012023.shp')
ie <- rast('data/ie/ie_xgb_slic_2017.tif')
cosmos_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')

sort(anp$NOMBRE)
sort(unique(anp$REGION))

anp_cosmos <- subset(anp, anp$NOMBRE %in% cosmos_anps$ANP_name)
setdiff(cosmos_anps$ANP_name,anp_cosmos$NOMBRE)
# writeVector(anp_cosmos, "data/anp_cosmos/anp_cosmos.shp")

anp_selected <- subset(anp, anp$NOMBRE == anp_name)
anp_selected <- project(anp_selected, crs(ie))
anp_selected_buffer <- cosmos_anps[cosmos_anps$ANP_name == anp_name, "Buffer_file"]

periphery <- buffer_list[[anp_selected_buffer]]
periphery <- project(periphery, crs(ie))

ie_anp <- crop(ie, anp_selected, mask=TRUE)
ie_periphery <- crop(ie, periphery, mask=TRUE)
ie_anp_periphery <- merge(ie_anp, ie_periphery)


plot(ie_anp_periphery)


##### project ####
raster_files <- list.files('data/ie/', ".tif$", full.names = TRUE)

r_2017 <- rast(raster_files[1])
for(i in 2:length(raster_files)) {
  r <- rast(raster_files[i])
  r <- project(r, r_2017)
  
  writeRaster(r, raster_files[i], overwrite=T)
}



##### difference plot ####
buffer_folder <- "data/Zonas de Influencia_Vectores/"
buffer_files <- list.files(buffer_folder, pattern = "\\.shp$", full.names = TRUE)
buffer_list <- lapply(buffer_files, vect)
names(buffer_list) <- tools::file_path_sans_ext(basename(buffer_files))

anp_name <- "El Tepozteco"

anp <- vect('data/anp/186ANP_ITRF08_19012023.shp')
ie <- rast('data/ie/ie_xgb_2017.tif')
cosmos_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')

sort(anp$NOMBRE)
sort(unique(anp$REGION))

anp_cosmos <- subset(anp, anp$NOMBRE %in% cosmos_anps$ANP_name)
setdiff(cosmos_anps$ANP_name,anp_cosmos$NOMBRE)
# writeVector(anp_cosmos, "data/anp_cosmos/anp_cosmos.shp")

anp_selected <- subset(anp, anp$NOMBRE == anp_name)
anp_selected <- project(anp_selected, crs(ie))
anp_selected_buffer <- cosmos_anps[cosmos_anps$ANP_name == anp_name, "Buffer_file"]

periphery <- buffer_list[[anp_selected_buffer]]
periphery <- project(periphery, crs(ie))

ie_anp <- crop(ie, anp_selected, mask=TRUE)
ie_periphery <- crop(ie, periphery, mask=TRUE)
ie_anp_periphery <- merge(ie_anp, ie_periphery)

plot(ie_anp_periphery)



df_ie <- rbind(data.frame(ie = as.data.frame(ie_anp, 
                                             xy = FALSE)$prediction, 
                          location = 'anp'),
               data.frame(ie = as.data.frame(ie_periphery,
                                             xy = FALSE)$prediction, 
                          location = 'periphery'))

df_ie <- df_ie %>% 
  mutate(ie_4cat = ifelse(ie <= 3, "d. IE alta",
                          ifelse(ie <= 8, "c. IE media",
                                 ifelse(ie <= 14, "b. IE baja", "a. IE muy baja"))))


col <- brewer.pal(4, "RdYlGn")

df_plot <- df_ie %>% 
  group_by(location,ie_4cat) %>%
  summarise(pct = n()) %>%
  ungroup() %>% 
  group_by(location) %>%
  mutate(pct = pct/sum(pct))

df_diff <- df_plot %>%
  select(location, ie_4cat, pct) %>%
  tidyr::pivot_wider(names_from = location, values_from = pct) %>%
  replace(is.na(.), 0) %>% 
  mutate(diff = round((anp - periphery)*100))  # replace Loc1, Loc2 with your actual names

ggplot(df_diff, aes(x = ie_4cat, y = diff, fill = diff > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "IE Category",
       y = "Difference (Loc2 - Loc1)",
       title = "Difference in Proportions Between Locations") +
  coord_flip() +
  theme_minimal()


##### global plot ####
buffer_folder <- "data/Zonas de Influencia_Vectores/"
buffer_files <- list.files(buffer_folder, pattern = "\\.shp$", full.names = TRUE)
buffer_list <- lapply(buffer_files, vect)
names(buffer_list) <- tools::file_path_sans_ext(basename(buffer_files))

anp <- vect('data/anp/186ANP_ITRF08_19012023.shp')
cosmos_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')

anp_cosmos <- subset(anp, anp$NOMBRE %in% cosmos_anps$ANP_name)

df_total <- data.frame()
for(anp_name in cosmos_anps$ANP_name) {
  for(y in 2017:2024){
    ie <- rast(paste0('data/ie/ie_xgb_',y,'.tif'))
    print(anp_name)
    anp_selected <- subset(anp, anp$NOMBRE == anp_name)
    anp_selected <- project(anp_selected, crs(ie))
    anp_selected_buffer <- cosmos_anps[cosmos_anps$ANP_name == anp_name, "Buffer_file"]
    
    periphery <- buffer_list[[anp_selected_buffer]]
    periphery <- project(periphery, crs(ie))
    
    ie_anp <- crop(ie, anp_selected, mask=TRUE)
    ie_periphery <- crop(ie, periphery, mask=TRUE)
    ie_anp_periphery <- merge(ie_anp, ie_periphery)
    
    df_ie <- rbind(data.frame(ie = as.data.frame(ie_anp, 
                                                 xy = FALSE)$prediction, 
                              location = 'anp'),
                   data.frame(ie = as.data.frame(ie_periphery,
                                                 xy = FALSE)$prediction, 
                              location = 'periphery'))
    
    df_ie <- df_ie %>% 
      mutate(ie_4cat = ifelse(ie <= 4, "d. IE alta",
                              ifelse(ie <= 9, "c. IE media",
                                     ifelse(ie <= 13, "b. IE baja", "a. IE muy baja"))))
    
    
    col <- brewer.pal(4, "RdYlGn")
    
    df_plot <- df_ie %>% 
      group_by(location,ie_4cat) %>%
      summarise(pct = n()) %>%
      ungroup() %>% 
      group_by(location) %>%
      mutate(pct = pct/sum(pct))
    
    df_diff <- df_plot %>%
      select(location, ie_4cat, pct) %>%
      tidyr::pivot_wider(names_from = location, values_from = pct) %>%
      replace(is.na(.), 0) %>% 
      mutate(diff = round((anp - periphery)*100))  # replace Loc1, Loc2 with your actual names
    
    df_diff$anp_name <- anp_name
    df_diff$year <- y
    
    df_total <- rbind(df_total, df_diff)
  }
}

col <- brewer.pal(4, "RdYlGn")

ggplot(df_total %>% 
         filter(year == 2020 | year == 2023) %>% 
         pivot_longer(cols=c(anp,periphery), names_to = "location", values_to = "pct"), 
       aes(x = location, y = pct, fill = ie_4cat,  alpha = location)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Integridad ecológica", y = "Proporción") +
  coord_flip() +
  facet_grid(rows = vars(anp_name), switch = "y",
             cols = vars(year)) +
  scale_alpha_manual(
    values = c("anp" = 1, "periphery" = 0.4)
  ) +
  scale_fill_manual("IE",
                    values = c("a. IE muy baja" = col[1],
                               "b. IE baja" = col[2],
                               "c. IE media" = col[3],
                               "d. IE alta" = col[4])) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  theme(strip.text.y.left = element_text(angle = 0),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


ggplot(df_total %>% 
         filter((year == 2020 | year == 2023)
                & anp_name == "Mariposa Monarca") %>% 
         pivot_longer(cols=c(anp,periphery), names_to = "location", values_to = "pct"), 
       aes(x = location, y = pct, fill = ie_4cat,  alpha = location)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Integridad ecológica", y = "Proporción") +
  facet_grid(rows = vars(anp_name),
             cols = vars(year)) +
  scale_alpha_manual(
    values = c("anp" = 1, "periphery" = 0.4)
  ) +
  scale_fill_manual("IE",
                    values = c("a. IE muy baja" = col[1],
                               "b. IE baja" = col[2],
                               "c. IE media" = col[3],
                               "d. IE alta" = col[4])) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(strip.text.y.left = element_text(angle = 0),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



df_efectividad <- df_total %>% 
  filter(ie_4cat == "d. IE alta") %>% 
  mutate(efectividad = anp/periphery) %>% 
  filter(year == 2020 | year == 2023)

ggplot(df_efectividad, 
       aes(x = fct_reorder(anp_name, anp), y = efectividad)) +
  geom_hline(yintercept = 1, linetype="dashed", color = "red") +
  geom_line(color="azure4") + 
  geom_point(aes(color = factor(year))) +
  scale_y_continuous(trans = 'log10') +
  scale_color_manual("Año",
                    values = c("2020" = "orange",
                               "2023" = "blue")) +
  labs(x = NULL, y = "Efectividad") +
  coord_flip() +
  theme_classic()

#### conglomerados ####
anp <- vect('data/anp/186ANP_ITRF08_19012023.shp')

df_anp <- data.frame(anp_name = anp$NOMBRE)

v_norte <- c("Cañón de Santa Elena",
              "Ocampo",
              "Maderas del Carmen",
              "C.A.D.N.R. 004 Don Martín")

v_centro <- c("Sierra del Abra Tanchipa",
              "Sierra Gorda",
              "Z.P.F.V. la Cuenca Hidrográfica del Río Necaxa",
              "Pico de Orizaba",
              "Cofre de Perote o Nauhcampatépetl")

v_sur <- c("Los Tuxtlas",
            "Pantanos de Centla",
            "Cañón del Usumacinta")
