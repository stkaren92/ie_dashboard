library(dplyr)
library(tidyverse)
library(ggplot2)
library(terra)
library(tidyterra)
library(leaflet)
library(sf)
library(RColorBrewer)
library(ggpattern)

buffer_folder <- "data/Zonas de Influencia_Vectores/"
anp <- vect('data/anp/186ANP_ITRF08_19012023.shp')
df_cosmos_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')
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
myt_anps <- c(v_norte, v_centro, v_sur)
cosmos_anps <- df_cosmos_anps$ANP_name
total_anps <- c(myt_anps, cosmos_anps)

buffer_files <- list.files(buffer_folder, pattern = "\\.shp$", full.names = TRUE)
buffer_list <- lapply(buffer_files, vect)
names(buffer_list) <- tools::file_path_sans_ext(basename(buffer_files))

col <- brewer.pal(4, "RdYlGn")

df_total <- data.frame()
for(anp_name in unique(df_cosmos_anps$Complejo)) {
  print(anp_name)
  for(y in c(2020,2023)){
    ie <- rast(paste0('data/ie/ie_xgb_',y,'.tif'))
    
    anps_plot <- df_cosmos_anps[df_cosmos_anps$Complejo==anp_name,
                            "ANP_name"]

    anp_selected <- subset(anp, anp$NOMBRE %in% anps_plot)
    anp_selected <- project(anp_selected, crs(ie))
    
    ie_anp <- crop(ie, anp_selected, mask=TRUE)
    if(anps_plot[1] %in% cosmos_anps){
      anp_selected_buffer <- df_cosmos_anps[df_cosmos_anps$ANP_name %in% anps_plot, 
                                            "Buffer_file"]
      
      periphery <- buffer_list[anp_selected_buffer]
      v_union <- Reduce(function(x, y) {
        terra::union(x, y)
      }, periphery)
      
      periphery <- project(v_union, crs(ie))
      
      ie_periphery <- crop(ie, periphery, mask=TRUE)
      
    }else{
      ie_anp_periphery <- crop(ie, 
                               buffer(anp_selected, 20000), 
                               mask=TRUE)
      ie_periphery <- mask(ie_anp_periphery,
                           anp_selected,
                           inverse = TRUE)
      names(ie_periphery) <- "prediction"
    }
    
    df_ie <- rbind(data.frame(ie = as.data.frame(ie_anp, 
                                                 xy = FALSE)$prediction, 
                              location = 'anp'),
                   data.frame(ie = as.data.frame(ie_periphery,
                                                 xy = FALSE)$prediction, 
                              location = 'periphery'))
    
    df_ie <- df_ie %>% 
      mutate(ie_4cat = ifelse(ie <= 4, "IE alta",
                              ifelse(ie <= 9, "IE media",
                                     ifelse(ie <= 13, "IE baja", "IE muy baja"))))
    df_pct <- df_ie %>% 
      group_by(location,ie_4cat) %>%
      summarise(pct = n()) %>%
      ungroup() %>% 
      group_by(location) %>%
      mutate(pct = pct/sum(pct))
    
    df_diff <- df_pct %>%
      select(location, ie_4cat, pct) %>%
      tidyr::pivot_wider(names_from = location, values_from = pct) %>%
      replace(is.na(.), 0)
    
    df_diff$anp_name <- anp_name
    df_diff$year <- y
    
    df_total <- rbind(df_total, df_diff)
  }
}

df_dist <- df_total %>% 
  pivot_longer(cols=c(anp,periphery), 
               names_to = "location", values_to = "pct") %>% 
  mutate(year = factor(year,
                       levels = c("2023","2020")),
         ie_4cat = factor(ie_4cat,
                          levels = c("IE muy baja",
                                     "IE baja",
                                     "IE media",
                                     "IE alta")),
         location_name = ifelse(location == "anp",
                                "ANP", "Zona de influencia"))

df_efectividad <- df_total %>% 
  filter(ie_4cat == "IE alta") %>% 
  mutate(efectividad = anp/periphery)

# write.csv(df_efectividad,
#           row.names = F,
#           "plots/efectividad_cosmos_complejos.csv")


#### por región ####
df_total <- data.frame()
for(anp_name in cosmos_anps) {
  print(anp_name)
  for(y in c(2020,2023)){
    ie <- rast(paste0('data/ie/ie_xgb_',y,'.tif'))
    anp_selected <- subset(anp, anp$NOMBRE == anp_name)
    anp_selected <- project(anp_selected, crs(ie))
    
    ie_anp <- crop(ie, anp_selected, mask=TRUE)
    if(anp_name %in% cosmos_anps){
      anp_selected_buffer <- df_cosmos_anps[df_cosmos_anps$ANP_name == anp_name, 
                                            "Buffer_file"]
      
      periphery <- buffer_list[[anp_selected_buffer]]
      periphery <- project(periphery, crs(ie))
      
      ie_periphery <- crop(ie, periphery, mask=TRUE)
      
    }else{
      ie_anp_periphery <- crop(ie, 
                               buffer(anp_selected, 20000), 
                               mask=TRUE)
      ie_periphery <- mask(ie_anp_periphery,
                           anp_selected,
                           inverse = TRUE)
      names(ie_periphery) <- "prediction"
    }
    
    df_ie <- rbind(data.frame(ie = as.data.frame(ie_anp, 
                                                 xy = FALSE)$prediction, 
                              location = 'anp'),
                   data.frame(ie = as.data.frame(ie_periphery,
                                                 xy = FALSE)$prediction, 
                              location = 'periphery'))
    
    df_ie <- df_ie %>% 
      mutate(ie_4cat = ifelse(ie <= 4, "IE alta",
                              ifelse(ie <= 9, "IE media",
                                     ifelse(ie <= 13, "IE baja", "IE muy baja"))))
    
    
    df_ie$anp_name <- anp_name
    df_ie$year <- y
    
    df_total <- rbind(df_total, df_ie)
  }
}

df_pct <- df_total %>% 
  group_by(location,ie_4cat, year) %>%
  summarise(pct = n()) %>%
  ungroup() %>% 
  group_by(location, year) %>%
  mutate(pct = pct/sum(pct))

df_efectividad <- df_pct %>% 
  filter(ie_4cat == "IE alta") %>% 
  pivot_wider(names_from = location, values_from = pct) %>% 
  mutate(efectividad = anp/periphery)