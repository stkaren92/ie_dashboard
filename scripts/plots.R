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
for(anp_name in total_anps) {
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

#### distribution plot ####
anps_plot <- myt_anps
save_name <- "c_mariposa_monarca"
unique(df_cosmos_anps$Complejo)
anps_plot <- df_cosmos_anps[df_cosmos_anps$Complejo=="Complejo Mariposa Monarca",
                            "ANP_name"]

df_plot_anp <- df_dist %>% 
  filter(anp_name %in% anps_plot)
df_plot_anp$anp_name <- ifelse(df_plot_anp$anp_name == "Z.P.F.T.C.C. de los ríos Valle de Bravo, Malacatepec, Tilostoc y Temascaltepec",
                               "Valle de Bravo, Malacatepec, Tilostoc, Temascaltepec",
                               df_plot_anp$anp_name)
df_aux <- df_plot_anp %>% 
  filter(ie_4cat == "IE alta" &
           location == "anp" &
           year == "2023")
df_plot_anp <- df_plot_anp %>% 
  mutate(anp_name = factor(anp_name,
                           levels = levels(fct_reorder(df_aux$anp_name, 
                                                       desc(df_aux$pct)))))
ggplot(df_plot_anp, 
       aes(x = year, y = pct, fill = ie_4cat,  
           alpha = year)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  facet_grid(rows = vars(anp_name), 
             switch = "y",
             cols = vars(location_name)) +
  scale_alpha_manual("Año",
    values = c("2020" = 0.4, "2023" = 1),
    breaks = c("2020","2023")
  ) +
  scale_fill_manual("IE",
                    values = c("IE muy baja" = col[1],
                               "IE baja" = col[2],
                               "IE media" = col[3],
                               "IE alta" = col[4])) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic(base_size = 14) +
  theme(strip.text.y.left = element_text(angle = 0),
        strip.text.y = element_text(hjust = 1),
        axis.title.y=element_blank(),
        # legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(0.48, "cm"))
# ggsave(paste0("plots/distribucion_",save_name,".jpg"), 
#        width = 23, 
#        height = length(anps_plot)*1.2, units = "cm")

#### Efectividad plot ####
df_efectividad_anp <- df_efectividad %>% 
  filter(anp_name %in% anps_plot)
df_efectividad_anp$anp_name <- ifelse(df_efectividad_anp$anp_name == "Z.P.F.T.C.C. de los ríos Valle de Bravo, Malacatepec, Tilostoc y Temascaltepec",
                               "Valle de Bravo, Malacatepec, Tilostoc, Temascaltepec",
                               df_efectividad_anp$anp_name)

df_efectividad_anp <- df_efectividad_anp %>% 
  mutate(anp_name = factor(anp_name,
                           levels = levels(fct_reorder(df_aux$anp_name, 
                                                       (df_aux$pct)))))
ggplot(df_efectividad_anp, 
       aes(x = anp_name, y = efectividad)) +
  geom_hline(yintercept = 1, linetype="dashed", color = "red") +
  geom_line(color="azure4") + 
  geom_point(aes(color = factor(year))) +
  scale_y_continuous(trans = 'log10') +
  scale_color_manual("Año",
                     values = c("2020" = "orange",
                                "2023" = "blue")) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_classic(base_size = 14)
# ggsave(paste0("plots/efectividad_",save_name,".jpg"), 
#        width = 23, 
#        height = length(anps_plot)*1.2, units = "cm")


# df_aux <- df_plot_anp %>% 
#   filter(ie_4cat == "IE alta" & 
#            location == "anp")
# df_aux <- df_aux %>% 
#   pivot_wider(names_from = year, values_from = pct)
# 
# write.csv(df_aux %>% 
#              select(-c(location,location_name)),
#            row.names = F,
#            "plots/ie_alta_cosmos.csv")


# df_aux <- df_efectividad_anp  %>% 
#   select(anp_name, year, efectividad) %>% 
#   pivot_wider(names_from = year, values_from = efectividad)
# 
# write.csv(df_aux,
#           row.names = F,
#           "plots/efectividad_cosmos.csv")
