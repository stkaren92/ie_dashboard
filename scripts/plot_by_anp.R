library(dplyr)
library(tidyverse)
library(ggplot2)
library(terra)
library(tidyterra)
library(leaflet)
library(sf)
library(RColorBrewer)
library(ggpattern)

anp_selected <- "Mariposa Monarca"

buffer_folder <- "data/Zonas de Influencia_Vectores/"
anp <- vect('data/anp/186ANP_ITRF08_19012023.shp')
df_cosmos_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')
complejo_selected <- df_cosmos_anps[df_cosmos_anps$ANP_name == anp_selected,"Complejo"]


buffer_files <- list.files(buffer_folder, pattern = "\\.shp$", full.names = TRUE)
buffer_list <- lapply(buffer_files, vect)
names(buffer_list) <- tools::file_path_sans_ext(basename(buffer_files))

col <- brewer.pal(4, "RdYlGn")

df_cosmos_anps <- df_cosmos_anps %>% 
  filter(Complejo == complejo_selected)

df_total <- data.frame()
for(anp_name in df_cosmos_anps$ANP_name) {
  print(anp_name)
  for(y in c(2020,2023)){
    ie <- rast(paste0('data/ie/ie_xgb_',y,'.tif'))
    anp_selected <- subset(anp, anp$NOMBRE == anp_name)
    anp_selected <- project(anp_selected, crs(ie))
    
    ie_anp <- crop(ie, anp_selected, mask=TRUE)
    anp_selected_buffer <- df_cosmos_anps[df_cosmos_anps$ANP_name == anp_name, 
                                            "Buffer_file"]
      
    periphery <- buffer_list[[anp_selected_buffer]]
    periphery <- project(periphery, crs(ie))
      
    ie_periphery <- crop(ie, periphery, mask=TRUE)
      
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
  group_by(anp_name,year,location,ie_4cat) %>%
  summarise(pct = n()) %>%
  ungroup() %>% 
  group_by(anp_name,year,location) %>%
  mutate(pct = pct/sum(pct))

df_pct <- df_pct %>%
  tidyr::pivot_wider(names_from = location, values_from = pct) %>%
  replace(is.na(.), 0)

df_pct <- df_pct %>% 
  left_join(df_cosmos_anps %>% 
              select(ANP_name,ANP_shortname),
            by=c("anp_name"="ANP_name"))

df_dist <- df_pct %>% 
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

df_efectividad <- df_dist %>% 
  filter(ie_4cat == "IE alta") %>% 
  mutate(efectividad = anp/periphery)

# Reorder ANPs by IEalta 2023
df_aux <- df_dist %>% 
  filter(ie_4cat == "IE alta" &
           location == "anp" &
           year == "2023")
df_dist <- df_dist %>% 
  mutate(anp_name = factor(anp_name,
                           levels = levels(fct_reorder(df_aux$anp_name, 
                                                       desc(df_aux$pct)))))
df_efectividad <- df_efectividad %>% 
  mutate(anp_name = factor(anp_name,
                           levels = levels(fct_reorder(df_aux$anp_name, 
                                                       (df_aux$pct)))))

#### distribution plot ####

ggplot(df_dist, 
       aes(x = year, y = pct, fill = ie_4cat,  
           alpha = year)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  facet_grid(rows = vars(ANP_shortname), 
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

#### Efectividad plot ####
ggplot(df_efectividad, 
       aes(x = ANP_shortname, y = efectividad)) +
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


table_ie_alta <- df_dist %>% 
  filter(ie_4cat == "IE alta" & location == "anp") %>% 
  select(ANP_shortname, year, pct) %>% 
  mutate(pct = round(pct*100,1)) %>% 
  pivot_wider(names_from = "ANP_shortname",
              values_from = "pct")

table_efectividad <- df_efectividad %>% 
  select(ANP_shortname, year, efectividad) %>% 
  mutate(efectividad = round(efectividad,1)) %>% 
  pivot_wider(names_from = "ANP_shortname",
              values_from = "efectividad")

table_presion <- df_dist %>% 
  filter(ie_4cat == "IE alta" & location == "periphery") %>% 
  select(ANP_shortname, year, pct) %>% 
  mutate(pct = round(100-pct*100,1)) %>% 
  pivot_wider(names_from = "ANP_shortname",
              values_from = "pct")


table_ie_alta_change <- df_dist %>% 
  filter(ie_4cat == "IE alta" & location == "anp") %>% 
  mutate(pct = round(pct*100,1)) %>% 
  select(ANP_shortname, year, pct) %>% 
  pivot_wider(names_from = "year",
              values_from = "pct") %>% 
  mutate(diff_ie_alta = round(`2023` - `2020`),
         diff_ie_alta_symbol = case_when(
           diff_ie_alta > 0 ~ "up",
           diff_ie_alta < 0 ~ "down",
           diff_ie_alta == 0 ~ "equal"
         ),
         diff_ie_alta_color = case_when(
           `2023` <= 25 ~ "baja",
           `2023` > 25 & `2023` <= 50 ~ "media",
           `2023` > 50 & `2023` <= 75 ~ "alta",
           `2023` > 75 ~ "muy alta",
         ))


table_efectividad_change <- df_efectividad %>% 
  select(ANP_shortname, year, efectividad) %>% 
  mutate(efectividad = round(efectividad,1)) %>% 
  pivot_wider(names_from = "year",
              values_from = "efectividad") %>%
  mutate(diff_ef = `2023` - `2020`,
         diff_ef_symbol = case_when(
           diff_ef > 0 ~ "up",
           diff_ef < 0 ~ "down",
           diff_ef == 0 ~ "equal"
         ),
         diff_ef_color = case_when(
           `2023` <= 1 ~ "baja",
           `2023` > 1 & `2023` <= 2 ~ "media",
           `2023` > 2 & `2023` <= 3 ~ "alta",
           `2023` > 3 ~ "muy alta",
         ))

table_change <- table_ie_alta_change %>% 
  select(ANP_shortname, diff_ie_alta_symbol, diff_ie_alta_color) %>% 
  left_join(table_efectividad_change %>% 
              select(ANP_shortname, diff_ef_symbol, diff_ef_color),
            by="ANP_shortname")

colores <- c(
  "muyalta"  = "#1A9641",
  "alta" = "#A6D96A",
  "media"  = "#FDAE61",
  "baja" = "#D7191C"
)

names(table_change) <- c("ANP", "IESymbol", "IEColor",
                         "EfSymbol", "EfColor")

table_change %>%
  mutate(color_fill = colores[IEColor]) %>%
  kable(escape = FALSE, align = "c", format = "latex") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(3, background = table_change %>% 
                pull(IEColor) %>% 
                purrr::map_chr(~ colores[.x]))
