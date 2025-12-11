library(shiny)
library(ggplot2)
library(tidyverse)
library(sf)
library(terra)
library(leaflet)
library(grid)
library(gridExtra) 
library(bslib)
library(RColorBrewer)
library(shinythemes)
library(markdown)
library(shinydashboard)

# Read IE rasters
raster_files <- list.files('data/ie/', ".tif$", full.names = TRUE)
raster_list <- rast(raster_files)
names(raster_list) <- gsub('data/ie//ie_xgb_','',
                           gsub('.tif','',raster_files))

# Read ANP shapefile
anp_file <- vect('data/anp/186ANP_ITRF08_19012023.shp')
anp_file <-  project(anp_file, crs(raster_list['2017']))
cosmos_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')

# Read influence zones
buffer_folder <- "data/Zonas de Influencia_Vectores/"
buffer_files <- list.files(buffer_folder, pattern = "\\.shp$", full.names = TRUE)
buffer_list <- lapply(buffer_files, vect)
names(buffer_list) <- tools::file_path_sans_ext(basename(buffer_files))


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
anp_file <- anp_file[anp_file$NOMBRE %in% myt_anps |
                       anp_file$NOMBRE %in% cosmos_anps$ANP_name]

col_pal <- brewer.pal(4, "RdYlGn")

ui <- navbarPage(
  title = "Integridad ecosistémica",  
  
  # ----- HEADER -----
  header = tagList(
    tags$head(
      tags$style(HTML("
        /* ===== HEADER STYLES ===== */
        .custom-title {
          background-color: white;
          color: #03697F;
          padding: 5px 5px;
          border-bottom: 5px solid #03697F;
          font-size: 24px;
          display: flex;
          align-items: center;
          gap: 5px;
          margin-bottom: 20px;
        }
        .custom-title img { height: 70px; }

        /* ===== TABLE STYLES ===== */
        .table-container {
          overflow-x: auto;
          margin-top: 15px;
          border: 1px solid #ddd;
          border-radius: 8px;
          background-color: white;
          padding: 8px;
        }
        table {
          width: 100%;
          border-collapse: collapse;
          font-size: 16px;
          min-width: 800px;
        }
        th, td {
          border: 1px solid #ddd;
          padding: 6px 10px;
          text-align: left;
          vertical-align: top;
        }
        th {
          background-color: #f2f2f2;
          font-weight: bold;
        }
        tr:nth-child(even) {background-color: #fafafa;}
        
        /* ===== COLOR TAGS ===== */
        .alta { background-color: #1A9641; color: white; font-weight: bold; }
        .media { background-color: #A6D96A; color: white; font-weight: bold; }
        .baja { background-color: #FDAE61; color: white; font-weight: bold; }
        .muybaja { background-color: #D7191C; color: white; font-weight: bold; }

        /* ===== LEGEND ===== */
        .legend {
          gap: 10px;
          margin-bottom: 8px;
          margin-top: 10px;
        }
        .legend-item {
          align-items: center;
          gap: 5px;
          font-size: 13px;
        }
        .color-box {
          width: 18px;
          height: 18px;
          border-radius: 4px;
          border: 1px solid #ccc;
        }
        
        /* ===== MARGIN ===== */
        .tab-content {
          margin-left: 60px;
          margin-right: 60px;
          margin-bottom: 60px;
        }
        
        /* ===== BASE TEXT ===== */
        body {
          font-size: 18px;
          font-family: 'Cronos Pro';
        }
        .custom-heading {
          font-size: 22px;
          font-weight: bold;
        }
        
        /* ===== FOOTER ===== */
        .custom-footer {
          background-color: white;
          color: #00896E;
          padding: 10px 20px;
          border-top: 5px solid #00896E;
          font-size: 18px;
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 10px;
          margin-top: 40px;
        }
        .custom-footer img {
          height: 70px;
        }
      ")      
    )
    ),
    
    div(class = "custom-title",
        tags$img(src = "logos_cosmos.png", alt = "Logo")
    )
  ),
  
  # ----- TAB 2: MAPAS -----
  tabPanel("Mapas",
           icon = icon("map"),
           # Entire tab arranged with fluidRow instead of sidebarLayout
           fluidRow(
             # Left column: controls (replaces sidebarPanel)
             column(
               width = 3,
               # checkboxInput("cosmos", "Filtrar COSMOS", value = TRUE),
               selectizeInput('filtrar_anps', "Filtrar ANPs por proyecto",
                              choices = c("CoSMoS",
                                          "Sierra y Mar",
                                          "Total"),
                              selected = "Total"),
               selectizeInput('anp', "Seleccionar ANP", choices = NULL),
               selectizeInput('ie_categories', "Seleccionar mapa", 
                              choices = c("IE con 4 categorías", 
                                          "IE con 16 categorías")),
               sliderInput("year", "Seleccionar año", 
                           min = 2017, max = 2023, value = 2017,
                           step = 3,
                           animate = animationOptions(interval = 3000)),
               tags$a("Descargar ficha en PDF", 
                      href="FICHA IE ANP COSMOS Mariposa FINAL.pdf") 
             ),
             
             # Right column: main visual outputs
             column(
               width = 9,
               fluidRow(
                 column(
                   width = 3,
                   h4("% de IE alta", class = "custom-heading"),
                   fluidRow(
                     column(
                       width = 12,
                       valueBoxOutput("alta_value_anp", width = 12)
                     )
                   ),
                   br(),
                   fluidRow(
                     column(
                       width = 12,
                       valueBoxOutput("alta_value_periphery", width = 12)
                     )
                   )
                 ),
                 column(
                   width = 9,
                   h4("Mapa", class = "custom-heading"),
                   leafletOutput("map", height = "300px")
                 )
               ),
               fluidRow(
                 column(
                   width = 12,
                   h4("Comparación ANP vs Zona de influencia", 
                      class = "custom-heading"),
                   plotOutput("barplot_binary", height = "300px")
                 )
               ),
               fluidRow(
                 column(
                   width = 12,
                   h4("IE a través del tiempo",
                      class = "custom-heading"),
                   plotOutput("ie_timeseries", height = "300px")
                 )
               ),
               fluidRow(
                 column(
                   width = 12,
                   h4("Efectividad", class = "custom-heading"),
                   plotOutput("ie_timeseries_efectividad", height = "300px"),
                   markdown("La efectividad es el resultado de dividir el % de
                            IE alta de la ANP entre el de la zona de influencia.")
                 )
               )
             )
           )
  ),
  
  
  # ----- TAB 1: METODOLOGÍA -----
  tabPanel(
    title = "Metodología",
    icon = icon("book"),
    fluidPage(
      fluidRow(
        column(
          width = 12,
          tags$div(
            class = "card p-3 shadow-sm mb-4",
            tags$h4("Integridad ecosistémica", class = "custom-heading"),
            markdown("El Índice de Integridad ecosistémica (IIE) se estima a partir 
            de un modelo XGBoost, que considera como variables predictoras las 
            zonas de vida de Holdridge y la elevación, como factores de las 
            condiciones bioclimáticas. La condición de la vegetación se incluyó 
            al modelo mediante datos de fotosíntesis y datos de radar. También 
            se consideró la cobertura terrestre, que identifica distintos tipos 
            relacionados a cierta integridad ecosistémica, como cultivos y 
            asentamientos urbanos. Con base en esto, el modelo predice el IIE 
            anual de todo el territorio Mexicano, donde 0 es el estado más 
            íntegro y el 15 el más degradado.")
          )
        )
      ),
      
      
      # ---- TABLE + LEGEND ----
      fluidRow(
        column(
          12,
          div(
            class = "card p-3 shadow-sm mb-4",
            tags$h4("Clasificación del IIE", class = "custom-heading"),
            
            # Table
            div(class = "table-container",
                HTML('
                <table>
                  <thead>
                    <tr>
                      <th></th>
                      <th>IIE (16 categorías)</th>
                      <th>IIE (4 Categorías)</th>
                      <th>Descripción</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr><td>0</td><td> Estasis</td><td class="alta">IE alta</td><td>No hay cambio de tipo de vegetación ni de estado primario.</td></tr>
                    <tr><td>1</td><td> Pseudoestasis inferior</td><td class="alta">IE alta</td><td>No hay cambio de estado primario, sí hay cambio de tipo de vegetación pero dentro de la misma ecovariante.</td></tr>
                    <tr><td>2</td><td> Pseudoestasis media</td><td class="alta">IE alta</td><td>No hay cambio de estado primario, sí hay cambio de tipo de vegetación a otra ecovariante pero el estrato dominante ascendiendo en porte (e.g. Matorral a Bosque).</td></tr>
                    <tr><td>3</td><td> Pseudoestasis superior</td><td class="alta">IE alta</td><td>No hay cambio de estado primario, sí hay cambio de tipo de vegetación a otra ecovariante pero manteniéndose el porte del estrato dominante (e.g. Bosque de Encino a Selva Baja).</td></tr>
                    <tr><td>4</td><td> Degradación somera inferior</td><td class="alta">IE alta</td><td>No hay cambio de estado primario, sí hay cambio de tipo de vegetación a otra ecovariante pero descendiendo el porte del estrato dominante (e.g. Matorral a Pastizal o Herbazal). Cuerpo de agua cambia a Vegetación primaria.</td></tr>
                    <tr><td>5</td><td> Degradación somera media</td><td class="media">IE media</td><td>Cambio de cualquier tipo de Vegetación primaria o "Cuerpo de agua" a Vegetación Secundaria Arbórea</td></tr>
                    <tr><td>6</td><td> Degradación moderada superior</td><td class="media">IE media</td><td>Cambio de cualquier clase de Vegetación primaria o "Cuerpo de agua" a Bosque inducido o Bosque cultivado (antes Plantación forestal).</td></tr>
                    <tr><td>7</td><td> Degradación moderada inferior</td><td class="media">IE media</td><td>Cambio de cualquier clase de Vegetación primaria o "Cuerpo de agua" a Vegetación Secundaria Arbustiva</td></tr>
                    <tr><td>8</td><td> Degradación moderada media</td><td class="media">IE media</td><td>Cambio de cualquier clase de Vegetación primaria o "Cuerpo de agua" a Vegetación Sabanoide.</td></tr>
                    <tr><td>9</td><td> Degradación moderada superior</td><td class="media">IE media</td><td>Cambio de cualquier clase de Vegetación primaria o "Cuerpo de agua" a Vegetación Secundaria Herbácea.</td></tr>
                    <tr><td>10</td><td> Degradación severa inferior</td><td class="baja">IE baja</td><td>Cambio de cualquier clase de Vegetación primaria o "Cuerpo de agua" a Palmar inducido, Pastizal inducido o Pastizal cultivado.</td></tr>
                    <tr><td>11</td><td> Degradación severa media</td><td class="baja">IE baja</td><td>Cambio de cualquier clase de Vegetación primaria o "Cuerpo de agua" a Agricultura de temporal.</td></tr>
                    <tr><td>12</td><td> Degradación severa superior</td><td class="baja">IE baja</td><td>Cambio de cualquier clase de Vegetación primaria o "Cuerpo de agua" a Agricultura de riego.</td></tr>
                    <tr><td>13</td><td> Degradación muy severa inferior</td><td class="baja">IE baja</td><td>Cambio de cualquier clase de Vegetación primaria o "Cuerpo de agua" a Agricultura de humedad.</td></tr>
                    <tr><td>14</td><td> Degradación muy severa media</td><td class="muybaja">IE muy baja</td><td>Cambio de cualquier clase de Vegetación primaria o "Cuerpo de agua" a Desprovista de Vegetación.</td></tr>
                    <tr><td>15</td><td> Degradación muy severa superior</td><td class="muybaja">IE muy baja</td><td>Cambio de cualquier clase de Vegetación primaria a "Asentamiento humano".</td></tr>
                    </tbody>
                </table>
                ')
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$div(
            class = "card p-3 shadow-sm mb-4",
            tags$h4("Análisis", class = "custom-heading"),
            markdown("- Se toma como indicador de qué tan bien se encuentra un área 
                      al porcentaje de área con IE alta."),
            markdown("- La distribución de las cuatro categorías de IIE
                      del Área Natural Protegida (ANP) es comparada
                      con la distribución de su zona de influencia. 
                      Ésta se define para cada ANP de acuerdo a sus características 
                     o como un buffer de 20km."),
            markdown("- También se puede observar la distribución del IIE a lo largo del tiempo."),
            markdown("- Por último, se muestra la efectividad de la ANP a lo largo del tiempo. 
                        Ésta se estima como el cociente del porcentaje de IE alta de la ANP 
                        entre el de la zona de influencia.
                        <br>
                        a) Efectividad > 1 ANP tiene mayor proporción de IE alta que la zona de influencia
                        <br>
                        b) Efectividad = 1 ANP tiene igual proporción de IE alta que la zona de influencia
                        <br>
                        c) Efectividad < 1 ANP tiene menor proporción de IE alta que la zona de influencia")

          )
        )
      )
    )
  ),
  # ----- FOOTER -----
  footer = tags$footer(
    class = "custom-footer",
    div(
      class = "footer-content",
      tags$img(src = "logos_conabio.png", alt = "Logo")
      )
  )
)


# Define server
server <- function(input, output, session) {
  
  # # Returns list of ANPs' names
  # anp_names_list <- reactive({
  #   if(input$cosmos==TRUE){
  #     anp_names_list <- subset(anp_file, 
  #                              anp_file$NOMBRE %in% cosmos_anps$ANP_name)$NOMBRE
  #   }else{
  #     anp_names_list <- anp_file$NOMBRE
  #   }
  #   return(anp_names_list)
  # })
  
  # Returns list of ANPs' names
  anp_names_list <- reactive({
    if(input$filtrar_anps=="CoSMoS"){
      anp_names_list <- subset(anp_file, 
                               anp_file$NOMBRE %in% cosmos_anps$ANP_name)$NOMBRE
    }else if(input$filtrar_anps=="Sierra y Mar"){
      anp_names_list <- subset(anp_file, 
                               anp_file$NOMBRE %in% myt_anps)$NOMBRE
    }else{
      anp_names_list <- anp_file$NOMBRE
      }
    return(anp_names_list)
  })
  
  # Updates list of ANPs to select from
  observeEvent(anp_names_list(),{
    updateSelectizeInput(session, 
                         inputId = "anp",
                         choices = sort(anp_names_list())
    )
  })
  
  # Filters shapefile with selected ANP
  anp_selected <- reactive({
    req(input$anp)
    return(subset(anp_file, anp_file$NOMBRE == input$anp))
  })
  
  # Reads IE raster from the selected year
  ie_selected <- reactive({
    req(input$anp)
    rast(paste0('data/ie/ie_xgb_',input$year,'.tif'))
  })
  
  # Creates IE rasters by location (ANP, periphery and combined)
  # from the selected ANP and the basal year (2017)
  ie_by_location_basal <- reactive({
    req(input$anp)
    req(input$cosmos)
    ie <- raster_list['2017']
    anp_selected <- anp_selected()
    
    ie_anp <- crop(ie, anp_selected, mask=TRUE)
    
    if(input$anp %in% cosmos_anps$ANP_name){
      anp_selected_buffer <- cosmos_anps[cosmos_anps$ANP_name == input$anp, 
                                         "Buffer_file"]
      
      periphery <- buffer_list[[anp_selected_buffer]]
      periphery <- project(periphery, crs(ie))
      
      ie_periphery <- crop(ie, periphery, mask=TRUE)
      ie_anp_periphery <- merge(ie_anp, ie_periphery)
      
    }else{
      ie_anp_periphery <- crop(ie, 
                               buffer(anp_selected, 20000), 
                               mask=TRUE)
      ie_periphery <- mask(ie_anp_periphery,
                           anp_selected,
                           inverse = TRUE)
      names(ie_periphery) <- "prediction"
    }
  
    return(list(ie_anp = ie_anp, 
                ie_anp_periphery = ie_anp_periphery,
                ie_periphery = ie_periphery))
  })

  # Creates IE rasters by location (ANP, periphery and combined)
  # from the selected ANP and year
  ie_by_location <- reactive({
    req(input$anp)
    ie <- ie_selected()
    anp_selected <- anp_selected()
    
    ie_anp <- crop(ie, anp_selected, mask=TRUE)
    if(input$anp %in% cosmos_anps$ANP_name){
      anp_selected_buffer <- cosmos_anps[cosmos_anps$ANP_name == input$anp, 
                                         "Buffer_file"]
      
      periphery <- buffer_list[[anp_selected_buffer]]
      periphery <- project(periphery, crs(ie))
      
      ie_periphery <- crop(ie, periphery, mask=TRUE)
      ie_anp_periphery <- merge(ie_anp, ie_periphery)
      
    }else{
      ie_anp_periphery <- crop(ie, 
                               buffer(anp_selected, 20000), 
                               mask=TRUE)
      ie_periphery <- mask(ie_anp_periphery,
                           anp_selected,
                           inverse = TRUE)
      names(ie_periphery) <- "prediction"
    }
    return(list(ie_anp = ie_anp, 
                ie_anp_periphery = ie_anp_periphery,
                ie_periphery = ie_periphery))
  })
  
  # Creates dataframes from IE rasters by location
  ie_values <- reactive({
    req(input$anp)
    ie_by_location <- ie_by_location()
    
    df_ie <- rbind(data.frame(ie = as.data.frame(ie_by_location$ie_anp, 
                                                 xy = FALSE)$prediction, 
                              location = 'anp'),
                   data.frame(ie = as.data.frame(ie_by_location$ie_periphery,
                                                 xy = FALSE)$prediction, 
                              location = 'periphery'))
    
    df_ie <- df_ie %>% 
      mutate(ie_4cat = ifelse(ie <= 4, "d. IE alta",
                         ifelse(ie <= 9, "c. IE media",
                                ifelse(ie <= 13, "b. IE baja", "a. IE muy baja"))))
    
    df_ie_pct <- df_ie %>% 
      group_by(location,ie_4cat) %>%
      summarise(pct = n()) %>%
      ungroup() %>% 
      group_by(location) %>%
      mutate(pct = pct/sum(pct))
    
    return(list(df_ie = df_ie,
                df_ie_pct = df_ie_pct))
    
  })
  
  # Creates dataframe with IE values for all available years 
  # of the ANP region
  df_timeseries <- reactive({
    req(input$anp)
    anp_selected <- anp_selected()
    ie <- raster_list['2017']
    raster_list_anp <- crop(raster_list, anp_selected, mask=TRUE)
    
    if(input$anp %in% cosmos_anps$ANP_name){
      anp_selected_buffer <- cosmos_anps[cosmos_anps$ANP_name == input$anp,
                                         "Buffer_file"]

      periphery <- buffer_list[[anp_selected_buffer]]
      periphery <- project(periphery, crs(ie))
      
      raster_list_periphery <- crop(raster_list, periphery, mask=TRUE)

    }else{
      raster_list_periphery <- crop(raster_list,
                               buffer(anp_selected, 20000),
                               mask=TRUE)
      raster_list_periphery <- mask(raster_list_periphery,
                           anp_selected,
                           inverse = TRUE)
      names(raster_list_periphery) <- names(raster_list)
    }

    df_ie_years_anp <- as.data.frame(raster_list_anp, xy = FALSE)
    df_ie_years_anp <- df_ie_years_anp %>% 
      pivot_longer(everything(), 
                   names_to = 'year',
                   values_to = "ie") %>% 
      drop_na() %>%
      mutate(ie_4cat = ifelse(ie <= 4, "d. IE alta",
                              ifelse(ie <= 9, "c. IE media",
                                     ifelse(ie <= 13, "b. IE baja", "a. IE muy baja")))) %>% 
      group_by(year,ie_4cat) %>%
      summarise(pct = n()) %>%
      ungroup() %>% 
      group_by(year) %>%
      mutate(pct = pct/sum(pct),
             location = "anp")
    
    df_ie_years_periphery <- as.data.frame(raster_list_periphery, xy = FALSE)
    df_ie_years_periphery <- df_ie_years_periphery %>%
      pivot_longer(everything(),
                   names_to = 'year',
                   values_to = "ie") %>%
      drop_na() %>%
      mutate(ie_4cat = ifelse(ie <= 4, "d. IE alta",
                              ifelse(ie <= 9, "c. IE media",
                                     ifelse(ie <= 13, "b. IE baja", "a. IE muy baja")))) %>%
      group_by(year,ie_4cat) %>%
      summarise(pct = n()) %>%
      ungroup() %>%
      group_by(year) %>%
      mutate(pct = pct/sum(pct),
             location = "periphery")

    df_ie_years_total <- rbind(df_ie_years_anp, df_ie_years_periphery)
    
    return(list(df_ie_years_anp = df_ie_years_anp,
                df_ie_years_total = df_ie_years_total))
    
  })
  
  # Based map of selected ANP
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles(options = tileOptions(opacity = 0.5)) %>% # Set opacity to 50%
      addPolygons(data = st_transform(sf::st_as_sf(anp_selected()), 
                                      crs = '+proj=longlat +datum=WGS84'), 
                  weight = 2,
                  fillOpacity = 0.001, color = 'black')
      
  })
  # Updates map with IE from selected year
  observe({
    
    req(input$ie_categories)
    
    ie_by_location <- ie_by_location()
    r_ie <- ie_by_location$ie_anp_periphery
    
    r_ie <- project(r_ie, "EPSG:3857", method='near')
    
    if(input$ie_categories == "IE con 4 categorías") {
      
      r_ie <- ifel(r_ie <= 4, 1,
                        ifel(r_ie <= 9, 2,
                             ifel(r_ie <= 13, 3, 4)))
      
      pal_domain <- c(1,2,3,4)
      pal_labels <- c("IE alta","IE media","IE baja","IE muy baja")
      label_map <- setNames(pal_labels, pal_domain)
      pal <- colorFactor(c("#1A9641", "#A6D96A", "#FDAE61", "#D7191C"), 
                         domain=pal_domain,
                         na.color = "transparent")
      
    } else {
      pal_domain <- 0:15
      label_map <- setNames(0:15, pal_domain)
      pal <- colorNumeric(c("#1A9641", "#FFFFBF", "#D7191C"), 
                          pal_domain,
                          na.color = "transparent")
    }
    
    leafletProxy("map") %>%
      clearImages() %>%
      clearControls() %>%  # This clears any existing legends or controls
      addRasterImage(r_ie,
                     colors = pal, opacity = 1) %>%
      addLegend(pal =  pal, 
                values =pal_domain,
                title = "IE", 
                position = "bottomright", 
                labFormat = function(type, cuts) {
                  label_map[as.character(cuts)]
                })
  })
  
  # Bar plots of 4 IE categories
  output$barplot_binary <- renderPlot({
    ie_values <- ie_values()
    df_plot <- ie_values$df_ie_pct
    
    plt_pct_stacked <- ggplot(df_plot, aes(x = location, y = pct, fill = ie_4cat)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_x_discrete(labels= c("ANP", "Zona de influencia")) +
      scale_fill_manual("IE",
                         values = c("a. IE muy baja" = col_pal[1],
                                    "b. IE baja" = col_pal[2],
                                    "c. IE media" = col_pal[3],
                                    "d. IE alta" = col_pal[4])) +
      labs(x = NULL, y = NULL) +
      guides(fill="none") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_classic(base_size = 18,)
    
    
    df_diff <- df_plot %>%
      select(location, ie_4cat, pct) %>%
      tidyr::pivot_wider(names_from = location, values_from = pct) %>%
      replace(is.na(.), 0) %>% 
      mutate(diff = round((anp - periphery),2))
    
    
    plt_diff <- ggplot(df_diff, aes(x = ie_4cat, y = diff, fill = ie_4cat)) +
      geom_bar(stat = "identity") +
      scale_fill_manual("IE",
                        values = c("a. IE muy baja" = col_pal[1],
                                   "b. IE baja" = col_pal[2],
                                   "c. IE media" = col_pal[3],
                                   "d. IE alta" = col_pal[4])) +
      geom_hline(yintercept = 0, color = "black") +
      labs(x = "IE",
           y = "ANP - Zona de influencia") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_x_discrete(name = NULL, labels = NULL) +
      coord_flip() +
      theme_classic(base_size = 18)
    
    grid.arrange(plt_pct_stacked, plt_diff, ncol = 2)
  })
  
  # Line plot of 4 IE categories by year and from ANP region
  output$ie_timeseries <- renderPlot({
    df_timeseries <- df_timeseries()
    
    ggplot(df_timeseries$df_ie_years_anp,
           aes(x = as.numeric(year), y = pct, group = ie_4cat)) +
      geom_line(aes(color=ie_4cat)) +
      geom_point(aes(color=ie_4cat)) +
      scale_color_manual("IE",
                         values = c("a. IE muy baja" = col_pal[1],
                                    "b. IE baja" = col_pal[2],
                                    "c. IE media" = col_pal[3],
                                    "d. IE alta" = col_pal[4])) +
      scale_y_continuous(limits = c(0, 1),
                         labels = scales::percent_format()) +
      scale_x_continuous(breaks = c(2017, 2020, 2023)) +
      guides(color="none") +
      labs(x = NULL, y = NULL) +
      theme_classic(base_size = 18)
    
    
    # ggplot(df_timeseries$df_ie_years_total, 
    #        aes(x = as.numeric(year), y = pct, 
    #            shape = location,
    #            group = interaction(ie_4cat,location))) +
    #   geom_line(aes(color=ie_4cat, linewidth = location)) +
    #   scale_color_manual("IE",
    #                      values = c("a. IE muy baja" = col_pal[1],
    #                                 "b. IE baja" = col_pal[2],
    #                                 "c. IE media" = col_pal[3],
    #                                 "d. IE alta" = col_pal[4])) +
    #   scale_y_continuous(#limits = c(0, 1),
    #                      labels = scales::percent_format()) +
    #   scale_x_continuous(breaks = c(2017, 2020, 2023)) +
    #   scale_linewidth_manual("",
    #                          values = c("anp" = 1.3, "periphery" = 0.4),
    #                          labels = c("ANP", "Zona de influencia")) +
    #   guides(color="none") +
    #   labs(x = NULL, y = NULL) +
    #   theme_classic(base_size = 18) +
    #   theme(legend.position = "top")
  })
  
  # Line plot of efectivity by year and from ANP region
  output$ie_timeseries_efectividad <- renderPlot({
    
    df_timeseries <- df_timeseries()
    df_efectividad <- df_timeseries$df_ie_years_total

    df_efectividad <- df_efectividad %>% 
      filter(ie_4cat == "d. IE alta") %>% 
      pivot_wider(names_from = location, values_from = pct) %>% 
      ungroup() %>% 
      mutate(efectividad = anp/periphery,
             year = as.numeric(year))

    ggplot(df_efectividad, aes(x = year, y = efectividad)) +
      geom_line(color="blue") +
      geom_point(color="blue") +
      labs(x = NULL, y = "Efectividad") +
      scale_x_continuous(breaks = c(2017, 2020, 2023)) +
      geom_hline(yintercept=1, linetype="dashed", color = "red") +
      theme_classic(base_size = 18)
  })
  
  # % IE alta ANP
  output$alta_value_anp <- renderUI({
    ie_values <- ie_values()
    df_ie_pct <- ie_values$df_ie_pct
    
    value <- df_ie_pct %>% 
      filter(ie_4cat == "d. IE alta" &
               location =="anp") %>% 
      pull(pct)
    
    value_box(
      value = tags$p(paste0(round(value,4)*100,"%"), 
                     style = "font-size: 500%;"),
      title = tags$p("ANP", style = "font-size: 18px;"),
      theme = value_box_theme(fg = "#1A9641")
    )
  })
  
  # % IE alta periphery
  output$alta_value_periphery <- renderUI({
    ie_values <- ie_values()
    df_ie_pct <- ie_values$df_ie_pct
    
    value <- df_ie_pct %>% 
      filter(ie_4cat == "d. IE alta" &
               location =="periphery") %>% 
      pull(pct)
    
    value_box(
      value = tags$p(paste0(round(value,4)*100,"%"), 
                     style = "font-size: 300%;"),
      title = tags$p("Zona de influencia", style = "font-size: 18px;"),
      theme = value_box_theme(fg = "#1A9641")
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)