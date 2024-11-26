library(shiny)
library(ggplot2)
library(tidyverse)
library(sf)
library(terra)
library(leaflet)
library(grid)
library(gridExtra) 
library(bslib)

# Read IE rasters
raster_files <- list.files('data/ie/', ".tif$", full.names = TRUE)
raster_list <- rast(raster_files)
names(raster_list) <- gsub('data/ie//ie_xgb_slic_','',
                           gsub('.tif','',raster_files))

# Read ANP shapefile
anp_file <- vect('data/anp/186ANP_ITRF08_19012023.shp')
anp_file <-  project(anp_file, crs(raster_list['2017']))
cosmos_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')

# Threshold to define IE categories
ie_threshold <- 4
pal <- colorNumeric(c("darkgreen", "#FFFFCC", "red"), 
                    c(-11,18),
                    na.color = "transparent")

ui <- fluidPage(
  # Application title
  titlePanel("Integridad Ecológica en ANPs"),
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      width = 3,
      
      # Select filter ANPs in COSMOS
      checkboxInput("cosmos", label = "Filtrar COSMOS", value=TRUE),
      
      # Select ANP
      selectizeInput('anp', label = "Seleccionar ANP", 
                      choices = NULL, multiple=FALSE),
      # Select year
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
      sliderInput("year", label = "Seleccionar año", 
                  min = 2017, max = 2023, value = 2017,
                  animate = animationOptions(interval = 3000)),
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      card(
        # card_header(
        #   class = "bg-dark",
        #   "Integridad ecológica"
        # ),
        card_body(
          markdown("La integridad ecológica (IE) se predice a partir de un modelo XGBoost,
                   que considera como variables predictoras las zonas de vida de Holdridge 
                   y la elevación, como factores de las condiciones bioclimáticas. 
                   La condición de la vegetación se incluyó al modelo mediante datos 
                   de fotosíntesis y datos de radar. También se consideró el uso de suelo, 
                   que identifica distintos tipos relacionados a cierta integridad ecológica, 
                   como cultivos y asentamientos urbanos. Con base en esto, el modelo 
                   predice la integridad ecológica anual de todo el territorio Mexicano, 
                   donde 0 es el estado más íntegro y el 18 el más degradado.
                   <br />
                   <br />
                   El mapa tiene una resolución de 250x250m y se considera que el área 
                   está degradada si toma valores mayor o igual a 4.")
        )),
      # tableOutput("table_area"),
      leafletOutput("map"),
      plotOutput("ie_timeserires"),
      
      card(
        card_body(
          markdown(
            "<br />
            La integridad ecológica del Área Natural Protegida (ANP) es comparada
            con la integridad de la periferia. Esta se define como un buffer de 25km.
            Para que sean comparables los valores dentro del ANP y la periferia, 
            se toma una muestra aleatoria de 1000 pixeles, cuyas gráficas se muestran 
            a la izquierda. Por otro lado, también se estimó la frecuencia relativa, 
            graficada a la derecha."
          )
        )
      ),
      plotOutput("barplot_binary"),
      plotOutput("barplot_categories"),
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Returns list of ANPs' names
  anp_names_list <- reactive({
    if(input$cosmos==TRUE){
      anp_names_list <- subset(anp_file, 
                               anp_file$NOMBRE %in% cosmos_anps$ANP_name)$NOMBRE
    }else{
      anp_names_list <- anp_file$NOMBRE
    }
    return(anp_names_list)
  })
  
  # Updates list of ANPs to select from
  observeEvent(anp_names_list(),{
    updateSelectizeInput(session, 
                         inputId = "anp",
                         choices = sort(anp_names_list()),
                         selected = 'Barranca de Metztitlán'
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
    rast(paste0('data/ie/ie_xgb_slic_',input$year,'.tif'))
  })
  
  # Creates IE rasters by location (ANP, periphery and combined)
  # from the selected ANP and the basal year (2017)
  ie_by_location_basal <- reactive({
    req(input$anp)
    ie <- raster_list['2017']
    anp_selected <- anp_selected()
    
    ie_anp <- crop(ie, anp_selected, mask=TRUE)
    ie_anp_periphery <- crop(ie, 
                             buffer(anp_selected, 25000), 
                             mask=TRUE)
    ie_periphery <- mask(ie_anp_periphery,
                         anp_selected,
                         inverse = TRUE)
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
    ie_anp_periphery <- crop(ie, 
                             buffer(anp_selected, 25000), 
                             mask=TRUE)
    ie_periphery <- mask(ie_anp_periphery,
                         anp_selected,
                         inverse = TRUE)
    return(list(ie_anp = ie_anp, 
                ie_anp_periphery = ie_anp_periphery,
                ie_periphery = ie_periphery))
  })
  
  # Creates dataframes from IE rasters by location. 
  # One with all the values and another with 1000 random values
  ie_values <- reactive({
    req(input$anp)
    ie_by_location <- ie_by_location()
    
    df_ie <- rbind(data.frame(ie = as.data.frame(ie_by_location$ie_anp, 
                                                 xy = FALSE)$prediction, 
                              location = 'anp'),
                   data.frame(ie = as.data.frame(ie_by_location$ie_periphery,
                                                 xy = FALSE)$prediction, 
                              location = 'periphery'))
    df_sampled <- df_ie %>% 
      group_by(location) %>% 
      slice_sample(n=1000) 
    
    return(list(df_ie = df_ie,
                df_sampled = df_sampled))
    
  })
  
  # Creates dataframe with IE values for all available years 
  # of the ANP region
  df_timeseries <- reactive({
    req(input$anp)
    raster_list_anp <- crop(raster_list, anp_selected(), mask=TRUE)
    df_ie_years <- as.data.frame(raster_list_anp, xy = FALSE)
    
    df_ie_years <- df_ie_years %>% 
      pivot_longer(everything(), 
                   names_to = 'year',
                   values_to = "ie") %>% 
      drop_na() %>%
      mutate(ie = ifelse(ie >= ie_threshold, 'Degradado', 'Integro')) %>%
      group_by(year,ie) %>%
      summarise(pct = n()) %>%
      group_by(year) %>%
      mutate(pct = pct/sum(pct))
    
    return(df_ie_years)
    
  })
  
  # Creates dataframe with area in km2 of the ANP and periphery regions
  # df_area <- reactive({
  #   ie_by_location_basal <- ie_by_location_basal()
  #   data.frame(Location = c('ANP','Periferia'),
  #              Area = c(expanse(ie_by_location_basal$ie_anp, 
  #                               unit="km")$area,
  #                       expanse(ie_by_location_basal$ie_periphery, 
  #                               unit="km")$area))
  # })
  
  # Based map of selected ANP
  output$map <- renderLeaflet({
    
    leaflet() %>% addTiles() %>%
      addPolygons(data = st_transform(sf::st_as_sf(anp_selected()), 
                                      crs = '+proj=longlat +datum=WGS84'), 
                  weight = 2,
                  fillOpacity = 0.001, color = 'black') %>%
      addLegend(pal = pal, values = c(0,18),
                title = "IE")
  })
  # Updates map with IE from selected year
  observe({
    ie_by_location <- ie_by_location()
    leafletProxy("map") %>%
      addRasterImage(ie_by_location$ie_anp_periphery, 
                     colors = pal, opacity = 1)
  })
  
  # Bar plots of two IE categories according to ie_threshold
  output$barplot_binary <- renderPlot({
    ie_values <- ie_values()
    plt_count <- ggplot(ie_values$df_sampled %>% 
             mutate(ie = ifelse(ie >= ie_threshold, 'Degradado', 'Integro')),
           aes(ie, fill = location)) +
      geom_bar(stat="count", position = position_dodge(preserve = "single")) +
      scale_fill_manual("Ubicación",
                        values = c("orange",
                                    "gray"),
                        labels = c("ANP", "Periferia")) +
      scale_y_continuous(limits = c(0, 1000)) +
      labs(x = NULL, y = "n") +
      theme_classic()
    
    plt_pct <- ggplot(ie_values$df_ie %>% 
             mutate(ie = ifelse(ie >= ie_threshold, 'Degradado', 'Integro')) %>%
             group_by(location,ie) %>%
             summarise(pct = n()) %>%
             group_by(location) %>%
             mutate(pct = pct/sum(pct)),
           aes(x = ie, y = pct,
               fill = location)) +
      geom_bar(stat="identity", 
               position = position_dodge(preserve = "single")) +
      scale_fill_manual("Ubicación",
                        values = c("orange",
                                   "gray"),
                        labels = c("ANP", "Periferia")) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = NULL, y = "%") +
      theme_classic()
    
    grid.arrange(plt_count, plt_pct, ncol = 2, widths = c(1, 1))
  })
  
  # Bar plots of all IE categories
  output$barplot_categories <- renderPlot({
    ie_values <- ie_values()
    
    plt_count <- ggplot(ie_values$df_sampled,
           aes(ie, fill = location)) +
      geom_bar(stat="count", 
               position = position_dodge(preserve = "single"),
               width = 0.8) +
      geom_vline(xintercept=4.6, linetype="dashed", color = "red") +
      scale_fill_manual("Ubicación",
                        values = c("orange",
                                   "gray"),
                        labels = c("ANP", "Periferia")) +
      scale_y_continuous(limits = c(0, 1000)) +
      labs(x = "Integridad Ecológica", y = "n") +
      theme_classic()
    
    plt_pct <- ggplot(ie_values$df_ie,aes(x=ie,fill=location))+
      geom_histogram(aes(y=0.8*..density..),
                     position=position_dodge(preserve = "single"),
                     binwidth=0.8) +
      geom_vline(xintercept=4.4, linetype="dashed", color = "red") +
      scale_fill_manual("Ubicación",
                        values = c("orange",
                                   "gray"),
                        labels = c("ANP", "Periferia")) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "Integridad Ecológica", y = "%") +
      theme_classic()
    
    grid.arrange(plt_count, plt_pct, ncol = 2, widths = c(1, 1))
  })
  
  # Line plot of two IE categories according to ie_threshold 
  # by year and from ANP region
  output$ie_timeserires <- renderPlot({
    ggplot(df_timeseries(), aes(x = year, y = pct,
                            group = ie)) +
      geom_line(aes(color=ie)) +
      geom_point(aes(color=ie)) +
      scale_color_manual("IE",
                         values = c("red",
                                    "darkgreen")) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "Año", y = "%") +
      theme_classic()
  })
  
  # Table with area values
  # output$table_area <- renderTable(df_area())
}

# Run the application 
shinyApp(ui = ui, server = server)