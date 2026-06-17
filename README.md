# Dashboard de Integridad Ecosistemica

Aplicacion Shiny para explorar el Indice de Integridad Ecosistemica (IIE) en Areas Naturales Protegidas (ANP) y sus zonas de influencia. El tablero permite filtrar por ANP, visualizar mapas por año, comparar la distribucion del IIE dentro del ANP contra su zona de influencia, revisar cambios temporales y descargar fichas tecnicas en PDF.

## Contenido principal

- Mapa interactivo con rasters de IIE para 2017, 2020 y 2023.
- Visualizacion del IIE en 4 categorias o en 16 categorias.
- Comparacion entre ANP y zona de influencia.
- Serie temporal del porcentaje de area por categoria de IIE.
- Indicador de eficacia: porcentaje de IIE alta en el ANP dividido entre el porcentaje de IIE alta en la zona de influencia.
- Descarga de fichas tecnicas por ANP desde la carpeta `www/`.

## Estructura del proyecto

```text
.
|-- app.R                         # Aplicacion Shiny
|-- data/
|   |-- anp/                      # Shapefile base de ANP y ajustes locales
|   |-- anp_cosmos/               # Catalogo de ANP del proyecto CoSMoS
|   |-- anp_sym/                  # Catalogo de ANP del proyecto Sierra y Mar
|   |-- ie/                       # Rasters de IIE por año
|   `-- Zonas de Influencia_Vectores/
|       |-- *.shp                 # Zonas de influencia por ANP
|       `-- Metadato/             # Metadatos de zonas de influencia
|-- technical_note/
|   |-- ficha.qmd                 # Plantilla Quarto para fichas tecnicas
|   |-- run_qmd.R                 # Render masivo de fichas
|   |-- header_cosmos.tex         # Encabezado LaTeX para fichas
|   `-- font/                     # Fuentes usadas por la ficha
|-- www/                          # Recursos publicos de Shiny: logos y PDFs
```

## Requisitos

Para desarrollo local:

- R con soporte para `renv`.
- Librerias de sistema para datos espaciales: GDAL, GEOS, PROJ, UDUNITS y dependencias de compilacion usadas por `sf`, `terra` y paquetes graficos.
- Quarto y una distribucion LaTeX con XeLaTeX si se van a regenerar las fichas PDF.

El archivo `renv.lock` registra el ambiente de R del proyecto. El contenedor Docker instala las dependencias principales sobre `rocker/shiny:4.5.2`.

## Instalacion local

Desde la raiz del proyecto:

```r
renv::restore()
```

La activacion de `renv` esta configurada en `.Rprofile`, por lo que al abrir el proyecto en RStudio se cargara el ambiente del proyecto automaticamente.

## Ejecutar la aplicacion localmente

En R:

```r
shiny::runApp()
```

Tambien se puede abrir `ie_dashboard.Rproj` en RStudio y ejecutar `app.R`.

## Datos usados por la app

La aplicacion carga los siguientes insumos al iniciar:

- `data/ie/ie_xgb_2017.tif`, `data/ie/ie_xgb_2020.tif` y `data/ie/ie_xgb_2023.tif`: rasters del IIE.
- `data/anp/232_ANP-ITRF08_04072025.shp`: poligonos base de ANP.
- `data/anp/cienegas_lerma/Subzon_CLerma_utmitrf08_DOFPM.shp`: geometria usada para reemplazar/agregar el caso de Cienegas del Lerma.
- `data/anp_cosmos/cosmos_anps.csv`: ANP del proyecto CoSMoS y el archivo de zona de influencia asociado.
- `data/anp_sym/sym_anps.csv`: ANP del proyecto Sierra y Mar.
- `data/Zonas de Influencia_Vectores/*.shp`: zonas de influencia. Para ANP sin zona definida en el catalogo CoSMoS, la app calcula un buffer de 20 km.

Los archivos de `www/ficha_*.pdf` son las fichas descargables que aparecen en el tablero.

## Regenerar fichas tecnicas

Las fichas se generan con Quarto a partir de `technical_note/ficha.qmd`. Para renderizar todas las fichas definidas en los catalogos:

```r
source("technical_note/run_qmd.R")
```

El script recorre las ANP de `data/anp_cosmos/cosmos_anps.csv` y `data/anp_sym/sym_anps.csv`, genera archivos `ficha_<clave>.pdf` y usa los parametros `rproject_path`, `project_name` y `anp_name` de la plantilla Quarto.

Despues de regenerarlas, copia o mueve las fichas finales a `www/` para que la app pueda ofrecerlas como descarga.

## Notas de mantenimiento

- Si se agrega una nueva ANP, actualiza el catalogo correspondiente (`cosmos_anps.csv` o `sym_anps.csv`) y verifica que exista la ficha `www/ficha_<ANP_shortname>.pdf`.
- Si una ANP CoSMoS usa zona de influencia definida, el campo `Buffer_file` debe coincidir con el nombre base del shapefile en `data/Zonas de Influencia_Vectores/`.
- Si se agregan nuevos años de IIE, coloca los rasters como `data/ie/ie_xgb_<año>.tif` y ajusta el `sliderInput` en `app.R`.
