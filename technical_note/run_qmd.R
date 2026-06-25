# Run Quarto file to create technical notes
library("here")

render_ficha <- function(anp, anp_shortname, project_name, header_file) {
  header_path <- normalizePath(file.path(here(), header_file), mustWork = TRUE)

  quarto::quarto_render(
    input = "technical_note/ficha.qmd",
    execute_dir = here(),
    output_file = paste0("ficha_", anp_shortname, ".pdf"),
    execute_params = list(rproject_path = here(),
                          project_name = project_name,
                          anp_name = anp
                          ),
    pandoc_args = c(paste0("--include-in-header=", header_path))
  )
}

catalogo_anps <- read.csv('data/catalogo_anps.csv',
                          fileEncoding = "UTF-8-BOM")

header_by_project <- c(
  "CoSMoS" = "technical_note/header_cosmos.tex",
  "Sierra y Mar" = "technical_note/header_sym.tex"
)

for (project_name in names(header_by_project)) {
  df_anps <- catalogo_anps[catalogo_anps$Proyecto == project_name, ]
  for (anp in df_anps$ANP_name) {
    print(anp)
    if(anp != "Ciénegas del Lerma") {
      anp_shortname <- df_anps[df_anps$ANP_name == anp, "ANP_shortname"]
      render_ficha(anp,
                   anp_shortname,
                   project_name,
                   header_by_project[[project_name]])
    }
  }
}
