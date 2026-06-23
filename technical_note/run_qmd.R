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

# Create technical notes for cosmos project
df_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')
for (anp in df_anps$ANP_name){
  print(anp)
  if(anp != "Ciénegas del Lerma") {
    anp_shortname <- df_anps[df_anps$ANP_name == anp, "ANP_shortname"]
    render_ficha(anp,
                 anp_shortname,
                 "cosmos",
                 "technical_note/header_cosmos.tex")
  }
}

# Create technical notes for sym project
df_anps <- read.csv('data/anp_sym/sym_anps.csv')
for (anp in df_anps$ANP_name){
  print(anp)
  anp_shortname <- df_anps[df_anps$ANP_name == anp, "ANP_shortname"]
  render_ficha(anp,
               anp_shortname,
               "other",
               "technical_note/header_sym.tex")
}
