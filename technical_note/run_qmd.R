# Run Quarto file to create technical notes
library("here")

# Create technical notes for cosmos project
df_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')
for (anp in df_anps$ANP_name){
  print(anp)
  if(anp != "Ciénegas del Lerma") {
    anp_shortname <- df_anps[df_anps$ANP_name == anp, "ANP_shortname"]
    quarto::quarto_render(
      input = "technical_note/ficha.qmd",
      execute_dir = here(),
      output_file = paste0("ficha_",anp_shortname,".pdf"),
      execute_params = list(rproject_path = here(),
                            project_name = "cosmos",
                            anp_name= anp
                            )
    )
  }
}

# Create technical notes for sym project
df_anps <- read.csv('data/anp_sym/sym_anps.csv')
for (anp in df_anps$ANP_name){
  print(anp)
  anp_shortname <- df_anps[df_anps$ANP_name == anp, "ANP_shortname"]
  quarto::quarto_render(
    input = "technical_note/ficha.qmd",
    execute_dir = here(),
    output_file = paste0("ficha_",anp_shortname,".pdf"),
    execute_params = list(rproject_path = here(),
                          project_name = "other",
                          anp_name= anp
                          )
  )
}