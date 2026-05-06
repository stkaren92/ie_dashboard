df_anps <- read.csv('data/anp_cosmos/cosmos_anps.csv')

for (anp in df_anps$ANP_name){
  print(anp)
  if(anp != "Ciénegas del Lerma") {
    anp_shortname <- df_anps[df_anps$ANP_name == anp, "ANP_shortname"]
    quarto::quarto_render(
      "/Users/kasanchez/Documents/ie_dashboard/technical_note/ficha.qmd",
      output_file = paste0("ficha_",anp_shortname,".pdf"),
      execute_params = list(anp_name= anp, 
                            project_name = "cosmos")
    )
  }
}

df_anps <- read.csv('data/anp_sym/sym_anps.csv')
for (anp in df_anps$ANP_name){
  print(anp)
  anp_shortname <- df_anps[df_anps$ANP_name == anp, "ANP_shortname"]
  quarto::quarto_render(
    "/Users/kasanchez/Documents/ie_dashboard/technical_note/ficha.qmd",
    output_file = paste0("ficha_",anp_shortname,".pdf"),
    execute_params = list(anp_name= anp, 
                          project_name = "sym")
  )
}


