To run locally (get this working before you try to deploy):

- From outside R: run `sudo R CMD javareconf`
- From inside R: 
  - `install.packages(c("devtools", "tidyverse", "shiny", "rsconnect", "DT", "markdown", "scales", "rJava"))`
  - `devtools::install_github("hrbrmstr/metricsgraphics")`
  - `shiny::runApp(".")`

To deploy: follow instructions here: https://shiny.posit.co/r/articles/share/shinyapps/
