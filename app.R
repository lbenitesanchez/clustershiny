# Cargar librerías necesarias asegurando su instalación
required_pkgs <- c(
  "shiny", "shinydashboard", "plotly", "DT", "dplyr", "ggplot2",
  "dendextend", "factoextra", "cluster", "corrplot", "RColorBrewer",
  "fmsb", "scales", "tidyr"
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("La librería '%s' no está instalada. Instalando...", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
