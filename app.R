# Cargar librerías necesarias
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)
library(dendextend)
library(factoextra)
library(cluster)
library(corrplot)
library(RColorBrewer)
library(fmsb)
library(scales)
library(tidyr)

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
