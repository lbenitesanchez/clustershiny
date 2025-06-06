# UI
ui <- dashboardPage(
  skin = "purple",
  
  # Header
  dashboardHeader(
    title = "Análisis de Conglomerados - NovaRetail",
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Exploración de Datos", tabName = "exploracion", icon = icon("chart-bar")),
      menuItem("Clustering Jerárquico", tabName = "hclust", icon = icon("sitemap")),
      menuItem("K-means", tabName = "kmeans", icon = icon("circle-nodes")),
      menuItem("Comparación", tabName = "comparacion", icon = icon("balance-scale")),
      menuItem("Perfiles de Clusters", tabName = "perfiles", icon = icon("users"))
    )
  ),
  
  # Body
  dashboardBody(
    # CSS personalizado
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .small-box {
          border-radius: 10px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .nav-tabs-custom>.nav-tabs>li.active {
          border-top-color: #605ca8;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background: #605ca8 !important;
          border-color: #605ca8 !important;
          color: white !important;
        }
        .btn-primary {
          background-color: #605ca8;
          border-color: #605ca8;
        }
        .btn-primary:hover {
          background-color: #504a96;
          border-color: #504a96;
        }
      "))
    ),
    
    tabItems(
      # Tab Inicio
      tabItem(
        tabName = "inicio",
        fluidRow(
          column(12,
                 h2("Análisis de Conglomerados - NovaRetail", style = "text-align: center; color: #605ca8;"),
                 br()
          )
        ),
        fluidRow(
          valueBoxOutput("nClientes"),
          valueBoxOutput("nVariables"),
          valueBoxOutput("ticketPromedio")
        ),
        fluidRow(
          box(
            title = "Descripción del Dataset",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Este dataset contiene información de 50 clientes de NovaRetail con las siguientes variables:"),
            tags$ul(
              tags$li(strong("freq_visitas:"), " Frecuencia de visitas del cliente"),
              tags$li(strong("ticket_promedio:"), " Valor promedio de compra por visita"),
              tags$li(strong("diversidad_categorías:"), " Variedad de categorías de productos comprados"),
              tags$li(strong("uso_app:"), " Si el cliente usa la aplicación móvil (0=No, 1=Sí)"),
              tags$li(strong("distancia_tienda:"), " Distancia del cliente a la tienda más cercana"),
              tags$li(strong("edad:"), " Edad del cliente")
            ),
            br(),
            p("Esta aplicación permite realizar análisis de conglomerados mediante dos métodos principales:"),
            tags$ol(
              tags$li(strong("Clustering Jerárquico (hclust):"), " Agrupa clientes de forma jerárquica"),
              tags$li(strong("K-means:"), " Particiona clientes en k grupos distintos")
            )
          )
        )
      ),
      
      # Tab Exploración
      tabItem(
        tabName = "exploracion",
        fluidRow(
          box(
            title = "Estadísticas Descriptivas",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("statsTable")
          ),
          box(
            title = "Matriz de Correlación",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("corrPlot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Distribución de Variables",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            selectInput("varExplore", "Selecciona una variable:",
                        choices = c("freq_visitas", "ticket_promedio", "diversidad_categorías", 
                                    "uso_app", "distancia_tienda", "edad")),
            plotlyOutput("distPlot", height = "400px")
          )
        )
      ),
      
      # Tab Clustering Jerárquico
      tabItem(
        tabName = "hclust",
        fluidRow(
          box(
            title = "Configuración",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            checkboxGroupInput("varsHclust", "Variables a incluir:",
                               choices = c("freq_visitas", "ticket_promedio", "diversidad_categorías", 
                                           "uso_app", "distancia_tienda", "edad"),
                               selected = c("freq_visitas", "ticket_promedio", "diversidad_categorías", "edad")),
            selectInput("distMethod", "Método de distancia:",
                        choices = c("euclidean", "manhattan", "maximum", "canberra", "minkowski"),
                        selected = "euclidean"),
            selectInput("linkMethod", "Método de enlace:",
                        choices = c("complete", "single", "average", "ward.D2", "centroid"),
                        selected = "ward.D2"),
            numericInput("nClustersHC", "Número de clusters:", value = 3, min = 2, max = 10),
            actionButton("runHclust", "Ejecutar Análisis", class = "btn-primary", style = "width: 100%;")
          ),
          box(
            title = "Dendrograma",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotOutput("dendrograma", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Visualización de Clusters",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("hclustPlot", height = "500px")
          )
        )
      ),
      
      # Tab K-means
      tabItem(
        tabName = "kmeans",
        fluidRow(
          box(
            title = "Configuración",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            checkboxGroupInput("varsKmeans", "Variables a incluir:",
                               choices = c("freq_visitas", "ticket_promedio", "diversidad_categorías", 
                                           "uso_app", "distancia_tienda", "edad"),
                               selected = c("freq_visitas", "ticket_promedio", "diversidad_categorías", "edad")),
            sliderInput("maxK", "Rango de k para evaluación:", min = 2, max = 10, value = c(2, 8)),
            numericInput("nClustersKM", "Número de clusters seleccionado:", value = 3, min = 2, max = 10),
            numericInput("nstart", "Número de inicializaciones:", value = 25, min = 1, max = 50),
            actionButton("runKmeans", "Ejecutar Análisis", class = "btn-primary", style = "width: 100%;")
          ),
          tabBox(
            title = "Determinación del número óptimo de k",
            width = 8,
            tabPanel("Método del Codo",
                     plotOutput("elbowPlot", height = "400px")
            ),
            tabPanel("Método Silhouette",
                     plotOutput("silhouettePlot", height = "400px")
            )
          )
        ),
        fluidRow(
          box(
            title = "Visualización de Clusters K-means",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("kmeansPlot", height = "500px")
          )
        )
      ),
      
      # Tab Comparación
      tabItem(
        tabName = "comparacion",
        fluidRow(
          box(
            title = "Comparación de Métodos",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            h4("Métricas de Calidad del Clustering"),
            DTOutput("comparisonTable"),
            br(),
            h4("Visualización Comparativa"),
            fluidRow(
              column(6, plotlyOutput("hclustCompare", height = "400px")),
              column(6, plotlyOutput("kmeansCompare", height = "400px"))
            )
          )
        )
      ),
      
      # Tab Perfiles
      tabItem(
        tabName = "perfiles",
        fluidRow(
          box(
            title = "Selección de Método",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            radioButtons("methodProfile", "Método de clustering:",
                         choices = c("Jerárquico" = "hclust", "K-means" = "kmeans"),
                         selected = "kmeans", inline = TRUE)
          )
        ),
        fluidRow(
          box(
            title = "Análisis ANOVA - Diferencias entre Clusters",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            p("Pruebas ANOVA para verificar si existen diferencias significativas entre clusters:"),
            DTOutput("anovaTable"),
            br(),
            p(em("Valores p < 0.05 indican diferencias estadísticamente significativas entre al menos dos clusters"))
          )
        ),
        fluidRow(
          box(
            title = "Perfiles de Clusters - Gráfico Radar",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("radarPlot", height = "400px")
          ),
          box(
            title = "Distribución por Variable - Gráfico de Cajas",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            selectInput("varBoxplot", "Selecciona una variable:",
                        choices = c("freq_visitas", "ticket_promedio", "diversidad_categorías", 
                                    "distancia_tienda", "edad"),
                        selected = "ticket_promedio"),
            plotlyOutput("boxplotProfile", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "Comparación de Variables por Cluster - Método Simple",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("simpleBoxplots", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Comparación de Variables por Cluster - Método Avanzado",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("multiBoxplot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Características por Cluster",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("clusterProfile")
          )
        )
      )
    )
  )
)
