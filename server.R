# Server (SOLO UNO)
server <- function(input, output, session) {
  
  # Cargar datos
  data <- reactive({
    # Cargar base de datos ubicada en la carpeta "data"

    df <- read.csv(file.path("data", "NovaRetail.csv"), stringsAsFactors = FALSE)

    # Unificar nombre de columna identificadora
    if (!"id_cliente" %in% names(df)) {
      id_col <- grep("cliente", names(df), ignore.case = TRUE, value = TRUE)
      if (length(id_col) == 0) {
        id_col <- names(df)[1]
      } else {
        id_col <- id_col[1]
      }
      names(df)[names(df) == id_col] <- "id_cliente"
    }

    df
  })
  
  # Value boxes
  output$nClientes <- renderValueBox({
    valueBox(
      value = nrow(data()),
      subtitle = "Total de Clientes",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$nVariables <- renderValueBox({
    valueBox(
      value = ncol(data()) - 1,
      subtitle = "Variables de Análisis",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$ticketPromedio <- renderValueBox({
    valueBox(
      value = paste0("$", round(mean(data()$ticket_promedio), 2)),
      subtitle = "Ticket Promedio General",
      icon = icon("shopping-cart"),
      color = "green"
    )
  })
  
  # Estadísticas descriptivas
  output$statsTable <- renderDT({
    df <- data() %>%
      select(-id_cliente) %>%
      summarise_all(list(
        Media = ~round(mean(., na.rm = TRUE), 2),
        Mediana = ~round(median(., na.rm = TRUE), 2),
        DE = ~round(sd(., na.rm = TRUE), 2),
        Min = ~round(min(., na.rm = TRUE), 2),
        Max = ~round(max(., na.rm = TRUE), 2)
      ))
    
    stats_df <- data.frame(
      Variable = rep(names(select(data(), -id_cliente)), each = 5),
      Estadística = rep(c("Media", "Mediana", "Desv. Est.", "Mínimo", "Máximo"), length(names(select(data(), -id_cliente)))),
      Valor = unlist(df)
    )
    
    stats_wide <- reshape(stats_df, idvar = "Variable", timevar = "Estadística", direction = "wide")
    names(stats_wide) <- gsub("Valor.", "", names(stats_wide))
    
    datatable(stats_wide, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  # Matriz de correlación
  output$corrPlot <- renderPlot({
    cor_matrix <- cor(select(data(), -id_cliente))
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", addCoef.col = "black",
             tl.col = "black", tl.srt = 45,
             col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200))
  })
  
  # Distribución de variables
  output$distPlot <- renderPlotly({
    if(input$varExplore == "uso_app") {
      # Para variable binaria, usar gráfico de barras
      p <- ggplot(data(), aes(x = factor(uso_app))) +
        geom_bar(fill = "#605ca8", alpha = 0.7) +
        scale_x_discrete(labels = c("0" = "No usa app", "1" = "Usa app")) +
        theme_minimal() +
        labs(title = "Distribución de Uso de App",
             x = "Uso de App", y = "Frecuencia") +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    } else {
      # Para variables continuas, usar histograma con densidad
      p <- ggplot(data(), aes_string(x = input$varExplore)) +
        geom_histogram(aes(y = ..density..), bins = 20, fill = "#605ca8", alpha = 0.7) +
        geom_density(color = "#ff6b6b", size = 1.2) +
        theme_minimal() +
        labs(title = paste("Distribución de", input$varExplore),
             x = input$varExplore, y = "Densidad") +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    }
    
    ggplotly(p)
  })
  
  # Clustering Jerárquico
  hclust_results <- eventReactive(input$runHclust, {
    req(input$varsHclust)
    
    # Preparar datos
    df_scaled <- scale(select(data(), all_of(input$varsHclust)))
    rownames(df_scaled) <- data()$id_cliente
    
    # Calcular distancias y clustering
    dist_matrix <- dist(df_scaled, method = input$distMethod)
    hc <- hclust(dist_matrix, method = input$linkMethod)
    clusters <- cutree(hc, k = input$nClustersHC)
    
    list(hc = hc, clusters = clusters, data_scaled = df_scaled)
  })
  
  output$dendrograma <- renderPlot({
    req(hclust_results())
    
    dend <- as.dendrogram(hclust_results()$hc)
    dend <- color_branches(dend, k = input$nClustersHC)
    
    plot(dend, main = "Dendrograma - Clustering Jerárquico",
         ylab = "Distancia", xlab = "Clientes")
    rect.hclust(hclust_results()$hc, k = input$nClustersHC, border = 2:10)
  })
  
  output$hclustPlot <- renderPlotly({
    req(hclust_results())
    
    pca <- prcomp(hclust_results()$data_scaled)
    df_pca <- data.frame(
      PC1 = pca$x[,1],
      PC2 = pca$x[,2],
      Cluster = factor(hclust_results()$clusters),
      Cliente = rownames(hclust_results()$data_scaled)
    )
    
    p <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster, text = Cliente)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      labs(title = "Visualización de Clusters - Componentes Principales",
           x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 1), "%)"),
           y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 1), "%)")) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    
    ggplotly(p, tooltip = c("text", "Cluster"))
  })
  
  # K-means
  kmeans_results <- eventReactive(input$runKmeans, {
    req(input$varsKmeans)
    
    # Preparar datos
    df_scaled <- scale(select(data(), all_of(input$varsKmeans)))
    rownames(df_scaled) <- data()$id_cliente
    
    # Método del codo
    wss <- sapply(input$maxK[1]:input$maxK[2], function(k){
      kmeans(df_scaled, centers = k, nstart = 25)$tot.withinss
    })
    
    # Método silhouette
    avg_sil <- sapply(input$maxK[1]:input$maxK[2], function(k){
      km <- kmeans(df_scaled, centers = k, nstart = 25)
      ss <- silhouette(km$cluster, dist(df_scaled))
      mean(ss[, 3])
    })
    
    # K-means final
    km_final <- kmeans(df_scaled, centers = input$nClustersKM, nstart = input$nstart)
    
    list(
      km = km_final,
      wss = wss,
      avg_sil = avg_sil,
      k_range = input$maxK[1]:input$maxK[2],
      data_scaled = df_scaled
    )
  })
  
  output$elbowPlot <- renderPlot({
    req(kmeans_results())

    num_scaled <- kmeans_results()$data_scaled

    fviz_nbclust(num_scaled,
                 kmeans,
                 method  = "wss",
                 k.max   = 10,
                 nstart  = 25) +
      labs(title = "Método del Codo")
  })

  output$silhouettePlot <- renderPlot({
    req(kmeans_results())

    num_scaled <- kmeans_results()$data_scaled

    fviz_nbclust(num_scaled,
                 kmeans,
                 method  = "silhouette",
                 k.max   = 10,
                 nstart  = 25) +
      labs(title = "Análisis Silhouette")
  })
  
  output$kmeansPlot <- renderPlotly({
    req(kmeans_results())
    
    pca <- prcomp(kmeans_results()$data_scaled)
    df_pca <- data.frame(
      PC1 = pca$x[,1],
      PC2 = pca$x[,2],
      Cluster = factor(kmeans_results()$km$cluster),
      Cliente = rownames(kmeans_results()$data_scaled)
    )
    
    # Centroides en PCA
    centroids_pca <- predict(pca, kmeans_results()$km$centers)
    
    p <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster, text = Cliente)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_point(data = data.frame(PC1 = centroids_pca[,1], PC2 = centroids_pca[,2]),
                 aes(x = PC1, y = PC2), color = "black", size = 5, shape = 17) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      labs(title = "Visualización de Clusters K-means - Componentes Principales",
           x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 1), "%)"),
           y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 1), "%)")) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    
    ggplotly(p, tooltip = c("text", "Cluster"))
  })
  
  # Comparación
  output$comparisonTable <- renderDT({
    req(hclust_results(), kmeans_results())
    
    # Calcular métricas para hierarchical
    hc_sil <- silhouette(hclust_results()$clusters, dist(hclust_results()$data_scaled))
    hc_avg_sil <- mean(hc_sil[, 3])
    
    # Calcular métricas para k-means
    km_sil <- silhouette(kmeans_results()$km$cluster, dist(kmeans_results()$data_scaled))
    km_avg_sil <- mean(km_sil[, 3])
    
    # Calcular varianza entre clusters para hierarchical
    hc_data <- hclust_results()$data_scaled
    hc_clusters <- hclust_results()$clusters
    hc_centers <- aggregate(hc_data, by = list(hc_clusters), FUN = mean)[,-1]
    hc_total_var <- sum(apply(hc_data, 2, var)) * (nrow(hc_data) - 1)
    hc_within_var <- sum(sapply(unique(hc_clusters), function(k) {
      cluster_data <- hc_data[hc_clusters == k, , drop = FALSE]
      if(nrow(cluster_data) > 1) {
        sum(apply(cluster_data, 2, var)) * (nrow(cluster_data) - 1)
      } else {
        0
      }
    }))
    hc_between_var <- hc_total_var - hc_within_var
    hc_var_explained <- hc_between_var / hc_total_var
    
    comparison_df <- data.frame(
      Método = c("Clustering Jerárquico", "K-means"),
      `Número de Clusters` = c(input$nClustersHC, input$nClustersKM),
      `Silhouette Promedio` = round(c(hc_avg_sil, km_avg_sil), 3),
      `Varianza Explicada` = paste0(round(c(hc_var_explained, 
                                            kmeans_results()$km$betweenss/kmeans_results()$km$totss) * 100, 1), "%"),
      `Criterio` = c(paste("Ward.D2,", input$distMethod), 
                     paste(input$nstart, "inicializaciones"))
    )
    
    datatable(comparison_df, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$hclustCompare <- renderPlotly({
    req(hclust_results())
    
    pca <- prcomp(hclust_results()$data_scaled)
    df_pca <- data.frame(
      PC1 = pca$x[,1],
      PC2 = pca$x[,2],
      Cluster = factor(hclust_results()$clusters)
    )
    
    p <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      labs(title = "Clustering Jerárquico",
           x = "PC1", y = "PC2") +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    
    ggplotly(p)
  })
  
  output$kmeansCompare <- renderPlotly({
    req(kmeans_results())
    
    pca <- prcomp(kmeans_results()$data_scaled)
    df_pca <- data.frame(
      PC1 = pca$x[,1],
      PC2 = pca$x[,2],
      Cluster = factor(kmeans_results()$km$cluster)
    )
    
    p <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      labs(title = "K-means",
           x = "PC1", y = "PC2") +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Perfiles
  # Tabla ANOVA
  output$anovaTable <- renderDT({
    req(input$methodProfile)
    
    if(input$methodProfile == "hclust" && !is.null(hclust_results())) {
      clusters <- factor(hclust_results()$clusters)
      vars_used <- input$varsHclust
    } else if(input$methodProfile == "kmeans" && !is.null(kmeans_results())) {
      clusters <- factor(kmeans_results()$km$cluster)
      vars_used <- input$varsKmeans
    } else {
      return(NULL)
    }
    
    # Crear data frame temporal con clusters
    df_anova <- data() %>%
      select(all_of(vars_used)) %>%
      mutate(cluster = clusters)
    
    # Realizar ANOVA para cada variable
    anova_results <- lapply(vars_used, function(var) {
      formula <- as.formula(paste(var, "~ cluster"))
      aov_result <- aov(formula, data = df_anova)
      summary_aov <- summary(aov_result)[[1]]
      
      data.frame(
        Variable = var,
        `F-value` = round(summary_aov$`F value`[1], 3),
        `p-value` = round(summary_aov$`Pr(>F)`[1], 6),
        Significancia = ifelse(summary_aov$`Pr(>F)`[1] < 0.001, "***",
                               ifelse(summary_aov$`Pr(>F)`[1] < 0.01, "**",
                                      ifelse(summary_aov$`Pr(>F)`[1] < 0.05, "*", "NS")))
      )
    })
    
    anova_df <- do.call(rbind, anova_results)
    
    datatable(anova_df, 
              options = list(pageLength = 10, dom = 't'), 
              rownames = FALSE) %>%
      formatStyle("p-value",
                  backgroundColor = styleInterval(c(0.05), c("lightgreen", "white")))
  })
  
  output$radarPlot <- renderPlot({
    req(input$methodProfile)
    
    if(input$methodProfile == "hclust" && !is.null(hclust_results())) {
      clusters <- hclust_results()$clusters
      vars_used <- input$varsHclust
    } else if(input$methodProfile == "kmeans" && !is.null(kmeans_results())) {
      clusters <- kmeans_results()$km$cluster
      vars_used <- input$varsKmeans
    } else {
      return(NULL)
    }
    
    # Calcular medias por cluster
    df_profile <- data() %>%
      select(all_of(vars_used)) %>%
      mutate(Cluster = clusters) %>%
      group_by(Cluster) %>%
      summarise_all(mean)
    
    # Normalizar a escala 0-1 para el radar
    df_radar <- df_profile %>%
      select(-Cluster) %>%
      apply(2, function(x) (x - min(x)) / (max(x) - min(x))) %>%
      as.data.frame() %>%
      mutate(Cluster = df_profile$Cluster)
    
    # Preparar para radar plot
    
    # Añadir filas de max y min
    max_min <- data.frame(
      rbind(rep(1, ncol(df_radar) - 1),
            rep(0, ncol(df_radar) - 1))
    )
    names(max_min) <- names(df_radar)[-ncol(df_radar)]
    
    # Crear gráfico radar para cada cluster
    par(mfrow = c(2, ceiling(length(unique(clusters))/2)), mar = c(2, 2, 2, 2))
    colors <- brewer.pal(n = max(3, length(unique(clusters))), name = "Set1")
    
    for(i in unique(clusters)) {
      radar_data <- rbind(max_min, df_radar[df_radar$Cluster == i, -ncol(df_radar)])
      radarchart(radar_data,
                 axistype = 1,
                 pcol = colors[i],
                 pfcol = scales::alpha(colors[i], 0.3),
                 plwd = 2,
                 cglcol = "grey",
                 cglty = 1,
                 axislabcol = "grey",
                 caxislabels = seq(0, 1, 0.25),
                 cglwd = 0.8,
                 vlcex = 0.8,
                 title = paste("Cluster", i))
    }
  })
  
  # Boxplot individual
  output$boxplotProfile <- renderPlotly({
    req(input$methodProfile, input$varBoxplot)
    
    if(input$methodProfile == "hclust" && !is.null(hclust_results())) {
      clusters <- hclust_results()$clusters
    } else if(input$methodProfile == "kmeans" && !is.null(kmeans_results())) {
      clusters <- kmeans_results()$km$cluster
    } else {
      return(NULL)
    }
    
    df_box <- data() %>%
      mutate(Cluster = factor(clusters))
    
    # Colores consistentes
    colors <- c("#E41A1C", "#ADD8E6", "#4DAF4A")
    
    p <- ggplot(df_box, aes_string(x = "Cluster", y = input$varBoxplot, fill = "Cluster")) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      labs(title = paste("Distribución de", input$varBoxplot, "por Cluster"),
           x = "Cluster",
           y = input$varBoxplot) +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.position = "none")
    
    ggplotly(p)
  })
  
  # Boxplots simples (estilo método 1 del código)
  output$simpleBoxplots <- renderPlot({
    req(input$methodProfile)
    
    if(input$methodProfile == "hclust" && !is.null(hclust_results())) {
      clusters <- hclust_results()$clusters
    } else if(input$methodProfile == "kmeans" && !is.null(kmeans_results())) {
      clusters <- kmeans_results()$km$cluster
    } else {
      return(NULL)
    }
    
    # Guardar parámetros gráficos originales
    old_par <- par(mfrow = c(2, 3))
    
    # Colores consistentes con el código original
    box_colors <- c("red", "lightblue1", "green")
    
    # Crear boxplots individuales
    boxplot(data()$freq_visitas ~ clusters, 
            col = box_colors, 
            xlab = "Cluster", 
            ylab = "Frecuencia visitas",
            main = "Frecuencia de Visitas")
    
    boxplot(data()$ticket_promedio ~ clusters, 
            col = box_colors,
            xlab = "Cluster", 
            ylab = "Ticket promedio",
            main = "Ticket Promedio")
    
    boxplot(data()$diversidad_categorias ~ clusters, 
            col = box_colors,
            xlab = "Cluster", 
            ylab = "Diversidad categorias",
            main = "Diversidad de categorias")
    
    boxplot(data()$distancia_tienda ~ clusters, 
            col = box_colors,
            xlab = "Cluster", 
            ylab = "Distancia tienda",
            main = "Distancia a Tienda")
    
    boxplot(data()$edad ~ clusters, 
            col = box_colors,
            xlab = "Cluster", 
            ylab = "Edad",
            main = "Edad")
    
    # Restaurar parámetros gráficos
    par(old_par)
  })
  
  # Boxplot múltiple (estilo método 2 del código)
  output$multiBoxplot <- renderPlotly({
    req(input$methodProfile)
    
    if(input$methodProfile == "hclust" && !is.null(hclust_results())) {
      clusters <- hclust_results()$clusters
      vars_used <- input$varsHclust
    } else if(input$methodProfile == "kmeans" && !is.null(kmeans_results())) {
      clusters <- kmeans_results()$km$cluster
      vars_used <- input$varsKmeans
    } else {
      return(NULL)
    }
    
    # Preparar datos en formato largo
    df_long <- data() %>%
      select(all_of(vars_used)) %>%
      mutate(Cluster = factor(clusters)) %>%
      pivot_longer(cols = -Cluster, names_to = "variable", values_to = "valor")
    
    # Colores consistentes
    colors <- c("#E41A1C", "#ADD8E6", "#4DAF4A")
    
    p <- ggplot(df_long, aes(x = Cluster, y = valor, fill = Cluster)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.25) +
      facet_wrap(~ variable, scales = "free_y", ncol = 3) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      labs(title = paste("Perfil numérico por cluster -", 
                         ifelse(input$methodProfile == "hclust", "Jerárquico", "K-means")),
           x = "Cluster",
           y = NULL) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            strip.text = element_text(size = 11, face = "bold"),
            legend.position = "none")
    
    ggplotly(p)
  })
  
  output$clusterProfile <- renderDT({
    req(input$methodProfile)
    
    if(input$methodProfile == "hclust" && !is.null(hclust_results())) {
      clusters <- hclust_results()$clusters
    } else if(input$methodProfile == "kmeans" && !is.null(kmeans_results())) {
      clusters <- kmeans_results()$km$cluster
    } else {
      return(NULL)
    }
    
    # Crear perfil detallado
    profile_df <- data() %>%
      mutate(Cluster = clusters) %>%
      group_by(Cluster) %>%
      summarise(
        `Tamaño` = n(),
        `Edad Promedio` = round(mean(edad), 1),
        `Freq. Visitas` = round(mean(freq_visitas), 2),
        `Ticket Promedio` = paste0("$", round(mean(ticket_promedio), 2)),
        `Diversidad categorias` = round(mean(diversidad_categorias), 1),
        `% Usa App` = paste0(round(mean(uso_app) * 100, 1), "%"),
        `Distancia Tienda (km)` = round(mean(distancia_tienda), 2)
      ) %>%
      arrange(Cluster)
    
    # Agregar una fila de totales/promedios
    total_row <- data() %>%
      mutate(Cluster = clusters) %>%
      summarise(
        Cluster = "TOTAL/PROMEDIO",
        `Tamaño` = n(),
        `Edad Promedio` = round(mean(edad), 1),
        `Freq. Visitas` = round(mean(freq_visitas), 2),
        `Ticket Promedio` = paste0("$", round(mean(ticket_promedio), 2)),
        `Diversidad categorias` = round(mean(diversidad_categorias), 1),
        `% Usa App` = paste0(round(mean(uso_app) * 100, 1), "%"),
        `Distancia Tienda (km)` = round(mean(distancia_tienda), 2)
      )
    
    profile_df <- bind_rows(profile_df, total_row)
    
    datatable(profile_df, 
              options = list(pageLength = 10, dom = 't'), 
              rownames = FALSE) %>%
      formatStyle(
        'Cluster',
        target = 'row',
        backgroundColor = styleEqual("TOTAL/PROMEDIO", "#f0f0f0"),
        fontWeight = styleEqual("TOTAL/PROMEDIO", "bold")
      )
  })
}
