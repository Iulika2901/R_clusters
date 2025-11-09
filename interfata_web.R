# install.packages("shiny")
# install.packages("bslib")
# install.packages("igraph")

library(shiny)
library(bslib)
library(igraph)

ui <- page_sidebar(
  title = "Graph Clustering Demo",
  
  sidebar = sidebar(
    fileInput("file", "Alege fișierul cu matricea de adiacență:", 
              accept = c(".csv", ".R", ".RData", ".rds")),
    numericInput("k", "Număr de clustere (pentru k-means):", min = 1, max = 10, value = 3),
    actionButton("btn_kmeans", "K-Means"),
    actionButton("btn_fastgreedy", "Fast Greedy"),
    actionButton("btn_louvain", "Louvain"),
    actionButton("btn_walktrap", "Walktrap"),
    actionButton("btn_label", "Label Propagation"),
    actionButton("btn_infomap", "Infomap")
  ),
  
  mainPanel(
    plotOutput("graphPlot"),
    tableOutput("clusteredTable")
  )
)

server <- function(input, output, session) {
  
 
  data_reactive <- reactive({
    infile <- input$file
    req(infile)
    
    ext <- tools::file_ext(infile$name)
    if (ext == "csv") read.csv(infile$datapath, row.names = 1)
    else if (ext == "rds") readRDS(infile$datapath)
    else if (ext %in% c("R", "r")) {
      source(infile$datapath, local = TRUE)
      df <- Filter(is.data.frame, mget(ls(), inherits = TRUE))
      if (length(df) > 0) df[[1]] else stop("Fișierul .R nu conține un data.frame!")
    } 
    else if (ext == "RData") {
      temp_env <- new.env()
      load(infile$datapath, envir = temp_env)
      objs <- mget(ls(temp_env), envir = temp_env)
      df <- NULL
      for (o in objs) {
        if (is.data.frame(o) || is.matrix(o)) {
          df <- as.data.frame(o)
          break
        }
      }
      if (is.null(df)) stop("Fișierul .RData nu conține un data.frame sau matrice!")
      df
    }
  })
  

  run_clustering <- function(g, method, k = 3) {
    if (method == "kmeans") {
      layout <- layout_with_fr(g)
      km <- kmeans(layout, centers = k)
      list(layout = layout, cluster = km$cluster, centers = km$centers)
    } else {
    
      comm <- switch(method,
                     fastgreedy = cluster_fast_greedy(g),
                     louvain = cluster_louvain(g),
                     walktrap = cluster_walktrap(g),
                     label = cluster_label_prop(g),
                     infomap = cluster_infomap(g))
      cluster <- membership(comm)
      layout <- layout_with_fr(g)
      list(layout = layout, cluster = cluster, centers = NULL)
    }
  }
  

  clustering_res <- reactiveVal(NULL)
  

  observeEvent(input$btn_kmeans, {
    adj <- as.matrix(data_reactive())
    g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    res <- run_clustering(g, "kmeans", input$k)
    res$graph <- g
    clustering_res(res)
  })
  
  observeEvent(input$btn_fastgreedy, {
    adj <- as.matrix(data_reactive())
    g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    res <- run_clustering(g, "fastgreedy")
    res$graph <- g
    clustering_res(res)
  })
  
  observeEvent(input$btn_louvain, {
    adj <- as.matrix(data_reactive())
    g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    res <- run_clustering(g, "louvain")
    res$graph <- g
    clustering_res(res)
  })
  
  observeEvent(input$btn_walktrap, {
    adj <- as.matrix(data_reactive())
    g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    res <- run_clustering(g, "walktrap")
    res$graph <- g
    clustering_res(res)
  })
  
  observeEvent(input$btn_label, {
    adj <- as.matrix(data_reactive())
    g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    res <- run_clustering(g, "label")
    res$graph <- g
    clustering_res(res)
  })
  
  observeEvent(input$btn_infomap, {
    adj <- as.matrix(data_reactive())
    g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    res <- run_clustering(g, "infomap")
    res$graph <- g
    clustering_res(res)
  })
  

  plot_graph <- function(g, layout, cluster, centers = NULL, k = 3) {
    plot(g, layout = layout, vertex.color = cluster, vertex.size = 15,
         vertex.label = NA, main = paste("Graph Clustering"))
    if (!is.null(centers)) points(centers, col = 1:k, pch = 8, cex = 2, lwd = 2)
  }
  

  output$graphPlot <- renderPlot({
    res <- clustering_res()
    req(!is.null(res))
    plot_graph(res$graph, res$layout, res$cluster, res$centers, input$k)
  })
  

  output$clusteredTable <- renderTable({
    res <- clustering_res()
    req(!is.null(res))
    data.frame(Node = rownames(res$graph[]), Cluster = res$cluster)
  })
  
}

shinyApp(ui, server)
