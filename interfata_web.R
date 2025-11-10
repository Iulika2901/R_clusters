library(shiny)
library(bslib)
library(igraph)
library(mclust)
library(ggplot2)
library(shinybusy)
library(promises)
library(future)

plan(multisession) # Asincron


append_log <- function(msg, log_reactive) {
  isolate({
    log <- log_reactive()
    log_reactive(c(log, paste(Sys.time(), "-", msg)))
  })
}

ui <- page_sidebar(
  title = "Graph Clustering Demo",
  
  sidebar = sidebar(
    fileInput("file", "Choose file:", 
              accept = c(".csv", ".R", ".RData", ".rds")),
    
    selectInput("select", "Select options:", 
                list("Apply a cluster method on a graph" = "cluster", 
                     "Correlation Matrix" = "correlation")),
    
    
    conditionalPanel(
      condition = "input.select == 'cluster'",
      numericInput("k", "Număr de clustere (pentru k-means):", min = 1, max = 10, value = 3),
      actionButton("btn_kmeans", "K-Means"),
      actionButton("btn_fastgreedy", "Fast Greedy"),
      actionButton("btn_louvain", "Louvain"),
      actionButton("btn_walktrap", "Walktrap"),
      actionButton("btn_label", "Label Propagation"),
      actionButton("btn_infomap", "Infomap"),
      hr(),
      h4("Log metode:"),
      verbatimTextOutput("log")
    ),
   
    conditionalPanel(
      condition = "input.select == 'correlation'",
      checkboxGroupInput("checkbox_group", "Select clustering methods:", 
                         c("K-Means" = "km", "Fast Greedy" = "fg", 
                           "Louvain" = "l", "Walktrap" = "w", 
                           "Label Propagation" = "lp", "Infomap" = "i")),
      actionButton("show_results", "Show Results")
    )
  ),
  
  mainPanel(
    conditionalPanel(condition = "input.select == 'cluster'",
                     plotOutput("graphPlot"), tableOutput("clusteredTable")),
    conditionalPanel(condition = "input.select == 'correlation'",
                     plotOutput("corrPlot"), tableOutput("corrTable"))
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
      if (length(df) > 0) df[[1]] else stop(".R file nu conține data.frame")
    } else if (ext == "RData") {
      temp_env <- new.env()
      load(infile$datapath, envir = temp_env)
      objs <- mget(ls(temp_env), envir = temp_env)
      df <- NULL
      for (o in objs) if (is.data.frame(o) || is.matrix(o)) { df <- as.data.frame(o); break }
      if (is.null(df)) stop(".RData nu conține data.frame sau matrice")
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
  all_results <- reactiveValues()
  log_msgs <- reactiveVal(character())
  
  methods <- list(
    btn_kmeans = "kmeans", btn_fastgreedy = "fastgreedy",
    btn_louvain = "louvain", btn_walktrap = "walktrap",
    btn_label = "label", btn_infomap = "infomap"
  )
  
  output$log <- renderText({
    paste(log_msgs(), collapse = "\n")
  })
  
  # Observere pentru toate metodele, in mod asincron
  lapply(names(methods), function(btn) {
    observeEvent(input[[btn]], {
      req(data_reactive())
      adj <- as.matrix(data_reactive())
      g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
      
      append_log(paste("Start", methods[[btn]]), log_msgs)
      
      future({
        run_clustering(g, methods[[btn]], input$k)
      }) %...>% (function(res) {
        res$graph <- g
        clustering_res(res)
        all_results[[methods[[btn]]]] <- res$cluster
        append_log(paste("Finalizat", methods[[btn]]), log_msgs)
      }) %...!% (function(e){
        append_log(paste("Eroare", methods[[btn]], ":", e$message), log_msgs)
      })
    })
  })
  
  plot_graph <- function(g, layout, cluster, centers = NULL, k = 3) {
    plot(g, layout = layout, vertex.color = cluster, vertex.size = 15,
         vertex.label = NA, main = "Graph Clustering")
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
  
  # correlation matrix
  observeEvent(input$show_results, {
    req(input$checkbox_group)
    clusters_selected <- list()
    not_run <- c()
    
    for (m in input$checkbox_group) {
      full_name <- switch(m,
                          km = "kmeans", fg = "fastgreedy",
                          l = "louvain", w = "walktrap",
                          lp = "label", i = "infomap")
      val <- all_results[[full_name]]
      if (!is.null(val)) clusters_selected[[full_name]] <- val
      else not_run <- c(not_run, full_name)
    }
    
    if (length(clusters_selected) < 2) {
      showNotification(paste("First run your methods in the section 'Apply a cluster method...'", paste(not_run, collapse=", ")), type="warning")
      return()
    }
    
    show_modal_spinner("circle", "Calculate ARI...")
    
    future({
      method_names <- names(clusters_selected)
      n <- length(method_names)
      corr_mat <- matrix(0, nrow = n, ncol = n, dimnames = list(method_names, method_names))
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          corr_mat[i,j] <- adjustedRandIndex(clusters_selected[[i]], clusters_selected[[j]])
        }
      }
      corr_mat
    }) %...>% (function(corr_mat) {
      corr_df <- as.data.frame(as.table(corr_mat))
      names(corr_df) <- c("Method1","Method2","ARI")
      
      output$corrPlot <- renderPlot({
        ggplot(corr_df, aes(x=Method1, y=Method2, fill=ARI)) +
          geom_tile(color="white") +
          scale_fill_gradient(low="lightblue", high="darkblue") +
          geom_text(aes(label=sprintf("%.2f", ARI)), color="white", size=4) +
          theme_minimal(base_size=14) +
          labs(title="Adjusted Rand Index între metode", x="", y="")
      })
      output$corrTable <- renderTable(round(corr_mat,3), rownames=TRUE)
      
      remove_modal_spinner()
      showNotification("ARI matrix run succesfully!", type="message")
    }) %...!% (function(e){
      remove_modal_spinner()
      showNotification(paste("Error at ARI:", e$message), type="error")
    })
  })
}

shinyApp(ui, server)
