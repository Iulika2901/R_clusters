# install.packages("shiny")
# install.packages("bslib")
# install.packages("igraph")

library(shiny)
library(bslib)
library(igraph)

ui <- page_sidebar(
  title = "K-Means pe Graf",
  
  sidebar = sidebar(
    numericInput("k", "Număr de clustere (k):", min = 1, max = 10, value = 3),
    fileInput("file", "Alege fișierul cu matricea de adiacență:", 
              accept = c(".csv", ".R", ".RData", ".rds")),
    actionButton("run", "Rulează K-means")
  ),
  
  mainPanel(
    plotOutput("graphPlot"),
    tableOutput("clusteredTable")
  )
)

server <- function(input, output, session) {
  
  data_reactive <- reactive({
    infile <- input$file
    if (is.null(infile)) {
      if (file.exists("adjacency.csv")) {
        read.csv("adjacency.csv", row.names = 1)
      } else {
        stop("Nu s-a încărcat fișier și nu există 'adjacency.csv' în folderul curent.")
      }
    } else {
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
    }
  })
  
  clusters <- eventReactive(input$run, {
    adj <- data_reactive()
    
    # Verificăm dimensiunea
    if (!is.matrix(adj) && !is.data.frame(adj)) stop("Datele trebuie să fie matrice/pseudo-matrice.")
    adj <- as.matrix(adj)
    
    if (nrow(adj) < 2) {
      showNotification("⚠️ Matricea de adiacență are prea puține noduri!", type = "error")
      return(NULL)
    }
    
    g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    layout <- layout_with_fr(g)
    
    k <- input$k
    n <- nrow(layout)
    if (k > n) {
      showNotification(paste("⚠️ k =", k, "este mai mare decât numărul de noduri (", n, "). Ajustez automat."), 
                       type = "warning")
      k <- n
    }
    
    km <- kmeans(layout, centers = k)
    
    list(graph = g, layout = layout, km = km)
  })
  
  # Plot grafic
  output$graphPlot <- renderPlot({
    res <- clusters()
    req(!is.null(res))
    
    g <- res$graph
    layout <- res$layout
    km <- res$km
    
    # Plot noduri colorate
    plot(g, layout = layout, vertex.color = km$cluster, vertex.size = 15,
         vertex.label = NA, main = paste("K-Means pe Graf (k =", input$k, ")"))
    
    # Adaugă stele pentru centrele clusterelor
    points(km$centers, col = 1:input$k, pch = 8, cex = 2, lwd = 2)
  })
  
  # Tabel cu noduri + cluster
  output$clusteredTable <- renderTable({
    res <- clusters()
    req(!is.null(res))
    data.frame(Node = rownames(res$graph[]), Cluster = res$km$cluster)
  })
  
}

shinyApp(ui, server)
