# Instalează pachete dacă nu sunt instalate
# install.packages("igraph")
# install.packages("dplyr")
# install.packages("openxlsx")   
# install.packages("threejs")
# install.packages("zoom")
# install.packages("Matrix")
# install.packages("htmlwidgets")
# install.packages("devtools")
# install.packages("visNetwork")
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ReactomePA")
# BiocManager::install("clusterProfiler")
# BiocManager::install("org.Hs.eg.db")
# BiocManager::install("gprofiler2")

# Încarcă librăriile necesare
library(igraph)
library(dplyr)
library(openxlsx)
library(threejs)
library(htmlwidgets)
library(zoom)
library(visNetwork)
library(ReactomePA)
library(clusterProfiler)
library(org.Hs.eg.db)
library(gprofiler2)
library(Matrix) 

# Încarcă matricea A din fișier
load(file.choose())
A <- get("work_mat2")

# Funcția ta de atașare a mapării
attach.entregene_gen <- function (x, map, x_name, map_name, map_target) {
  print(paste(x_name, map_name, map_target))
  output <- merge(x, map, by.x=as.character(x_name), by.y=as.character(map_name), all.x=TRUE, sort=FALSE)
  output <- output[!is.na(output$map_gprofiler.target), ]
  return(output)
}

# 1. Extragi numele coloanelor
col_names <- colnames(A)

# 2. Obții maparea la Entrez Gene IDs
map_gprofiler <- gconvert(query = col_names,
                          organism = "hsapiens",
                          target = "ENTREZGENE_ACC",
                          mthreshold = Inf,
                          filter_na = TRUE)
colnames(map_gprofiler)
colnames(map_gprofiler)[colnames(map_gprofiler) == "input"] <- "map_gprofiler.input"
colnames(map_gprofiler)[colnames(map_gprofiler) == "target"] <- "map_gprofiler.target"


# 3. Atașezi maparea la numele originale
mapped_cols <- attach.entregene_gen(
  x = data.frame(Original = col_names),
  map = map_gprofiler,
  x_name = "Original",
  map_name = "map_gprofiler.input",
  map_target = "map_gprofiler.target"
)

# 4. Vector cu noile nume (Entrez IDs)
new_col_names <- mapped_cols$map_gprofiler.target

# 5. Creezi matricea nouă cu numele de coloane înlocuite
# faci un dicționar între numele originale și cele noi
name_dict <- setNames(mapped_cols$map_gprofiler.target, mapped_cols$Original)

# creezi vectorul complet de nume (în ordinea lui A)
new_col_names <- name_dict[colnames(A)]

# pui la cele fără mapare numele original
new_col_names[is.na(new_col_names)] <- colnames(A)[is.na(new_col_names)]

# creezi matricea finală
A_mapped <- A
colnames(A_mapped) <- new_col_names
print(new_col_names)
print(col_names)

genelist <- colnames(A_mapped)
genelist <- genelist[!duplicated(genelist)]

gostres2 <- gost(query = genelist, organism = "hsapiens", ordered_query = FALSE,
                 multi_query = FALSE,
                 significant = TRUE,   #set false if you want all pathways!
                 exclude_iea = FALSE,
                 measure_underrepresentation = FALSE,
                 evcodes = TRUE,
                 user_threshold = 0.05,
                 correction_method = "fdr",
                 domain_scope = "annotated", 
                 custom_bg = NULL,
                 numeric_ns = "",
                 sources = NULL,
                 as_short_link = FALSE)

#plot the resulting pathway
p <- gostplot(gostres2, capped = FALSE, interactive = FALSE)
p

head(gostres2$result)   # primele linii
View(gostres2$result) 

# 6.1 Extragem functionalitatile si genele implicate
func_list <- dplyr::select(gostres2$result, term_name, intersection)


# 6.2 Construim dicționar protein -> functionalitati
protein_funcs <- list()
for (i in 1:nrow(func_list)) {
  genes <- unlist(func_list$intersection[i])
  for (g in genes) {
    if (!g %in% names(protein_funcs)) protein_funcs[[g]] <- c()
    protein_funcs[[g]] <- unique(c(protein_funcs[[g]], func_list$term_name[i]))
  }
}

# 6.3 Lista de proteine
proteins <- names(protein_funcs)
n <- length(proteins)

# 6.4 Cream matricea de similaritate
sim_matrix <- matrix(0, nrow = n, ncol = n)
rownames(sim_matrix) <- proteins
colnames(sim_matrix) <- proteins

# 6.5 Populam matricea cu nr de functionalitati comune
for (i in 1:n) {
  for (j in i:n) {  # matrice simetrica
    common_funcs <- length(intersect(protein_funcs[[proteins[i]]], protein_funcs[[proteins[j]]]))
    sim_matrix[i,j] <- common_funcs
    sim_matrix[j,i] <- common_funcs
  }
}

# 6.6 Verificare
sim_matrix[1:5, 1:5]

#vizualizare retea cu igraph
g <- graph_from_adjacency_matrix(sim_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
plot(g, vertex.label = V(g)$name, edge.width = E(g)$weight/5)


