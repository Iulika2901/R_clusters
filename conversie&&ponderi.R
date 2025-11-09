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

load(file.choose())
A <- get("work_mat2")


attach.entregene_gen <- function (x, map, x_name, map_name, map_target) {
  print(paste(x_name, map_name, map_target))
  output <- merge(x, map, by.x=as.character(x_name), by.y=as.character(map_name), all.x=TRUE, sort=FALSE)
  output <- output[!is.na(output$map_gprofiler.target), ]
  return(output)
}

col_names <- colnames(A)
map_gprofiler <- gconvert(query = col_names,
                          organism = "hsapiens",
                          target = "ENTREZGENE_ACC",
                          mthreshold = Inf,
                          filter_na = TRUE)
colnames(map_gprofiler)
colnames(map_gprofiler)[colnames(map_gprofiler) == "input"] <- "map_gprofiler.input"
colnames(map_gprofiler)[colnames(map_gprofiler) == "target"] <- "map_gprofiler.target"


mapped_cols <- attach.entregene_gen(
  x = data.frame(Original = col_names),
  map = map_gprofiler,
  x_name = "Original",
  map_name = "map_gprofiler.input",
  map_target = "map_gprofiler.target"
)
new_col_names <- mapped_cols$map_gprofiler.target
name_dict <- setNames(mapped_cols$map_gprofiler.target, mapped_cols$Original)
new_col_names <- name_dict[colnames(A)]
new_col_names[is.na(new_col_names)] <- colnames(A)[is.na(new_col_names)]
A_mapped <- A
colnames(A_mapped) <- new_col_names
print(new_col_names)
print(col_names)

genelist <- colnames(A_mapped)
genelist <- genelist[!duplicated(genelist)]
gostres2 <- gost(query = genelist, organism = "hsapiens", ordered_query = FALSE,
                 multi_query = FALSE,
                 significant = TRUE,   
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
func_list <- dplyr::select(gostres2$result, term_name, intersection)
protein_funcs <- list()
for (i in 1:nrow(func_list)) {
  genes <- unlist(func_list$intersection[i])
  for (g in genes) {
    if (!g %in% names(protein_funcs)) protein_funcs[[g]] <- c()
    protein_funcs[[g]] <- unique(c(protein_funcs[[g]], func_list$term_name[i]))
  }
}


proteins <- names(protein_funcs)
n <- length(proteins)

#matricea de similaritate + funct comune
sim_matrix <- matrix(0, nrow = n, ncol = n)
rownames(sim_matrix) <- proteins
colnames(sim_matrix) <- proteins
for (i in 1:n) {
  for (j in i:n) {  
    common_funcs <- length(intersect(protein_funcs[[proteins[i]]], protein_funcs[[proteins[j]]]))
    sim_matrix[i,j] <- common_funcs
    sim_matrix[j,i] <- common_funcs
  }
}


sim_matrix[1:5, 1:5]
g <- graph_from_adjacency_matrix(sim_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
plot(g, vertex.label = V(g)$name, edge.width = E(g)$weight/5)



