<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">

</head>
<body>

<div class="header">
    <h1>Graph Bio-Clustering Tool</h1>
    <p>Advanced Shiny Platform for Network Analysis & Functional Biology</p>
    <div class="badge">R v4.2+</div>
    <div class="badge">Shiny Ecosystem</div>
    <div class="badge">Interactive 3D</div>
    <div class="badge">Bioinformatics</div>
</div>

<p>A sophisticated analytical tool built in R, designed to bridge the gap between raw adjacency matrices and biological insights. It supports asynchronous processing to ensure a smooth user experience even with complex genomic computations.</p>

<h2>🚀 Key Workflows</h2>
<div class="grid">
    <div class="card">
        <h3>Simple Undirected</h3>
        <p>Classic network analysis. Upload any symmetric adjacency matrix and detect structural communities using optimized algorithms like <strong>Louvain</strong> or <strong>Fast Greedy</strong>.</p>
    </div>
    <div class="card">
        <h3>Biological Directed</h3>
        <p>Translates gene symbols into functional networks. Uses <strong>gProfiler2</strong> to map Entrez IDs and builds directed graphs based on shared biological pathways.</p>
    </div>
</div>

<h2>🛠️ Core Features</h2>
<ul>
    <li><strong>Asynchronous Computation:</strong> Powered by <code>future</code> and <code>promises</code> to keep the UI responsive during heavy lifting.</li>
    <li><strong>Interactive 3D Visualization:</strong> Rotate, zoom, and explore clusters in a three-dimensional space using <code>threejs</code>.</li>
    <li><strong>Cluster Evaluation:</strong> Compare different methods using the <strong>Adjusted Rand Index (ARI)</strong> matrix.</li>
    <li><strong>Data Export:</strong> One-click download of node-cluster assignments in CSV format.</li>
</ul>

<h2>📦 Requirements</h2>
<p>To run this application locally, ensure you have the following packages installed:</p>
<div class="code-block">
<code>install.packages(c("shiny", "bslib", "igraph", "mclust", "ggplot2", "shinybusy", "promises", "future", "threejs", "gprofiler2", "dplyr"))</code>
</div>

<h2>📖 How to Use</h2>
<ol>
    <li><strong>Select Workflow:</strong> Choose <em>Simple</em> for topology or <em>Biological</em> for gene-enrichment based graphs.</li>
    <li><strong>Data Input:</strong> Upload your file (CSV, RData, or RDS). For biological mode, ensure column names are <strong>Gene Symbols</strong> (e.g., TP53, BRCA1).</li>
    <li><strong>Compute:</strong> Click on any clustering button. A spinner will appear while the parallel process completes.</li>
    <li><strong>Analyze & Export:</strong> Switch between 2D, 3D, and Table views. Use the <em>Download</em> button to save your results.</li>
</ol>



</body>
</html>
