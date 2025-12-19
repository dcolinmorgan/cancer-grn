# CancerGRN

## Overview

**CancerGRN** is an interactive R Shiny application designed for visualizing and analyzing Gene Regulatory Networks (GRNs). It focuses on networks inferred using perturbation-based methods to unravel oncogenic mechanisms, specifically highlighting MYC signatures.

The application allows users to explore networks generated via different inference methods (LASSO, LSCO, TLSCO), visualize regulatory interactions, and analyze network stability and similarity.

## Features

- **Interactive Network Visualization**:
  - **CytoscapeJS**: Explore network topology with interactive nodes and edges. Blue links represent upregulation (stimulation), while red links represent downregulation (inhibition).
  - **Force Directed Graph**: A D3.js-based force-directed layout where node size reflects betweenness centrality and color indicates edge weights.
  
- **Statistical Analysis**:
  - **Overlap Analysis**: Visualize bootstrap support ranges and overlap between inferred networks for measured vs. shuffled data, helping to estimate False Discovery Rates (FDR).
  - **Jaccard Similarity**: View dendrograms clustering networks based on Jaccard similarity indices.

- **Data Management**:
  - **Pre-loaded Datasets**: Access built-in datasets for LASSO, LSCO, and TLSCO inference methods.
  - **Custom Uploads**: Upload your own network edge lists and overlap tables for visualization.
  - **Export**: Download processed network data (CSV/TSV) and capture high-quality PNG screenshots of interactive plots.

## Installation

To run this application locally, you need R installed along with the following packages:

```r
install.packages(c(
  "shiny", 
  "igraph", 
  "zoo", 
  "dplyr", 
  "gtools", 
  "networkD3", 
  "pracma", 
  "network", 
  "reshape2", 
  "plotly", 
  "radarchart", 
  "visNetwork", 
  "shinyscreenshot"
))

# cyjShiny is available via Bioconductor
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("cyjShiny")
```

## Usage

1. Clone this repository:
   ```bash
   git clone https://github.com/dcolinmorgan/cancer-grn.git
   ```

2. Open the project in RStudio or navigate to the directory in your R console.

3. Run the application:
   ```r
   shiny::runApp()
   ```

## Data Format for Uploads

If uploading your own data:
- **Network File**: A CSV or TSV file (no header) representing the edge list. Expected columns: `Source`, `Target`, `Link1`, `Link2`, `Sign`, `Weight`.
- **Overlap File**: A CSV or TSV file used for the Overlap tab visualization.

## References

- **Publication**: [Perturbation-based gene regulatory network inference to unravel oncogenic mechanisms](https://www.nature.com/articles/s41598-020-70941-y)
- **Raw Data**: [NCBI GEO GSE125958](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE125958)
- **Python Implementation**: [GeneSPIDER (pyGS)](https://github.com/dcolinmorgan/pyGS)

## Author

**Daniel Colin Morgan**
- [GitHub](https://github.com/dcolinmorgan)
- [Twitter/X](https://x.com/dcolinmorgan/)