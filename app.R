#### Load necessary packages ####
library(shiny)
library(igraph)
library(zoo)
library(dplyr)
library(gtools)
library(cyjShiny)
library(networkD3)
library(pracma)
library(network)
library(reshape2)
library(plotly)
library(radarchart)
library(visNetwork)
library(shinyscreenshot)

# Optional: Load graph if available (Bioconductor dependency often used with graph structures)
if (requireNamespace("graph", quietly = TRUE)) {
  library(graph)
}

#### Data Loading ####
# Load all RData files into the global environment
# We wrap this in a try-catch or check existence to avoid errors if files are missing during dev
rdata_files <- c(
  "lassodataMYCsign2017-11-08.rdata",
  "lscodataMYCsign2017-11-08.rdata",
  "tlscodataMYCsign2019-02-14.rdata",
  "lassoMYCsignlinkplotdata2017-02-08.rdata",
  "lscoMYCsignlinkplotdata2017-02-14.rdata",
  "tlscoMYCsignlinkplotdata2019-02-14.rdata"
)

for (file in rdata_files) {
  if (file.exists(file)) {
    load(file)
  }
}

# Organize global data into structured lists for easier access
# We use get() to retrieve variables by name if they exist, handling the dynamic loading
get_if_exists <- function(var_name) {
  if (exists(var_name)) get(var_name) else NULL
}

network_data_map <- list(
  lasso = get_if_exists("lassodataMYCsign"),
  lsco = get_if_exists("lscodataMYCsign"),
  tlsco = get_if_exists("tlscodataMYCsign")
)

plot_data_map <- list(
  lasso = get_if_exists("lassoMYCsignlinkplotdata"),
  lsco = get_if_exists("lscoMYCsignlinkplotdata"),
  tlsco = get_if_exists("tlscoMYCsignlinkplotdata")
)

network_names_map <- list(
  lasso = get_if_exists("lassonameMYCsign"),
  lsco = get_if_exists("lsconameMYCsign"),
  tlsco = get_if_exists("tlsconameMYCsign")
)

cutoff_data_map <- list(
  lasso = get_if_exists("lassoMYCsignlinkcutdata"),
  lsco = get_if_exists("lscoMYCsignlinkcutdata"),
  tlsco = get_if_exists("tlscoMYCsignlinkcutdata")
)

#### Network Operations Manager (OOP-style) ####
NetworkManager <- list(
  
  # Process a raw edge list dataframe into a standardized format
  process_edge_list = function(df, remove_self_loops = FALSE) {
    if (is.null(df) || ncol(df) < 6) return(NULL)
    
    # Subset and rename standard columns
    df_clean <- df[, 1:6]
    colnames(df_clean) <- c("source_name", "target_name", "link", "link2", "sign", "weight")
    
    # Ensure numeric columns are numeric to avoid "invalid type (character)" errors
    df_clean$link <- as.numeric(as.character(df_clean$link))
    df_clean$link2 <- as.numeric(as.character(df_clean$link2))
    df_clean$sign <- as.numeric(as.character(df_clean$sign))
    df_clean$weight <- as.numeric(as.character(df_clean$weight))
    
    # Filter self-loops if requested
    if (remove_self_loops) {
      df_clean <- df_clean %>% 
        filter(as.character(source_name) != as.character(target_name))
    }
    
    return(df_clean)
  },
  
  # Calculate Jaccard Similarity Matrix for a list of networks
  calculate_jaccard_matrix = function(network_list, remove_self_loops = FALSE) {
    n <- length(network_list)
    jaccard_mat <- matrix(0, n, n)
    
    # Pre-process all networks to ensure consistent format
    processed_nets <- lapply(network_list, function(net) {
      NetworkManager$process_edge_list(as.data.frame(net), remove_self_loops)
    })
    
    for (i in 1:n) {
      for (k in 1:n) {
        net1 <- processed_nets[[i]]
        net2 <- processed_nets[[k]]
        
        if (is.null(net1) || is.null(net2)) {
          jaccard_mat[i, k] <- 0
          next
        }
        
        # Create unique edge identifiers for comparison
        edges1 <- paste(net1$source_name, net1$target_name)
        edges2 <- paste(net2$source_name, net2$target_name)
        
        intersection_size <- length(intersect(edges1, edges2))
        union_size <- length(union(edges1, edges2))
        
        jaccard_mat[i, k] <- if (union_size > 0) intersection_size / union_size else 0
      }
    }
    return(jaccard_mat)
  }
)

#### Server ####
server <- function(input, output) {
  
  # Reactive: Get the current set of networks (either pre-loaded or uploaded)
  current_network_set <- reactive({
    if (input$raw == FALSE) {
      return(network_data_map[[input$data]])
    } else {
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      
      # Read uploaded files
      data_list <- lapply(rev(mixedsort(inFile$datapath)), read.csv, header = FALSE, sep = input$sep)
      names(data_list) <- rev(mixedsort(inFile$name))
      return(data_list)
    }
  })
  
  # Reactive: Get current filename base (without extension)
  current_filename_base <- reactive({
    fname <- NULL
    if (input$raw == FALSE) {
      name_list <- network_names_map[[input$data]]
      idx <- as.numeric(input$sparsity)
      if (!is.null(name_list) && idx <= length(name_list)) {
        fname <- name_list[[idx]]
      }
    } else {
      # Uploaded data
      data_set <- current_network_set()
      idx <- as.numeric(input$sparsity)
      if (!is.null(data_set) && idx <= length(data_set)) {
         fname <- names(data_set)[idx]
      }
    }
    
    if (!is.null(fname)) {
      return(sub("\\.[^.]*$", "", fname))
    } else {
      return(paste0("network_", input$data))
    }
  })
  
  # Reactive: Get the specific single network selected by sparsity/index
  selected_network_raw <- reactive({
    data_set <- current_network_set()
    if (is.null(data_set)) return(NULL)
    
    idx <- as.numeric(input$sparsity)
    # Safety check for index bounds
    if (idx > length(data_set)) idx <- length(data_set)
    if (idx < 1) idx <- 1
    
    return(data_set[[idx]])
  })
  
  # Reactive: Processed Edge List for the selected network
  processed_edge_list <- reactive({
    raw_data <- selected_network_raw()
    NetworkManager$process_edge_list(as.data.frame(raw_data), input$self == FALSE)
  })
  
  # Output: Table
  output$table <- renderTable({
    processed_edge_list()
  })
  
  # Output: Download Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(current_filename_base(), input$filetype, sep = ".")
    },
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      write.table(processed_edge_list(), file, sep = sep, row.names = FALSE)
    }
  )
  
  # Reactive: Force Network Object
  force_network_obj <- reactive({
    edge_list <- processed_edge_list()
    if (is.null(edge_list)) return(NULL)
    
    # Rename for D3 compatibility logic
    colnames(edge_list) <- c("SourceName", "TargetName", "Link", "Link2", "Sign", "Weight")
    
    gD <- igraph::simplify(igraph::graph.data.frame(edge_list, directed = TRUE))
    node_list <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)), nName = igraph::V(gD)$name)
    
    get_node_id <- function(x) { match(x, igraph::V(gD)$name) - 1 }
    
    edge_list_d3 <- edge_list %>%
      mutate(SourceID = get_node_id(SourceName),
             TargetID = get_node_id(TargetName))
    
    node_list$nodeDegree <- igraph::degree(gD, mode = "all")
    bet_all <- igraph::betweenness(gD, directed = FALSE)
    
    # Normalize betweenness for node size
    bet_norm <- if (max(bet_all) != min(bet_all)) {
      (bet_all - min(bet_all)) / (max(bet_all) - min(bet_all))
    } else {
      rep(0, length(bet_all))
    }
    node_list$nodeBetweenness <- 100 * bet_norm
    
    # Color mapping based on weights
    f2 <- colorRampPalette(c("#0000FF", "#FF0000"), space = "rgb", interpolate = "linear")
    col_codes <- f2(length(unique(edge_list$Weight)))
    edges_col <- sapply(edge_list$Weight, function(x) col_codes[which(sort(unique(edge_list$Weight)) == x)])
    
    networkD3::forceNetwork(
      Links = edge_list_d3,
      Nodes = node_list,
      Source = "SourceID",
      Target = "TargetID",
      Value = "Weight",
      NodeID = "nName",
      Nodesize = "nodeBetweenness",
      Group = "nodeDegree",
      height = 500,
      width = 1000,
      fontSize = 15,
      linkDistance = networkD3::JS("function(d) { return 100*d.value; }"),
      linkWidth = networkD3::JS("function(d) { return 2*d.value; }"),
      opacity = 0.85,
      zoom = TRUE,
      opacityNoHover = 0.1,
      legend = FALSE,
      arrows = TRUE,
      linkColour = edges_col,
      bounded = FALSE
    )
  })
  
  # Output: Force Network (D3)
  output$forcelasso <- renderForceNetwork({
    force_network_obj()
  })
  
  # Capture: Force Network
  observeEvent(input$shotForce, {
    screenshot(selector = "#forcelasso", filename = paste0("force_network_", current_filename_base()))
  })
  
  # Reactive: Cytoscape Object
  cyj_chart_obj <- reactive({
    edge_data <- processed_edge_list()
    if (is.null(edge_data)) return(NULL)
    
    # Prepare for Cytoscape - ensure all columns are character/numeric, not factors
    colnames(edge_data) <- c("sourceName", "targetName", "link1", "link2", "link", "weight")
    
    # Convert factors to characters and ensure numeric columns
    edge_data$sourceName <- as.character(edge_data$sourceName)
    edge_data$targetName <- as.character(edge_data$targetName)
    edge_data$link1 <- as.numeric(edge_data$link1)
    edge_data$weight <- as.numeric(edge_data$weight)
    
    edge_data$edgeTargetShape <- ifelse(edge_data$link1 == -1, "tee", 
                                        ifelse(edge_data$link1 == 1, "triangle", "triangle"))
    edge_data$color <- ifelse(edge_data$link1 == -1, "#FF0000", 
                              ifelse(edge_data$link1 == 1, "#0000FF", "#888888"))
    
    # Build graph
    gD <- igraph::simplify(igraph::graph.data.frame(edge_data[, c("sourceName", "targetName")], directed = TRUE))
    
    # Create node list
    node_names <- igraph::V(gD)$name
    node_data <- data.frame(
      id = node_names,
      name = node_names,
      stringsAsFactors = FALSE
    )
    
    # Calculate node properties
    node_data$nodeDegree <- igraph::degree(gD, mode = "all")
    node_data$rank <- if(max(node_data$nodeDegree) > 0) {
      node_data$nodeDegree / max(node_data$nodeDegree)
    } else {
      rep(0.5, nrow(node_data))
    }
    node_data$shape <- "ellipse"
    
    # Create edge list with proper node references
    edge_list <- data.frame(
      source = as.character(edge_data$sourceName),
      target = as.character(edge_data$targetName),
      weight = as.numeric(edge_data$weight),
      color = as.character(edge_data$color),
      targetShape = as.character(edge_data$edgeTargetShape),
      interaction = ifelse(edge_data$edgeTargetShape == "tee", "inhibit", "stimulate"),
      stringsAsFactors = FALSE
    )
    
    # Create the graph JSON
    graph_json <- toJSON(dataFramesToJSON(edge_list, node_data), auto_unbox = TRUE)
    
    # Define style directly as JSON string (matching user's request)
    cy_style_json <- '[
       {"selector":"node", "css": {
           "text-valign":"center",
           "text-halign":"center",
           "border-color": "black",
           "content": "data(name)",
           "border-width": "1px",
           "font-size":"10px",
           "width": "40px",
           "height": "40px"
           }},
        {"selector": "node[rank>0]", "css": {
            "background-color": "mapData(rank, 0, 1, white,cyan)"
        }},
        {"selector": "node:selected", "css": {
           "overlay-opacity": 0.3,
           "overlay-color": "gray"
        }},
        {"selector": "edge", "css": {
            "curve-style": "bezier"
        }},
        {"selector": "edge[interaction=\'stimulate\']", "css": {
            "line-color": "blue",
            "width": 1,
            "target-arrow-shape": "triangle",
            "target-arrow-color": "blue",
            "arrow-scale": 1
        }},
        {"selector": "edge[interaction=\'inhibit\']", "css": {
            "line-color": "red",
            "width": 1,
            "target-arrow-shape": "tee",
            "target-arrow-color": "red",
            "arrow-scale": 1
          }}
    ]'
    
    # Write style to a temporary file
    style_file <- tempfile(pattern = "cy_style", fileext = ".js")
    write(cy_style_json, style_file)
    
    cyjShiny(graph_json, layoutName = input$clay, styleFile = style_file)
  })
  
  # Output: Cytoscape (cyjShiny)
  output$cyjShiny <- renderCyjShiny({
    cyj_chart_obj()
  })
  
  # Capture: Cytoscape
  observeEvent(input$shotCyj, {
    screenshot(selector = "#cyjShiny", filename = paste0("cytoscape_network_", current_filename_base()))
  })
  
  # Reactive: Overlap Plot Object
  overlap_plot_obj <- reactive({
    if (input$raw == TRUE) {
      return(NULL)
    }
    if (input$data == "tlsco") {
      validate(need(FALSE, "Not available for TLSCO"))
    }
    
    idx <- as.numeric(input$sparsity)
    plot_data_raw <- plot_data_map[[input$data]][[idx]]
    cutoffs_raw <- cutoff_data_map[[input$data]][[idx]]
    
    if (is.null(plot_data_raw)) return(NULL)
    
    # Data Processing
    data_df <- as.data.frame(plot_data_raw)
    n_rows <- nrow(data_df)
    subset_idx <- 1:(n_rows - 3)
    
    # Calculate Overlap MYC (Column 11 logic from original code)
    val_col9 <- as.numeric(as.character(data_df[subset_idx, 9]))
    val_col6 <- as.numeric(as.character(data_df[subset_idx, 6]))
    data_df[subset_idx, 11] <- val_col9 + (max(val_col6) - max(val_col9))
    
    plot_df <- data_df[subset_idx, c(2, 3, 4, 6, 5)]
    colnames(plot_df) <- c("bins", "Data", "shuffle", "overlap", "overlap_shuffle")
    
    # Ensure all columns are numeric to avoid factor issues
    plot_df[] <- lapply(plot_df, function(x) as.numeric(as.character(x)))
    
    cutoff_df <- as.data.frame(cutoffs_raw)
    
    long <- melt(plot_df, id.vars = c("bins"))
    long$bins <- as.numeric(long$bins)
    long$value <- as.numeric(long$value)
    
    # Plotting Parameters
    # Replicating original logic for scaling and ranges
    ccc <- max(long$value)
    hh <- nrow(plot_df)
    
    # FDR Calculation
    a <- vector()
    b <- vector()
    
    # Normalize bins to 0-1 range
    max_bin <- max(long$bins, na.rm = TRUE)
    
    for (i in 1:hh) {
      a[i] <- trapz(long$bins[i:hh]/max_bin, long$value[i:hh]/max(long$value[i:hh]))
      b[i] <- trapz(long$bins[c(((hh+1)+i):(hh*2))]/max_bin, long$value[c(((hh+1)+i):(hh*2))]/max(long$value[c(i:(dim(plot_df)[1]))]))
    }
    
    fdr_x_vals <- c(1:(dim(plot_df)[1]))/(dim(plot_df)[1])
    fdr_y_vals <- na.locf(b/a)/3
    fdr_cutoff_x <- approx(x = fdr_y_vals, y = fdr_x_vals, xout = 0.05, rule = 2)$y
    
    # Simplified Plotly construction
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "Frequency",
      range = c(0, ccc),
      showline = FALSE,
      showgrid = FALSE
    )
    
    ff=10
    gg=1010
    
    plot_ly() %>%
      add_lines(x = long$bins[c(ff:(dim(plot_df)[1]))]/max_bin, y = (long$value[c(ff:(dim(plot_df)[1]))]/max(long$value[c(ff:(dim(plot_df)[1]))])), name = "Measured",line = list(color = 'rgb(22, 96, 167)')) %>%
      add_lines(x = long$bins[c(gg:(hh*2))]/max_bin, y = (long$value[c(gg:(hh*2))]/max(long$value[c(ff:(dim(plot_df)[1]))])), name = "Shuffled", line = list(color = 'rgb(205, 12, 24)'))%>%
      add_lines(x = long$bins[(1+hh*3):(hh*4)]/max_bin, y = (long$value[(1+hh*3):(hh*4)]/max(long$value[(1+hh*3):(hh*4)]))*ccc, name = "Overlap, Measured", yaxis = "y2",fill = 'tozeroy',line = list(color = 'rgba(55, 15, 255,0.1)'),fillcolor = list(color = 'rgba(55, 15, 255,0.01)')) %>%
      add_lines(x = long$bins[(1+hh*2):(hh*3)]/max_bin, y = (long$value[(1+hh*2):(hh*3)]/max(long$value[(1+hh*2):(hh*3)]))*ccc, name = "Overlap, Shuffled", yaxis = "y2",fill = 'tozeroy',line = list(color = 'rgba(207, 114, 129,0.1)'),fillcolor = list(color = 'rgba(207, 114, 129,0.01)')) %>%
      add_lines(x=fdr_x_vals, y=fdr_y_vals, name = "FDR",line = list(color = 'grey',dash = "dash")) %>%
      add_trace(x = fdr_cutoff_x, y = c(0,1),line = list(dash = "dash",color = "orange"),type='scatter',mode='lines',name="support at cutoff") %>%
      layout(margin = list(l=100, r=50, b=50, t=50, pad=0),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             yaxis2 = ay,yaxis=list(range=c(0,1),title="Overlap",showline = FALSE,showgrid = FALSE), 
             xaxis = list(title="Support", range=c(0,1), y = 0.05,showline = FALSE,showgrid = FALSE),
             legend = list(x = 0.1, y = -0.2,orientation = 'h')
      )
  })
  
  # Output: Overlap Plot (Plotly)
  output$overlapPLOT <- renderPlotly({
    overlap_plot_obj()
  })
  
  # Capture: Overlap Plot
  observeEvent(input$shotOverlap, {
    screenshot(selector = "#overlapPLOT", filename = paste0("overlap_plot_", current_filename_base()))
  })
  
  # Function: Jaccard Plot
  jaccard_plot_func <- function() {
    data_set <- current_network_set()
    if (is.null(data_set)) return(NULL)
    
    jaccard_mat <- NetworkManager$calculate_jaccard_matrix(data_set, input$self == FALSE)
    
    cell_lines <- names(data_set)
    
    # Fallback if names are missing (e.g. pre-loaded data might not have names on the list)
    if (is.null(cell_lines) && input$raw == FALSE) {
       cell_lines <- network_names_map[[input$data]]
    }
    
    # Final fallback
    if (is.null(cell_lines)) {
      cell_lines <- paste0("Net_", 1:length(data_set))
    }
    
    # Ensure character vector
    cell_lines <- as.character(cell_lines)
    
    # Try to extract zetavecs (sparsity) values
    if (any(grepl("zetavecs[0-9]+", cell_lines))) {
       cell_lines <- sub(".*zetavecs([0-9]+).*", "\\1", cell_lines)
    } else if (any(grepl("_", cell_lines))) {
      cell_lines <- sapply(strsplit(cell_lines, "_"), "[[", 1)
    }
    
    # Fallback: If labels are not unique (e.g. all "bolasso"), use 1-N
    if (length(unique(cell_lines)) < length(cell_lines)) {
       cell_lines <- as.character(1:length(cell_lines))
    }
    
    rownames(jaccard_mat) <- cell_lines
    colnames(jaccard_mat) <- cell_lines
    
    dist_mat <- as.dist(1 - jaccard_mat)
    
    # Use hclust with default "average" method
    hc <- hclust(dist_mat, method = "average")
    
    plot(hc, main = "Jaccard Similarity Dendrogram", xlab = "Networks", sub = "")
  }
  
  # Output: Jaccard Similarity Dendrogram
  output$jaccard <- renderPlot({
    jaccard_plot_func()
  })
  
  # Capture: Jaccard Plot
  observeEvent(input$shotJaccard, {
    screenshot(selector = "#jaccard", filename = paste0("jaccard_dendrogram_", input$data))
  })
  
  # Output: Text Info
  output$text1 <- renderPrint({
    if (input$raw == FALSE) {
      name_list <- network_names_map[[input$data]]
      idx <- as.numeric(input$sparsity)
      if (!is.null(name_list) && idx <= length(name_list)) {
        filename <- name_list[[idx]]
        
        cat(paste0(toupper(input$data), " GRN number ", idx, ".\n"))
        
        details <- c()
        
        if (grepl("Bolasso", filename)) {
          details <- c(details, "Nestboot of LASSO")
        } else if (grepl("Bolsco", filename)) {
          details <- c(details, "Nestboot of LSCO")
        } else if (grepl("Botlsco", filename)) {
          details <- c(details, "Nestboot of TLSCO")
        }
        
        # Extract Penalty (last numeric part before extension)
        if (grepl("_[0-9.e-]+\\.[a-zA-Z]+$", filename)) {
          penalty <- sub(".*_([0-9.e-]+)\\.[a-zA-Z]+$", "\\1", filename)
          details <- c(details, paste0("L1 penalty: ", penalty))
        }
        
        # Extract Links (_Lxxxx_)
        if (grepl("_L[0-9]+_", filename)) {
          links <- sub(".*_L([0-9]+)_.*", "\\1", filename)
          details <- c(details, paste0("Original Links: ", links))
        }
        
        # Extract Experiments (_Mxxx_)
        if (grepl("_M[0-9]+_", filename)) {
          experiments <- sub(".*_M([0-9]+)_.*", "\\1", filename)
          details <- c(details, paste0("Experiments: ", experiments))
        }
        
        if (length(details) > 0) {
          cat(paste(details, collapse = "\n"))
        }
      }
    } else {
      cat("Uploaded Data Mode")
    }
  })
}

#### UI ####
ui <- fluidPage(
  titlePanel("CancerGRN"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data", label = "Method", c("LASSO" = "lasso", "LSCO" = "lsco", "TLSCO" = "tlsco")),
      h4("Networks inferred in:"),
      h5(tags$a(href="https://www.nature.com/articles/s41598-020-70941-y", "'Perturbation-based gene regulatory network inference to unravel oncogenic mechanisms.'", target="_blank")),
      tags$a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE125958", " Raw data: GSE125958", target = "_blank"),
      
      checkboxInput('self', 'Self-Loop', FALSE),
      sliderInput("sparsity", "Sparsity", 18, min = 1, max = 30, step = 1),
      
      verbatimTextOutput("text1"),
      
      tags$br(),
      radioButtons("filetype", "File type:", choices = c("csv", "tsv"), inline = TRUE),
      downloadButton('downloadData', 'Download Network Data'),
      tags$hr(),
      
      h4("Other Projects"),
      checkboxInput('raw', 'Upload Data', FALSE),
      
      # Conditional Panel for Upload Settings
      conditionalPanel(
        condition = "input.raw == true",
        fileInput('file1', 'Network file(s) to upload', multiple = TRUE),
        radioButtons('sep', 'Separator', c(Tab = '\t', Comma = ','), '\t', inline = TRUE)
      ),
      
      tags$hr(),
      tags$div(class = "header",
               tags$p("Examples for each file type:"),
               tags$div(class="header", checked=NA,
               tags$p("Examples for each file type:"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/L1000_comparison", "L1000_comp,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/L1000NestBoot_Aug2018", "L1000_full,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/MycNestBoot_May2017", "MYC,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/arrieta-ortizNestBoot_Feb2018", "Arrieta-Ortiz,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/lorenzNestBoot_OCT2017", "Lorenz,",target="_blank"),
               tags$a(href="https://github.com/dcolinmorgan/NestBoot-Viz/tree/master/gardnerNestBoot_OCT2017", "Gardner",target="_blank")
      ),tags$br(),
      tags$div(class="header", checked=NA,
               tags$a(href="https://bitbucket.org/sonnhammergrni/genespider/src/BFECV/%2BMethods/BalanceFitError.m", "Inference code on Bitbucket",target="_blank")),
      tags$div(class="header", checked=NA,
               tags$a(href="https://github.com/dcolinmorgan/cancer-grn", "Shiny app on Github",target="_blank")),
      tags$div(class="header", checked=NA,
               tags$a(href="https://x.com/dcolinmorgan/", "dev",target="_blank")),
      tags$hr()
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("CytoscapeJS", 
                 h4("This tab displays the seleceted GRN where blue links reflect up regulation while red reflect negative down regulation."), 
                 actionButton("shotCyj", "Capture PNG"),
                 tags$hr(), 
                 selectInput("clay", label = "Layout:", 
                             c("CoSE" = "cose", "Cola" = "cola", "Concentric" = "concentric", 
                               "Circle" = "circle", "Breadthfirst" = "breadthfirst", 
                               "Grid" = "grid", "Random" = 'random', "Preset" = 'preset')), 
                 cyjShinyOutput("cyjShiny", height = '800px')),
        
        tabPanel("ForceSign", 
                 h4("This tab displays the seleceted GRN where node size and color represents overall, degree blue links reflect up regulation while red reflect negative down regulation."),
                 actionButton("shotForce", "Capture PNG"),
                 tags$hr(), 
                 forceNetworkOutput("forcelasso", height = '800px')),
        
        tabPanel("Overlap", 
                 h4("This tab displays the entire bootstrap support range from 0 to 1, as well as overlap between all bootstrap GRNs for measured (blue) and shuffled (red) data. The FDR is estimated via a null background model based on networks inferred from shuffled data. This is done to restrict inclusion of false links by setting FDR e.g. to 5%. The dashed orange line represents the cutoff where this is reached, The dashed grey line shows how the FDR behaves as a function of the bootstrap support."), 
                 actionButton("shotOverlap", "Capture PNG"),
                 tags$hr(), 
                 plotlyOutput("overlapPLOT")),
                 
        tabPanel("Jaccard Similarity",
                 h4("This tab displays the Jaccard similarity dendrogram for the inferred networks."),
                 actionButton("shotJaccard", "Capture PNG"),
                 plotOutput("jaccard"))
      )
    )
  )
)

#### Run ####
shinyApp(ui = ui, server = server)

