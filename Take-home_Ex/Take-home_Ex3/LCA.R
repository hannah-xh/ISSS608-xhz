# LCA.R - Module for Latent Class Analysis
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(cluster)
library(factoextra)
library(shinyjs)

# UI function for the LCA module
lca_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    # Add loading indicator - visible when operations are running
    tags$div(
      id = ns("loading-content"),
      style = "display: none; position: absolute; background: rgba(230, 247, 243, 0.85); z-index: 100; left: 0; right: 0; height: 100%; text-align: center; color: #0e523e; padding-top: 200px;",
      tags$img(src = "https://www.svgrepo.com/show/48294/loading.svg", width = 80, height = 80),
      tags$h4("Processing data...", style = "color: #0e523e;")
    ),
    
    # Custom CSS for green styling
    tags$style(HTML("
      /* Green statistic boxes */
      .stat-box {
        background-color: #e6f7f3;
        border-left: 5px solid #0e523e;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
        position: relative;
        min-height: 80px;
      }
      .stat-box .value {
        font-size: 24px;
        font-weight: bold;
        color: #0e523e;
      }
      .stat-box .title {
        font-size: 14px;
        color: #1a7d5d;
        font-weight: bold;
      }
      .stat-box .icon {
        position: absolute;
        right: 20px;
        bottom: 20px;
        opacity: 0.4;
        color: #0e523e;
      }
      
      /* For variable tags in the UI */
      .var-tag {
        display: inline-block;
        background-color: #b3eadb;
        color: #0e523e;
        border-radius: 4px;
        padding: 2px 8px;
        margin: 2px;
        font-size: 12px;
      }
    ")),
    
    fluidRow(
      # Left sidebar clustering panel
      column(width = 3,
             # Left sidebar clustering panel - using success for green color
             box(width = 12, title = "Clustering Panel", status = "success", solidHeader = TRUE,
                 style = "background-color: #f0f9f6;",
                 
                 # Number of clusters slider - with longer width
                 h4("Number of Clusters:", style = "color: #0e523e;"),
                 fluidRow(
                   column(2, p("3", style = "color: #1a7d5d;")),
                   column(8, 
                          sliderInput(ns("num_clusters"), "", 
                                      min = 3, max = 8, value = 4, step = 1,
                                      ticks = TRUE, width = "100%")
                   ),
                   column(2, p("8", style = "color: #1a7d5d;"))
                 ),
                 
                 # Sample percentage slider - with longer width
                 h4("Data Sample Size:", style = "color: #0e523e;"),
                 fluidRow(
                   column(2, p("10%", style = "color: #1a7d5d;")),
                   column(8, 
                          sliderInput(ns("sample_pct"), "", 
                                      min = 10, max = 100, value = 50, step = 10,
                                      ticks = TRUE, width = "100%")
                   ),
                   column(2, p("100%", style = "color: #1a7d5d;"))
                 ),
                 helpText("Percentage of data used for analysis", style = "color: #1a7d5d;"),
                 
                 # Variable selection section with collapsible panels
                 h4("Select Variables:", style = "color: #0e523e;"),
                 
                 # Explanation of pre-tested variables
                 div(
                   style = "background-color: #e6f7f3; border-left: 4px solid #0e523e; padding: 10px; margin-bottom: 15px;",
                   p("The following variables have been pre-tested and shown to provide effective clustering results in preliminary experiments.", 
                     style = "color: #0e523e; margin-bottom: 0;")
                 ),
                 
                 # Primary Variables as tags input with dropdown
                 div(
                   id = ns("primary_vars_container"),
                   tags$label("Variables:", style = "color: #1a7d5d; font-weight: bold;"),
                   div(
                     style = "border: 1px solid #1a7d5d; border-radius: 4px; padding: 8px; margin-bottom: 10px; background-color: white;",
                     uiOutput(ns("selected_primary_vars")),
                     actionLink(ns("toggle_primary"), "Select Variables", style = "color: #0e523e;")
                   )
                 ),
                 
                 # Hidden panel for primary variables selection
                 shinyjs::hidden(
                   div(
                     id = ns("primary_vars_panel"),
                     style = "margin-top: 5px; margin-bottom: 15px;",
                     checkboxGroupInput(ns("primary_vars"), NULL,
                                        choices = c("Size_cat", "Floor_cat", "Green_Index_cat", 
                                                    "Subway_Dist_cat", "Pop_Density_cat", "Median_Age_cat"),
                                        selected = c("Size_cat", "Floor_cat", "Green_Index_cat"),
                                        inline = FALSE
                     ),
                     div(
                       style = "text-align: right;",
                       actionLink(ns("done_primary"), "Done", style = "color: #0e523e; font-weight: bold;")
                     )
                   )
                 ),
                 
                 # Add space before button
                 br(),
                 
                 # Run button with deep green styling
                 div(style = "text-align: center;",
                     actionButton(ns("run_cluster"), "Run Cluster", 
                                  style = "color: #fff; background-color: #0e523e; border-color: #0a3c2e; width: 80%; font-weight: bold; padding: 10px;")
                 )
             )
      ),
      
      # Main content area - decreased width to 9
      column(width = 9,
             # Tabs at the top
             tabBox(
               id = ns("clusterTabs"),
               width = 12,
               tabPanel("Cluster Proportion", 
                        # Title in right corner
                        div(style = "text-align: right; margin-top: -10px; margin-bottom: 20px;",
                            h2("Latent Class Analysis", style = "font-family: serif; color: #0e523e;")),
                        
                        # Visualization for proportion - stacked bar chart
                        plotlyOutput(ns("cluster_proportion"), height = 400),
                        
                        # Statistics boxes at bottom
                        fluidRow(
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "AIC"),
                                     p(class = "value", textOutput(ns("aic_value"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "BIC"),
                                     p(class = "value", textOutput(ns("bic_value"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "Likelihood Ratio"),
                                     p(class = "value", textOutput(ns("likelihood_value"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "Silhouette Score"),
                                     p(class = "value", textOutput(ns("entropy_value"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          )
                        )
               ),
               
               tabPanel("Cluster Characteristics", 
                        div(style = "text-align: right; margin-top: -10px; margin-bottom: 20px;",
                            h2("Latent Class Analysis", style = "font-family: serif; color: #0e523e;")),
                        plotOutput(ns("cluster_features"), height = 400),
                        # Same statistics boxes
                        fluidRow(
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "AIC"),
                                     p(class = "value", textOutput(ns("aic_value2"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "BIC"),
                                     p(class = "value", textOutput(ns("bic_value2"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "Likelihood Ratio"),
                                     p(class = "value", textOutput(ns("likelihood_value2"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "Silhouette Score"),
                                     p(class = "value", textOutput(ns("entropy_value2"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          )
                        )
               ),
               
               tabPanel("Parallel Coordinates", 
                        div(style = "text-align: right; margin-top: -10px; margin-bottom: 20px;",
                            h2("Latent Class Analysis", style = "font-family: serif; color: #0e523e;")),
                        plotlyOutput(ns("parallel_plot"), height = 400),
                        # Same statistics boxes
                        fluidRow(
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "AIC"),
                                     p(class = "value", textOutput(ns("aic_value3"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "BIC"),
                                     p(class = "value", textOutput(ns("bic_value3"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "Likelihood Ratio"),
                                     p(class = "value", textOutput(ns("likelihood_value3"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "Silhouette Score"),
                                     p(class = "value", textOutput(ns("entropy_value3"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          )
                        )
               ),
               
               tabPanel("Data Explorer", 
                        div(style = "text-align: right; margin-top: -10px; margin-bottom: 20px;",
                            h2("Latent Class Analysis", style = "font-family: serif; color: #0e523e;")),
                        
                        # Filter controls with green styling
                        fluidRow(
                          column(3, selectInput(ns("filter_cluster"), "Filter by Cluster:", 
                                                choices = c("All", "1", "2", "3", "4", "5", "6", "7", "8"), 
                                                selected = "All")),
                          column(3, textInput(ns("search_text"), "Search:", "",
                                              width = "100%")),
                          column(3, numericInput(ns("display_rows"), "Rows to Display:", 10, min = 5, max = 100,
                                                 width = "100%"))
                        ),
                        
                        # Data table
                        DTOutput(ns("data_table")),
                        
                        # Same statistics boxes
                        fluidRow(
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "AIC"),
                                     p(class = "value", textOutput(ns("aic_value4"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "BIC"),
                                     p(class = "value", textOutput(ns("bic_value4"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "Likelihood Ratio"),
                                     p(class = "value", textOutput(ns("likelihood_value4"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          ),
                          column(width = 3,
                                 div(class = "stat-box",
                                     p(class = "title", "Silhouette Score"),
                                     p(class = "value", textOutput(ns("entropy_value4"))),
                                     div(class = "icon", icon("calculator"))
                                 )
                          )
                        )
               )
             )
      )
    )
  )
}

# Server function for the LCA module
lca_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper functions for loading indicator
    showLoading <- function() {
      shinyjs::show(id = "loading-content", anim = TRUE, animType = "fade")
    }
    
    hideLoading <- function() {
      shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
    }
    
    # Function to calculate BIC for k-means
    calculate_kmeans_bic <- function(kmeans_result, data) {
      n <- nrow(data)
      k <- length(kmeans_result$size)  # Number of clusters
      d <- ncol(data)                  # Number of dimensions
      
      # Within-cluster sum of squares
      wcss <- kmeans_result$tot.withinss
      
      # Calculate log-likelihood
      # -n/2 * log(wcss/n) - n*d/2 * log(2*pi) - n/2
      log_likelihood <- -n/2 * log(wcss/n) - n*d/2 * log(2*pi) - n/2
      
      # BIC = log_likelihood - (k*d + k-1) * log(n)/2
      # k*d parameters for cluster centers, k-1 for cluster probabilities
      bic <- log_likelihood - (k*d + k-1) * log(n)/2
      
      return(round(bic, 2))
    }
    
    # Function to calculate AIC for k-means
    calculate_kmeans_aic <- function(kmeans_result, data) {
      n <- nrow(data)
      k <- length(kmeans_result$size)  # Number of clusters
      d <- ncol(data)                  # Number of dimensions
      
      # Within-cluster sum of squares
      wcss <- kmeans_result$tot.withinss
      
      # Calculate log-likelihood
      log_likelihood <- -n/2 * log(wcss/n) - n*d/2 * log(2*pi) - n/2
      
      # AIC = log_likelihood - (k*d + k-1)
      # k*d parameters for cluster centers, k-1 for cluster probabilities
      aic <- log_likelihood - (k*d + k-1)
      
      return(round(aic, 2))
    }
    
    # Calculate silhouette score as a measure of cluster quality
    calculate_silhouette <- function(kmeans_result, data) {
      if(requireNamespace("cluster", quietly = TRUE)) {
        sil <- cluster::silhouette(kmeans_result$cluster, dist(data))
        return(round(mean(sil[,3]), 3))  # Average silhouette width
      } else {
        return(NA)
      }
    }
    
    # Calculate pseudo-likelihood ratio test
    # Ratio of between-cluster to total sum of squares
    calculate_pseudo_lr <- function(kmeans_result) {
      between_ss <- kmeans_result$betweenss
      total_ss <- kmeans_result$totss
      return(round(between_ss/total_ss, 3))
    }
    
    # Create reactive values to store all statistics
    cluster_stats <- reactiveValues(
      aic = NA,
      bic = NA,
      lr = NA,
      silhouette = NA
    )
    
    # Setup variable selection panel toggle functionality
    observeEvent(input$toggle_primary, {
      shinyjs::toggle(id = "primary_vars_panel", asis = FALSE, selector = paste0("#", ns("primary_vars_panel")))
    })
    
    observeEvent(input$done_primary, {
      shinyjs::hide(id = "primary_vars_panel", asis = FALSE, selector = paste0("#", ns("primary_vars_panel")))
    })
    
    # Render selected variables as tags
    output$selected_primary_vars <- renderUI({
      if(length(input$primary_vars) == 0) {
        return(HTML("<span style='color:#888;'>No variables selected</span>"))
      }
      
      var_tags <- lapply(input$primary_vars, function(var) {
        tags$span(var, class = "var-tag")
      })
      
      # Add some spacing between tags
      spaced_tags <- list()
      for(i in seq_along(var_tags)) {
        spaced_tags[[2*i-1]] <- var_tags[[i]]
        if(i < length(var_tags)) spaced_tags[[2*i]] <- tags$span(" ")
      }
      
      do.call(tagList, spaced_tags)
    })
    
    # Reactive values to store results
    current_clusters <- reactiveVal(NULL)
    clustered_data <- reactiveVal(NULL)
    
    # Use only primary variables
    selected_variables <- reactive({
      input$primary_vars
    })
    
    # Read data from RDS file
    Property_data_lca <- readRDS("data/Property_data_lca.rds")
    
    # Sampled data for performance - improved to ensure minimum sample for visualization
    sampled_data <- reactive({
      req(input$sample_pct)
      
      # Calculate minimum sample size for good visualization (at least 300 points)
      min_sample_size <- min(300, nrow(Property_data_lca))
      
      if(input$sample_pct == 100) {  # Using 100% of the data
        return(Property_data_lca)
      } else {
        # Calculate sample size based on percentage, but ensure it's at least the minimum
        sample_size <- max(min_sample_size, floor(nrow(Property_data_lca) * (input$sample_pct/100)))
        # Random sampling
        set.seed(123) # For reproducibility
        return(Property_data_lca[sample(1:nrow(Property_data_lca), sample_size), ])
      }
    })
    
    # Perform clustering when the button is clicked
    observeEvent(input$run_cluster, {
      showLoading() # Show loading indicator
      
      # Ensure at least one variable is selected
      req(length(selected_variables()) > 0)
      
      # Get sampled data
      data_to_use <- sampled_data()
      
      # Get selected variables
      vars_to_use <- selected_variables()
      
      # Extract data for clustering
      cluster_data <- data_to_use[, vars_to_use, drop = FALSE]
      
      # Handle categorical variables properly
      # Convert all selected variables to numeric (since they are categorical)
      cluster_data_numeric <- as.data.frame(lapply(cluster_data, function(x) {
        if(is.factor(x) || is.character(x)) {
          return(as.numeric(as.factor(x)))
        } else {
          return(as.numeric(x))
        }
      }))
      
      # Show progress indicator
      withProgress(message = 'Performing clustering analysis...', {
        # Perform K-means clustering
        set.seed(123) # For reproducibility
        result <- kmeans(cluster_data_numeric, centers = input$num_clusters)
        
        # Calculate cluster statistics and update reactive values
        cluster_stats$aic <- calculate_kmeans_aic(result, cluster_data_numeric)
        cluster_stats$bic <- calculate_kmeans_bic(result, cluster_data_numeric)
        cluster_stats$lr <- calculate_pseudo_lr(result)
        cluster_stats$silhouette <- calculate_silhouette(result, cluster_data_numeric)
        
        # Get cluster assignments
        cluster_assignments <- result$cluster
        
        # Save results
        current_clusters(result)
        
        # Add cluster assignments to the data
        clustered_data_with_results <- data_to_use %>%
          mutate(cluster = as.factor(cluster_assignments))
        
        # Update reactive value
        clustered_data(clustered_data_with_results)
        
        # Update filter dropdown
        updateSelectInput(session, "filter_cluster",
                          choices = c("All", as.character(1:input$num_clusters)))
        
        # Notify user that clustering is complete
        showNotification("Clustering complete!", type = "message")
      })
      
      hideLoading() # Hide loading indicator
    })
    
    # Cluster proportion visualization
    output$cluster_proportion <- renderPlotly({
      req(clustered_data())
      
      # Calculate cluster proportions
      cluster_props <- clustered_data() %>%
        count(cluster) %>%
        mutate(
          prop = n / sum(n),
          percent = prop * 100,
          cluster = as.integer(as.character(cluster))
        ) %>%
        arrange(cluster)
      
      # Create color palette - using more green shades
      colors <- colorRampPalette(c("#4dcfa9", "#1a7d5d", "#0e523e"))(input$num_clusters)
      
      # Create the bar chart
      p <- plot_ly(
        x = cluster_props$percent,
        y = cluster_props$cluster,
        type = "bar",
        orientation = "h",
        marker = list(color = colors),
        hoverinfo = "text",
        text = ~paste("Cluster", cluster_props$cluster, ": ", round(cluster_props$percent, 1), "%")
      )
      
      # Layout
      p <- layout(
        p,
        title = list(
          text = "Percentage of Data in Each Cluster",
          font = list(color = "#0e523e")
        ),
        xaxis = list(
          title = list(
            text = "Percentage",
            font = list(color = "#1a7d5d")
          ),
          range = c(0, 100),
          tickvals = c(0, 10, 20, 30, 40, 50),
          ticktext = c("0%", "10%", "20%", "30%", "40%", "50%")
        ),
        yaxis = list(
          title = list(
            text = "Cluster",
            font = list(color = "#1a7d5d")
          ),
          autorange = "reversed",
          dtick = 1
        ),
        margin = list(t = 50, r = 50),
        paper_bgcolor = "#f9fcfb",
        plot_bgcolor = "#f9fcfb"
      )
      
      return(p)
    })
    
    # Cluster characteristics visualization
    output$cluster_features <- renderPlot({
      req(clustered_data(), selected_variables())
      
      # Calculate means for each cluster and variable
      cluster_means <- clustered_data() %>%
        group_by(cluster) %>%
        summarise(across(all_of(selected_variables()), ~mean(as.numeric(as.character(.)), na.rm = TRUE)))
      
      # Convert to long format for plotting
      long_means <- cluster_means %>%
        pivot_longer(cols = -cluster, names_to = "variable", values_to = "mean")
      
      # Create bar chart with custom palette - using more green shades
      ggplot(long_means, aes(x = variable, y = mean, fill = cluster)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = colorRampPalette(c("#4dcfa9", "#1a7d5d", "#0e523e"))(length(unique(long_means$cluster)))) +
        labs(title = "Variable Means by Cluster", 
             x = "Variable", 
             y = "Mean Value") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#0e523e", face = "bold"),
          axis.title = element_text(color = "#1a7d5d"),
          axis.text = element_text(color = "#1a7d5d"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(face = "bold", color = "#0e523e"),
          legend.text = element_text(color = "#1a7d5d"),
          panel.background = element_rect(fill = "#f9fcfb"),
          plot.background = element_rect(fill = "#f9fcfb")
        )
    })
    
    # Parallel coordinates plot - improved to show more clearly
    output$parallel_plot <- renderPlotly({
      req(clustered_data(), selected_variables())
      
      # Prepare data for parallel coordinates
      parallel_data <- clustered_data()
      
      # Ensure we have data for plotting
      req(nrow(parallel_data) > 0)
      
      # Select only variables for plotting
      plot_vars <- selected_variables()
      
      # Need at least 2 variables for parallel coordinates
      req(length(plot_vars) >= 2)
      
      # Convert all variables to numeric for plotting
      parallel_data_numeric <- parallel_data
      for(var in plot_vars) {
        if(is.factor(parallel_data[[var]]) || is.character(parallel_data[[var]])) {
          parallel_data_numeric[[var]] <- as.numeric(as.factor(parallel_data[[var]]))
        }
      }
      
      # Create dimensions list for plotly
      dimensions <- lapply(plot_vars, function(var) {
        list(
          range = c(min(parallel_data_numeric[[var]], na.rm = TRUE), 
                    max(parallel_data_numeric[[var]], na.rm = TRUE)),
          label = var,
          values = parallel_data_numeric[[var]]
        )
      })
      
      # Create color scale - using more green shades
      cluster_colors <- colorRampPalette(c("#4dcfa9", "#1a7d5d", "#0e523e"))(input$num_clusters)
      color_scale <- setNames(
        data.frame(
          seq(0, 1, length.out = input$num_clusters), 
          cluster_colors
        ),
        NULL
      )
      
      # Create parallel coordinates plot with improved line visibility
      p <- plot_ly(
        type = 'parcoords',
        line = list(
          color = as.numeric(as.character(parallel_data$cluster)),
          colorscale = color_scale,
          showscale = TRUE,
          opacity = 0.7,  # Add some opacity for better visibility
          width = 1.5,    # Thicker lines
          colorbar = list(
            title = 'Cluster',
            tickfont = list(color = "#1a7d5d"),
            titlefont = list(color = "#0e523e")
          )
        ),
        dimensions = dimensions
      )
      
      # Layout
      p <- layout(
        p,
        title = list(
          text = "Parallel Coordinates Plot by Cluster",
          font = list(color = "#0e523e")
        ),
        margin = list(t = 50, r = 50),
        paper_bgcolor = "#f9fcfb"
      )
      
      return(p)
    })
    
    # Data table
    output$data_table <- renderDT({
      req(clustered_data())
      
      # Filter by cluster if specified
      filtered_data <- clustered_data()
      if(input$filter_cluster != "All") {
        filtered_data <- filtered_data %>%
          filter(cluster == input$filter_cluster)
      }
      
      # Apply search filter if provided
      if(!is.null(input$search_text) && input$search_text != "") {
        search_term <- tolower(input$search_text)
        
        # Search in character/factor columns
        text_cols <- sapply(filtered_data, function(x) is.character(x) || is.factor(x))
        
        if(any(text_cols)) {
          text_data <- filtered_data[, text_cols, drop = FALSE]
          match_rows <- apply(text_data, 1, function(row) {
            any(grepl(search_term, tolower(row), fixed = TRUE))
          })
          filtered_data <- filtered_data[match_rows, ]
        }
      }
      
      # Enhanced green color palette for clusters
      green_palette <- colorRampPalette(c("#e6f7f3", "#b3eadb", "#80dcc2", "#4dcfa9", "#1a7d5d", "#0e523e"))(input$num_clusters)
      
      # Create data table with cluster column highlighted
      datatable(filtered_data, 
                options = list(
                  pageLength = input$display_rows,
                  lengthMenu = c(5, 10, 25, 50),
                  scrollX = TRUE,
                  scrollY = "400px",
                  dom = 'ltipr',
                  deferRender = TRUE,
                  scroller = TRUE
                ),
                rownames = FALSE
      ) %>%
        formatStyle(
          'cluster',
          backgroundColor = styleEqual(
            levels = as.character(1:input$num_clusters),
            values = green_palette
          ),
          color = styleEqual(
            levels = as.character(input$num_clusters-(0:(input$num_clusters-1))),
            values = c(rep("#FFFFFF", input$num_clusters/2), rep("#000000", ceiling(input$num_clusters/2)))
          ),
          fontWeight = 'bold'
        )
    })
    
    # Dynamic statistics for all tabs - use the reactive values
    output$aic_value <- output$aic_value2 <- output$aic_value3 <- output$aic_value4 <- renderText({
      if(is.na(cluster_stats$aic)) {
        return("Run clustering first")
      } else {
        return(format(cluster_stats$aic, big.mark = ","))
      }
    })
    
    output$bic_value <- output$bic_value2 <- output$bic_value3 <- output$bic_value4 <- renderText({
      if(is.na(cluster_stats$bic)) {
        return("Run clustering first")
      } else {
        return(format(cluster_stats$bic, big.mark = ","))
      }
    })
    
    output$likelihood_value <- output$likelihood_value2 <- output$likelihood_value3 <- output$likelihood_value4 <- renderText({
      if(is.na(cluster_stats$lr)) {
        return("Run clustering first")
      } else {
        return(cluster_stats$lr)
      }
    })
    
    output$entropy_value <- output$entropy_value2 <- output$entropy_value3 <- output$entropy_value4 <- renderText({
      if(is.na(cluster_stats$silhouette)) {
        return("Run clustering first")
      } else {
        return(cluster_stats$silhouette)
      }
    })
  })
}