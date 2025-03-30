# MLR Module for Property Price Analysis Dashboard
# Load required libraries are already in the main app

# UI Function
mlr_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    # Main content layout
    fluidRow(
      # Left sidebar MLR panel - 3 columns wide
      column(width = 3,
             # Left sidebar MLR panel - using success for green color
             box(width = 12, title = "MLR Model Builder", status = "primary", solidHeader = TRUE,
                 style = "background-color: #f0f9f6;",
                 
                 # Variable selection sections with collapsible panels
                 h4("Select Variables:", style = "color: #2E6C40;"),
                 
                 # Property Characteristics Variables
                 div(
                   id = ns("property_vars_container"),
                   tags$label("Property Characteristics:", style = "color: #2E6C40; font-weight: bold;"),
                   div(
                     style = "border: 1px solid #A8D08D; border-radius: 4px; padding: 8px; margin-bottom: 10px; background-color: white;",
                     uiOutput(ns("selected_property_vars")),
                     actionLink(ns("toggle_property"), "Select Variables", style = "color: #2E6C40;")
                   )
                 ),
                 
                 # Hidden panel for property variables selection
                 shinyjs::hidden(
                   div(
                     id = ns("property_vars_panel"),
                     style = "margin-top: 5px; margin-bottom: 15px;",
                     checkboxGroupInput(ns("property_vars"), NULL,
                                        choices = c("Size", "Floor", "Units", "Parking", "Year", "Highest floor"),
                                        selected = c("Size", "Floor", "Units", "Parking", "Year"),
                                        inline = FALSE
                     ),
                     div(
                       style = "text-align: right;",
                       actionLink(ns("done_property"), "Done", style = "color: #2E6C40; font-weight: bold;")
                     )
                   )
                 ),
                 
                 # Location Variables
                 div(
                   id = ns("location_vars_container"),
                   tags$label("Location Variables:", style = "color: #2E6C40; font-weight: bold;"),
                   div(
                     style = "border: 1px solid #A8D08D; border-radius: 4px; padding: 8px; margin-bottom: 10px; background-color: white;",
                     uiOutput(ns("selected_location_vars")),
                     actionLink(ns("toggle_location"), "Select Variables", style = "color: #2E6C40;")
                   )
                 ),
                 
                 # Hidden panel for location variables selection
                 shinyjs::hidden(
                   div(
                     id = ns("location_vars_panel"),
                     style = "margin-top: 5px; margin-bottom: 15px;",
                     checkboxGroupInput(ns("location_vars"), NULL,
                                        choices = c("Dist. Subway", "Dist. CBD", "Pop. Density", "Dist. Green"),
                                        selected = c("Dist. Subway", "Dist. CBD", "Pop. Density"),
                                        inline = FALSE
                     ),
                     div(
                       style = "text-align: right;",
                       actionLink(ns("done_location"), "Done", style = "color: #2E6C40; font-weight: bold;")
                     )
                   )
                 ),
                 
                 # Demographic Variables
                 div(
                   id = ns("demographic_vars_container"),
                   tags$label("Demographic Variables:", style = "color: #2E6C40; font-weight: bold;"),
                   div(
                     style = "border: 1px solid #A8D08D; border-radius: 4px; padding: 8px; margin-bottom: 10px; background-color: white;",
                     uiOutput(ns("selected_demographic_vars")),
                     actionLink(ns("toggle_demographic"), "Select Variables", style = "color: #2E6C40;")
                   )
                 ),
                 
                 # Hidden panel for demographic variables selection
                 shinyjs::hidden(
                   div(
                     id = ns("demographic_vars_panel"),
                     style = "margin-top: 5px; margin-bottom: 15px;",
                     checkboxGroupInput(ns("demographic_vars"), NULL,
                                        choices = c("Higher Degree", "Median Age", "Young Population", "Old Population"),
                                        selected = c("Higher Degree"),
                                        inline = FALSE
                     ),
                     div(
                       style = "text-align: right;",
                       actionLink(ns("done_demographic"), "Done", style = "color: #2E6C40; font-weight: bold;")
                     )
                   )
                 ),
                 
                 # Seasonal Variables
                 div(
                   id = ns("seasonal_vars_container"),
                   tags$label("Seasonal Variables:", style = "color: #2E6C40; font-weight: bold;"),
                   div(
                     style = "border: 1px solid #A8D08D; border-radius: 4px; padding: 8px; margin-bottom: 10px; background-color: white;",
                     uiOutput(ns("selected_seasonal_vars")),
                     actionLink(ns("toggle_seasonal"), "Select Variables", style = "color: #2E6C40;")
                   )
                 ),
                 
                 # Hidden panel for seasonal variables selection
                 shinyjs::hidden(
                   div(
                     id = ns("seasonal_vars_panel"),
                     style = "margin-top: 5px; margin-bottom: 15px;",
                     checkboxGroupInput(ns("seasonal_vars"), NULL,
                                        choices = c("Spring", "Fall", "Winter", "Heating"),
                                        selected = c("Spring", "Fall", "Winter"),
                                        inline = FALSE
                     ),
                     div(
                       style = "text-align: right;",
                       actionLink(ns("done_seasonal"), "Done", style = "color: #2E6C40; font-weight: bold;")
                     )
                   )
                 ),
                 
                 # VIF threshold slider
                 h4("Model Settings:", style = "color: #2E6C40; margin-top: 20px;"),
                 fluidRow(
                   column(12, 
                          sliderInput(ns("vif_threshold"), "VIF Threshold:",
                                      min = 2, max = 10, value = 5, step = 0.5,
                                      ticks = TRUE, width = "100%")
                   )
                 ),
                 helpText("Variables with VIF above threshold will be highlighted", style = "color: #3B8C53;"),
                 
                 # Target variable transformation
                 radioButtons(ns("transform_type"), "Target Variable Transformation:",
                              choices = c("Original" = "original", 
                                          "Log Transform" = "log",
                                          "Square Root" = "sqrt"),
                              selected = "log"),
                 
                 # Add space before button
                 br(),
                 
                 # Run button with deep green styling
                 div(style = "text-align: center;",
                     actionButton(ns("run_mlr"), "Run MLR Analysis", 
                                  style = "color: #fff; background-color: #2E6C40; border-color: #A8D08D; width: 80%; font-weight: bold; padding: 10px;")
                 ),
                 
                 # Button to use recommended model
                 div(style = "text-align: center; margin-top: 10px;",
                     actionButton(ns("use_recommended"), "Use Recommended Model", 
                                  style = "color: #2E6C40; background-color: #A8D08D; border-color: #2E6C40; width: 80%; font-weight: bold; padding: 10px;")
                 )
             )
      ),
      
      # Main content area - 9 columns wide
      column(width = 9,
             # Tabs at the top
             tabBox(
               id = ns("mlrTabs"),
               width = 12,
               
               # Model Summary Tab
               tabPanel("Model Summary", 
                        # Title in right corner
                        div(style = "text-align: right; margin-top: -10px; margin-bottom: 20px;",
                            h2("Multiple Linear Regression Analysis", style = "font-family: serif; color: #2E6C40;")),
                        
                        # Regression equation box
                        box(width = 12, 
                            title = "Regression Equation", status = "primary", solidHeader = TRUE, 
                            style = "background-color: #f0f9f6;",
                            verbatimTextOutput(ns("regression_equation"))
                        ),
                        
                        # Model coefficients table and statistics
                        fluidRow(
                          column(width = 8, 
                                 box(width = 12, title = "Model Coefficients", status = "primary", solidHeader = TRUE,
                                     style = "background-color: #f0f9f6;",
                                     DTOutput(ns("coefficients_table"))
                                 )
                          ),
                          column(width = 4,
                                 # Statistics boxes
                                 div(class = "stat-box",
                                     p(class = "title", "R-squared"),
                                     p(class = "value", textOutput(ns("r_squared"))),
                                     div(class = "icon", icon("chart-line"))
                                 ),
                                 div(class = "stat-box",
                                     p(class = "title", "Adjusted R-squared"),
                                     p(class = "value", textOutput(ns("adj_r_squared"))),
                                     div(class = "icon", icon("chart-line"))
                                 ),
                                 div(class = "stat-box",
                                     p(class = "title", "F-statistic"),
                                     p(class = "value", textOutput(ns("f_statistic"))),
                                     div(class = "icon", icon("calculator"))
                                 ),
                                 div(class = "stat-box",
                                     p(class = "title", "Residual Standard Error"),
                                     p(class = "value", textOutput(ns("residual_std_error"))),
                                     div(class = "icon", icon("ruler"))
                                 )
                          )
                        )
               ),
               
               # Variable Importance Tab
               tabPanel("Variable Importance", 
                        div(style = "text-align: right; margin-top: -10px; margin-bottom: 20px;",
                            h2("Multiple Linear Regression Analysis", style = "font-family: serif; color: #2E6C40;")),
                        
                        # Standardized coefficients plot
                        box(width = 12, title = "Variable Importance", status = "primary", solidHeader = TRUE,
                            style = "background-color: #f0f9f6;",
                            plotlyOutput(ns("variable_importance_plot"), height = 400)
                        ),
                        
                        # Explanatory text
                        box(width = 12, title = "Interpretation", status = "primary", solidHeader = TRUE,
                            style = "background-color: #f0f9f6;",
                            htmlOutput(ns("importance_interpretation"))
                        )
               ),
               
               # Multicollinearity Analysis Tab
               tabPanel("Multicollinearity", 
                        div(style = "text-align: right; margin-top: -10px; margin-bottom: 20px;",
                            h2("Multiple Linear Regression Analysis", style = "font-family: serif; color: #2E6C40;")),
                        
                        fluidRow(
                          # Correlation matrix
                          column(width = 6,
                                 box(width = 12, title = "Correlation Matrix", status = "primary", solidHeader = TRUE,
                                     style = "background-color: #f0f9f6;",
                                     plotOutput(ns("correlation_plot"), height = 400)
                                 )
                          ),
                          
                          # VIF analysis
                          column(width = 6,
                                 box(width = 12, title = "Variance Inflation Factors", status = "primary", solidHeader = TRUE,
                                     style = "background-color: #f0f9f6;",
                                     plotlyOutput(ns("vif_plot"), height = 400)
                                 )
                          )
                        ),
                        
                        # Highly correlated pairs
                        box(width = 12, title = "Highly Correlated Variables", status = "primary", solidHeader = TRUE,
                            style = "background-color: #f0f9f6;",
                            DTOutput(ns("high_correlation_table"))
                        )
               ),
               
               # Diagnostics Tab
               tabPanel("Model Diagnostics", 
                        div(style = "text-align: right; margin-top: -10px; margin-bottom: 20px;",
                            h2("Multiple Linear Regression Analysis", style = "font-family: serif; color: #2E6C40;")),
                        
                        # Residual plots
                        fluidRow(
                          column(width = 6,
                                 box(width = 12, title = "Residuals vs Fitted", status = "primary", solidHeader = TRUE,
                                     style = "background-color: #f0f9f6;",
                                     plotOutput(ns("residual_fitted_plot"), height = 300)
                                 )
                          ),
                          column(width = 6,
                                 box(width = 12, title = "Normal Q-Q Plot", status = "primary", solidHeader = TRUE,
                                     style = "background-color: #f0f9f6;",
                                     plotOutput(ns("qq_plot"), height = 300)
                                 )
                          )
                        ),
                        
                        fluidRow(
                          column(width = 6,
                                 box(width = 12, title = "Scale-Location Plot", status = "primary", solidHeader = TRUE,
                                     style = "background-color: #f0f9f6;",
                                     plotOutput(ns("scale_location_plot"), height = 300)
                                 )
                          ),
                          column(width = 6,
                                 box(width = 12, title = "Residuals Histogram", status = "primary", solidHeader = TRUE,
                                     style = "background-color: #f0f9f6;",
                                     plotOutput(ns("residual_histogram"), height = 300)
                                 )
                          )
                        ),
                        
                        # Diagnostic test results
                        fluidRow(
                          column(width = 6,
                                 div(class = "stat-box",
                                     p(class = "title", "Shapiro-Wilk Test (Normality)"),
                                     p(class = "value", textOutput(ns("shapiro_test"))),
                                     div(class = "icon", icon("check-circle"))
                                 )
                          ),
                          column(width = 6,
                                 div(class = "stat-box",
                                     p(class = "title", "Breusch-Pagan Test (Heteroscedasticity)"),
                                     p(class = "value", textOutput(ns("bp_test"))),
                                     div(class = "icon", icon("check-circle"))
                                 )
                          )
                        )
               )
             )
      )
    ),
    # Additional CSS for styling specific to this module
    tags$style(HTML("
      /* MLR module specific styles */
      .stat-box {
        background-color: #E3F6E5;
        border-left: 5px solid #2E6C40;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
        position: relative;
        min-height: 80px;
      }
      .stat-box .value {
        font-size: 24px;
        font-weight: bold;
        color: #2E6C40;
      }
      .stat-box .title {
        font-size: 14px;
        color: #3B8C53;
        font-weight: bold;
      }
      .stat-box .icon {
        position: absolute;
        right: 20px;
        bottom: 20px;
        opacity: 0.4;
        color: #2E6C40;
      }
      
      /* For variable tags in the UI */
      .var-tag {
        display: inline-block;
        background-color: #A8D08D;
        color: #2E6C40;
        border-radius: 4px;
        padding: 2px 8px;
        margin: 2px;
        font-size: 12px;
      }
    "))
  )
}

# Server Function
mlr_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper functions for loading indicator
    showLoading <- function() {
      showNotification("Processing MLR Analysis...", type = "message", duration = NULL, id = "mlr_loading")
    }
    
    hideLoading <- function() {
      removeNotification("mlr_loading")
    }
    
    # Create reactive values to store results
    mlr_results <- reactiveValues(
      model = NULL,
      vif_data = NULL,
      correlation_matrix = NULL,
      high_correlations = NULL,
      std_coefficients = NULL,
      residuals_data = NULL,
      shapiro_test_result = NULL,
      bp_test_result = NULL
    )
    
    # Function to load preprocessed data
    load_preprocessed_data <- reactive({
      tryCatch({
        # Load data from RDS file instead of Excel
        Property_data <- readRDS("data/Property_data.rds")
        
        # Apply the same preprocessing as in your analysis
        # Convert categorical variables to factors
        Property_data <- Property_data %>%
          mutate(
            Spring = as.factor(Spring),
            Fall = as.factor(Fall),
            Winter = as.factor(Winter),
            Heating = as.factor(Heating),
            `Top Univ.` = as.factor(`Top Univ.`)
          )
        
        # Add log-transformed price which is used in your final model
        Property_data$LogPrice <- log(Property_data$`Property Prices`)
        
        # Add sqrt-transformed price for optional use
        Property_data$SqrtPrice <- sqrt(Property_data$`Property Prices`)
        
        return(Property_data)
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error", duration = NULL)
        return(NULL)
      })
    })
    
    # Function to create the target variable based on transformation type
    get_target_variable <- reactive({
      if(input$transform_type == "log") {
        return("LogPrice")
      } else if(input$transform_type == "sqrt") {
        return("SqrtPrice")
      } else {
        return("`Property Prices`")
      }
    })
    
    # Combine selected variables from all groups
    selected_variables <- reactive({
      c(input$property_vars, input$location_vars, input$demographic_vars, input$seasonal_vars)
    })
    
    # Function to calculate VIF values
    calculate_vif <- function(formula, data) {
      model <- lm(formula, data = data)
      vif_values <- car::vif(model)
      
      vif_df <- data.frame(
        Variable = names(vif_values),
        VIF = as.numeric(vif_values)
      )
      
      return(vif_df)
    }
    
    # Function to check multicollinearity
    check_multicollinearity <- function(data, threshold = 0.7) {
      # Select only numeric variables
      numeric_vars <- selected_variables()[sapply(data[, selected_variables()], is.numeric)]
      numeric_data <- data[, numeric_vars, drop = FALSE]
      
      # Calculate correlation matrix
      cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
      
      # Find highly correlated variables
      high_cor <- which(abs(cor_matrix) > threshold & abs(cor_matrix) < 1, arr.ind = TRUE)
      
      if(length(high_cor) > 0) {
        high_cor_pairs <- data.frame(
          var1 = rownames(cor_matrix)[high_cor[,1]],
          var2 = colnames(cor_matrix)[high_cor[,2]],
          correlation = cor_matrix[high_cor]
        )
        
        # Sort by absolute correlation value
        high_cor_pairs$abs_corr <- abs(high_cor_pairs$correlation)
        high_cor_pairs <- high_cor_pairs[order(-high_cor_pairs$abs_corr), ]
        high_cor_pairs$abs_corr <- NULL
        
        # Remove duplicates (e.g., A-B and B-A)
        high_cor_pairs <- high_cor_pairs[!duplicated(t(apply(high_cor_pairs[, c("var1", "var2")], 1, sort))), ]
      } else {
        high_cor_pairs <- data.frame(var1 = character(0), var2 = character(0), correlation = numeric(0))
      }
      
      return(list(
        correlation_matrix = cor_matrix,
        high_correlation_pairs = high_cor_pairs
      ))
    }
    
    # Setup variable selection panels toggle functionality
    observeEvent(input$toggle_property, {
      shinyjs::toggle("property_vars_panel", anim = TRUE, animType = "slide")
    })
    
    observeEvent(input$done_property, {
      shinyjs::hide("property_vars_panel", anim = TRUE, animType = "slide")
    })
    
    observeEvent(input$toggle_location, {
      shinyjs::toggle("location_vars_panel", anim = TRUE, animType = "slide")
    })
    
    observeEvent(input$done_location, {
      shinyjs::hide("location_vars_panel", anim = TRUE, animType = "slide")
    })
    
    observeEvent(input$toggle_demographic, {
      shinyjs::toggle("demographic_vars_panel", anim = TRUE, animType = "slide")
    })
    
    observeEvent(input$done_demographic, {
      shinyjs::hide("demographic_vars_panel", anim = TRUE, animType = "slide")
    })
    
    observeEvent(input$toggle_seasonal, {
      shinyjs::toggle("seasonal_vars_panel", anim = TRUE, animType = "slide")
    })
    
    observeEvent(input$done_seasonal, {
      shinyjs::hide("seasonal_vars_panel", anim = TRUE, animType = "slide")
    })
    
    # Initialize the module with data loaded
    observeEvent(1, {
      # Load data at startup
      data <- load_preprocessed_data()
      
      if(!is.null(data)) {
        # Show notification that data is loaded
        showNotification(paste("MLR Data loaded successfully:", nrow(data), "observations"), type = "message")
        
        # Run the model with recommended settings
        shinyjs::delay(1000, {
          click(ns("run_mlr"))
        })
      }
    }, once = TRUE)
    
    # Render selected variables as tags
    output$selected_property_vars <- renderUI({
      if(length(input$property_vars) == 0) {
        return(HTML("<span style='color:#888;'>No variables selected</span>"))
      }
      
      var_tags <- lapply(input$property_vars, function(var) {
        tags$span(var, class = "var-tag")
      })
      
      # Add spacing between tags
      spaced_tags <- list()
      for(i in seq_along(var_tags)) {
        spaced_tags[[2*i-1]] <- var_tags[[i]]
        if(i < length(var_tags)) spaced_tags[[2*i]] <- tags$span(" ")
      }
      
      do.call(tagList, spaced_tags)
    })
    
    output$selected_location_vars <- renderUI({
      if(length(input$location_vars) == 0) {
        return(HTML("<span style='color:#888;'>No variables selected</span>"))
      }
      
      var_tags <- lapply(input$location_vars, function(var) {
        tags$span(var, class = "var-tag")
      })
      
      # Add spacing between tags
      spaced_tags <- list()
      for(i in seq_along(var_tags)) {
        spaced_tags[[2*i-1]] <- var_tags[[i]]
        if(i < length(var_tags)) spaced_tags[[2*i]] <- tags$span(" ")
      }
      
      do.call(tagList, spaced_tags)
    })
    
    output$selected_demographic_vars <- renderUI({
      if(length(input$demographic_vars) == 0) {
        return(HTML("<span style='color:#888;'>No variables selected</span>"))
      }
      
      var_tags <- lapply(input$demographic_vars, function(var) {
        tags$span(var, class = "var-tag")
      })
      
      # Add spacing between tags
      spaced_tags <- list()
      for(i in seq_along(var_tags)) {
        spaced_tags[[2*i-1]] <- var_tags[[i]]
        if(i < length(var_tags)) spaced_tags[[2*i]] <- tags$span(" ")
      }
      
      do.call(tagList, spaced_tags)
    })
    
    output$selected_seasonal_vars <- renderUI({
      if(length(input$seasonal_vars) == 0) {
        return(HTML("<span style='color:#888;'>No variables selected</span>"))
      }
      
      var_tags <- lapply(input$seasonal_vars, function(var) {
        tags$span(var, class = "var-tag")
      })
      
      # Add spacing between tags
      spaced_tags <- list()
      for(i in seq_along(var_tags)) {
        spaced_tags[[2*i-1]] <- var_tags[[i]]
        if(i < length(var_tags)) spaced_tags[[2*i]] <- tags$span(" ")
      }
      
      do.call(tagList, spaced_tags)
    })
    
    # Use recommended model button
    observeEvent(input$use_recommended, {
      # Update UI with recommended variables based on your analysis results
      updateCheckboxGroupInput(session, "property_vars", 
                               selected = c("Size", "Floor", "Units", "Parking", "Year"))
      updateCheckboxGroupInput(session, "location_vars", 
                               selected = c("Dist. Subway", "Dist. CBD", "Pop. Density"))
      updateCheckboxGroupInput(session, "demographic_vars", 
                               selected = c("Higher Degree"))
      updateCheckboxGroupInput(session, "seasonal_vars", 
                               selected = c("Spring", "Fall", "Winter"))
      updateRadioButtons(session, "transform_type", selected = "log")
      
      # Run the model
      click(ns("run_mlr"))
    })
    
    # Run MLR analysis when the button is clicked
    observeEvent(input$run_mlr, {
      showLoading() # Show loading indicator
      
      # Ensure at least one variable is selected
      req(length(selected_variables()) > 0)
      
      # Get data and target variable
      data <- load_preprocessed_data()
      req(data)
      
      target_var <- get_target_variable()
      
      # Create formula for the model
      # 确保处理含有空格或特殊字符的变量名
      formula_parts <- sapply(selected_variables(), function(var) {
        paste0("`", var, "`")
      })
      formula_str <- paste(target_var, "~", paste(formula_parts, collapse = " + "))
      model_formula <- as.formula(formula_str)
      
      # Fit the model
      withProgress(message = 'Fitting MLR model...', {
        model <- lm(model_formula, data = data)
        
        # Calculate VIF
        vif_result <- calculate_vif(model_formula, data)
        
        # Check multicollinearity
        mc_result <- check_multicollinearity(data, threshold = 0.7)
        
        # Calculate standardized coefficients
        std_coef <- lm.beta::lm.beta(model)
        
        # Store results
        mlr_results$model <- model
        mlr_results$vif_data <- vif_result
        mlr_results$correlation_matrix <- mc_result$correlation_matrix
        mlr_results$high_correlations <- mc_result$high_correlation_pairs
        mlr_results$std_coefficients <- std_coef
        
        # Prepare residuals data for plots
        fitted_vals <- fitted(model)
        residuals_vals <- residuals(model)
        std_residuals <- rstandard(model)
        
        mlr_results$residuals_data <- data.frame(
          fitted = fitted_vals,
          residuals = residuals_vals,
          std_residuals = std_residuals,
          sqrt_std_residuals = sqrt(abs(std_residuals))
        )
        
        # Run diagnostic tests
        # Shapiro-Wilk test on a sample of residuals (since dataset is large)
        residuals_sample <- sample(residuals_vals, min(5000, length(residuals_vals)))
        mlr_results$shapiro_test_result <- shapiro.test(residuals_sample)
        
        # Breusch-Pagan test
        mlr_results$bp_test_result <- car::ncvTest(model)
        
        # Notify user that analysis is complete
        showNotification("MLR Analysis complete!", type = "message")
      })
      
      hideLoading() # Hide loading indicator
    })
    
    # Render model summary outputs
    output$regression_equation <- renderText({
      req(mlr_results$model)
      
      model <- mlr_results$model
      coef_values <- coef(model)
      equation <- paste(get_target_variable(), "=", round(coef_values[1], 4))
      
      for (i in 2:length(coef_values)) {
        var_name <- names(coef_values)[i]
        coef_value <- coef_values[i]
        
        if (coef_value >= 0) {
          equation <- paste(equation, "+", round(coef_value, 4), "*", var_name)
        } else {
          equation <- paste(equation, round(coef_value, 4), "*", var_name)
        }
      }
      
      return(equation)
    })
    
    output$coefficients_table <- renderDT({
      req(mlr_results$model)
      
      # Get model summary
      model_summary <- summary(mlr_results$model)
      coef_table <- as.data.frame(model_summary$coefficients)
      
      # Format column names
      colnames(coef_table) <- c("Estimate", "Std. Error", "t value", "p-value")
      
      # Add significance stars
      coef_table$Significance <- ""
      coef_table$Significance[coef_table$`p-value` < 0.001] <- "***"
      coef_table$Significance[coef_table$`p-value` >= 0.001 & coef_table$`p-value` < 0.01] <- "**"
      coef_table$Significance[coef_table$`p-value` >= 0.01 & coef_table$`p-value` < 0.05] <- "*"
      coef_table$Significance[coef_table$`p-value` >= 0.05 & coef_table$`p-value` < 0.1] <- "."
      
      # Create data table
      datatable(coef_table, 
                options = list(pageLength = 25,
                               scrollX = TRUE,
                               dom = 'Bfrtip'),
                rownames = TRUE) %>%
        formatRound(columns = c("Estimate", "Std. Error", "t value"), digits = 4) %>%
        formatSignif(columns = "p-value", digits = 3) %>%
        formatStyle('p-value',
                    color = styleInterval(c(0.001, 0.01, 0.05), 
                                          c('#2E6C40', '#2E6C40', '#2E6C40', 'black')),
                    fontWeight = styleInterval(0.05, c('bold', 'normal')))
    })
    
    output$r_squared <- renderText({
      req(mlr_results$model)
      model_summary <- summary(mlr_results$model)
      return(round(model_summary$r.squared, 4))
    })
    
    output$adj_r_squared <- renderText({
      req(mlr_results$model)
      model_summary <- summary(mlr_results$model)
      return(round(model_summary$adj.r.squared, 4))
    })
    
    output$f_statistic <- renderText({
      req(mlr_results$model)
      model_summary <- summary(mlr_results$model)
      f_stat <- model_summary$fstatistic
      f_value <- round(f_stat[1], 2)
      df1 <- f_stat[2]
      df2 <- f_stat[3]
      p_value <- pf(f_value, df1, df2, lower.tail = FALSE)
      
      if (p_value < 0.001) {
        p_text <- "p < 0.001"
      } else {
        p_text <- paste("p =", round(p_value, 4))
      }
      
      return(paste(f_value, "on", df1, "and", df2, "DF,", p_text))
    })
    
    output$residual_std_error <- renderText({
      req(mlr_results$model)
      model_summary <- summary(mlr_results$model)
      return(round(model_summary$sigma, 4))
    })
    
    # Render variable importance plot
    output$variable_importance_plot <- renderPlotly({
      req(mlr_results$std_coefficients)
      
      # Get standardized coefficients
      std_coef <- mlr_results$std_coefficients$standardized.coefficients
      
      # Create data frame for plotting
      std_df <- data.frame(
        Variable = names(std_coef),
        Coefficient = as.numeric(std_coef)
      )
      
      # Remove intercept
      std_df <- std_df[std_df$Variable != "(Intercept)", ]
      
      # Sort by absolute coefficient value
      std_df$AbsCoefficient <- abs(std_df$Coefficient)
      std_df <- std_df[order(std_df$AbsCoefficient, decreasing = TRUE), ]
      
      # Create color gradient based on coefficient values
      colors <- colorRampPalette(c("#A8D08D", "#2E6C40"))(nrow(std_df))
      
      # Create bar chart
      p <- plot_ly(
        std_df,
        x = ~reorder(Variable, AbsCoefficient),
        y = ~Coefficient,
        type = "bar",
        marker = list(color = colors),
        hoverinfo = "text",
        text = ~paste("Variable:", Variable, "<br>Standardized Coefficient:", round(Coefficient, 4))
      )
      
      # Layout
      p <- layout(
        p,
        title = list(
          text = "Variable Importance (Standardized Coefficients)",
          font = list(color = "#2E6C40")
        ),
        xaxis = list(
          title = list(
            text = "Variable",
            font = list(color = "#3B8C53")
          ),
          autorange = "reversed"
        ),
        yaxis = list(
          title = list(
            text = "Standardized Coefficient",
            font = list(color = "#3B8C53")
          )
        ),
        margin = list(t = 50, r = 50, b = 120),
        paper_bgcolor = "#F4F7F6",
        plot_bgcolor = "#F4F7F6"
      )
      
      return(p)
    })
    
    # Render importance interpretation
    output$importance_interpretation <- renderUI({
      req(mlr_results$std_coefficients)
      
      # Get standardized coefficients
      std_coef <- mlr_results$std_coefficients$standardized.coefficients
      
      # Remove intercept and create data frame
      std_df <- data.frame(
        Variable = names(std_coef),
        Coefficient = as.numeric(std_coef)
      )
      std_df <- std_df[std_df$Variable != "(Intercept)", ]
      
      # Sort by absolute coefficient value
      std_df$AbsCoefficient <- abs(std_df$Coefficient)
      std_df <- std_df[order(std_df$AbsCoefficient, decreasing = TRUE), ]
      
      # Get top 3 positive and negative variables (如果存在)
      top_positive <- if(any(std_df$Coefficient > 0)) {
        head(std_df[std_df$Coefficient > 0, ], 3)
      } else {
        data.frame(Variable = character(0), Coefficient = numeric(0), AbsCoefficient = numeric(0))
      }
      
      top_negative <- if(any(std_df$Coefficient < 0)) {
        head(std_df[std_df$Coefficient < 0, ], 3)
      } else {
        data.frame(Variable = character(0), Coefficient = numeric(0), AbsCoefficient = numeric(0))
      }
      
      # 创建积极因素的HTML
      positive_html <- if(nrow(top_positive) > 0) {
        paste0(
          "<h5 style='color: #2E6C40;'>Most Influential Positive Factors:</h5>",
          "<ul>",
          paste0("<li><strong>", top_positive$Variable, "</strong>: ", round(top_positive$Coefficient, 4), 
                 " (", round(top_positive$AbsCoefficient/sum(std_df$AbsCoefficient)*100, 1), "% influence)</li>", collapse = ""),
          "</ul>"
        )
      } else {
        "<h5 style='color: #2E6C40;'>No Positive Factors Found</h5>"
      }
      
      # 创建消极因素的HTML
      negative_html <- if(nrow(top_negative) > 0) {
        paste0(
          "<h5 style='color: #2E6C40;'>Most Influential Negative Factors:</h5>",
          "<ul>",
          paste0("<li><strong>", top_negative$Variable, "</strong>: ", round(top_negative$Coefficient, 4), 
                 " (", round(top_negative$AbsCoefficient/sum(std_df$AbsCoefficient)*100, 1), "% influence)</li>", collapse = ""),
          "</ul>"
        )
      } else {
        "<h5 style='color: #2E6C40;'>No Negative Factors Found</h5>"
      }
      
      # 组合显示文本
      HTML(paste(
        "<div style='color: #3B8C53; padding: 10px;'>",
        "<h4 style='color: #2E6C40;'>Key Findings:</h4>",
        "<p>The standardized coefficients represent the relative importance of each predictor variable in the model.</p>",
        positive_html,
        negative_html,
        "<p><em>Note: The standardized coefficients allow direct comparison between variables measured on different scales. 
        Variables with higher absolute values have a stronger effect on the property price.</em></p>",
        "</div>"
      ))
    })
    
    # Render correlation plot
    output$correlation_plot <- renderPlot({
      req(mlr_results$correlation_matrix)
      
      # Get correlation matrix
      cor_matrix <- mlr_results$correlation_matrix
      
      # Create correlation plot
      corrplot(cor_matrix, method = "color", 
               type = "upper", 
               tl.col = "#3B8C53",
               tl.srt = 45,
               addCoef.col = "black",
               number.cex = 0.7,
               diag = FALSE,
               col = colorRampPalette(c("#A8D08D", "white", "#2E6C40"))(200),
               title = "Correlation Matrix of Numeric Variables",
               mar = c(0, 0, 2, 0))
    })
    
    # Render VIF plot
    output$vif_plot <- renderPlotly({
      req(mlr_results$vif_data)
      
      # Get VIF data
      vif_df <- mlr_results$vif_data
      
      # Set threshold
      threshold <- input$vif_threshold
      
      # Create bar chart
      p <- plot_ly(
        vif_df,
        x = ~reorder(Variable, VIF),
        y = ~VIF,
        type = "bar",
        marker = list(
          color = ~ifelse(VIF > threshold, "#E46726", "#A8D08D")
        ),
        hoverinfo = "text",
        text = ~paste("Variable:", Variable, "<br>VIF:", round(VIF, 2))
      )
      
      # Add threshold line
      p <- p %>% add_trace(
        x = vif_df$Variable,
        y = rep(threshold, nrow(vif_df)),
        type = "scatter",
        mode = "lines",
        line = list(color = "#E46726", width = 2, dash = "dash"),
        name = "Threshold",
        hoverinfo = "text",
        text = paste("Threshold:", threshold)
      )
      
      # Layout
      p <- layout(
        p,
        title = list(
          text = "Variance Inflation Factors",
          font = list(color = "#2E6C40")
        ),
        xaxis = list(
          title = list(
            text = "Variable",
            font = list(color = "#3B8C53")
          ),
          autorange = "reversed"
        ),
        yaxis = list(
          title = list(
            text = "VIF",
            font = list(color = "#3B8C53")
          ),
          rangemode = "tozero"
        ),
        margin = list(t = 50, r = 50, b = 120),
        paper_bgcolor = "#F4F7F6",
        plot_bgcolor = "#F4F7F6",
        showlegend = FALSE
      )
      
      return(p)
    })
    
    # Render high correlation table
    output$high_correlation_table <- renderDT({
      req(mlr_results$high_correlations)
      
      # Get high correlation pairs
      high_cor_df <- mlr_results$high_correlations
      
      if(nrow(high_cor_df) == 0) {
        return(data.frame(Message = "No high correlations detected (r > 0.7)"))
      }
      
      # Create data table
      datatable(high_cor_df,
                options = list(pageLength = 10,
                               dom = 'Bfrtip'),
                rownames = FALSE) %>%
        formatRound(columns = c("correlation"), digits = 3) %>%
        formatStyle('correlation',
                    backgroundColor = styleInterval(c(0.8, 0.9), 
                                                    c('#A8D08D', '#3B8C53', '#2E6C40')),
                    color = styleInterval(0.9, c('black', 'white')))
    })
    
    # Render residual plots
    output$residual_fitted_plot <- renderPlot({
      req(mlr_results$residuals_data)
      
      # Create residuals vs fitted plot
      ggplot(mlr_results$residuals_data, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.3, color = "#3B8C53") +
        geom_smooth(method = "loess", color = "#2E6C40", se = FALSE) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#E46726") +
        labs(x = "Fitted values", y = "Residuals",
             title = "Residuals vs Fitted") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#2E6C40", face = "bold"),
          axis.title = element_text(color = "#3B8C53"),
          axis.text = element_text(color = "#3B8C53"),
          panel.background = element_rect(fill = "#F4F7F6"),
          plot.background = element_rect(fill = "#F4F7F6")
        )
    })
    
    output$qq_plot <- renderPlot({
      req(mlr_results$residuals_data)
      
      # Create Q-Q plot
      ggplot(mlr_results$residuals_data, aes(sample = std_residuals)) +
        stat_qq(color = "#3B8C53") +
        stat_qq_line(color = "#E46726") +
        labs(x = "Theoretical Quantiles", y = "Standardized Residuals",
             title = "Normal Q-Q Plot") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#2E6C40", face = "bold"),
          axis.title = element_text(color = "#3B8C53"),
          axis.text = element_text(color = "#3B8C53"),
          panel.background = element_rect(fill = "#F4F7F6"),
          plot.background = element_rect(fill = "#F4F7F6")
        )
    })
    
    output$scale_location_plot <- renderPlot({
      req(mlr_results$residuals_data)
      
      # Create scale-location plot
      ggplot(mlr_results$residuals_data, aes(x = fitted, y = sqrt_std_residuals)) +
        geom_point(alpha = 0.3, color = "#3B8C53") +
        geom_smooth(method = "loess", color = "#2E6C40", se = FALSE) +
        labs(x = "Fitted values", y = "√|Standardized residuals|",
             title = "Scale-Location Plot") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#2E6C40", face = "bold"),
          axis.title = element_text(color = "#3B8C53"),
          axis.text = element_text(color = "#3B8C53"),
          panel.background = element_rect(fill = "#F4F7F6"),
          plot.background = element_rect(fill = "#F4F7F6")
        )
    })
    
    output$residual_histogram <- renderPlot({
      req(mlr_results$residuals_data)
      
      # Create residuals histogram
      ggplot(mlr_results$residuals_data, aes(x = residuals)) +
        geom_histogram(bins = 30, fill = "#A8D08D", color = "#2E6C40", alpha = 0.7) +
        geom_density(color = "#E46726", linewidth = 1) +
        labs(x = "Residuals", y = "Count",
             title = "Histogram of Residuals") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#2E6C40", face = "bold"),
          axis.title = element_text(color = "#3B8C53"),
          axis.text = element_text(color = "#3B8C53"),
          panel.background = element_rect(fill = "#F4F7F6"),
          plot.background = element_rect(fill = "#F4F7F6")
        )
    })
    
    # Render diagnostic test results
    output$shapiro_test <- renderText({
      req(mlr_results$shapiro_test_result)
      
      test_result <- mlr_results$shapiro_test_result
      p_value <- test_result$p.value
      
      if(p_value < 0.001) {
        return(paste("W =", round(test_result$statistic, 3), ", p < 0.001 (Non-normal)"))
      } else if(p_value < 0.05) {
        return(paste("W =", round(test_result$statistic, 3), ", p =", round(p_value, 3), "(Non-normal)"))
      } else {
        return(paste("W =", round(test_result$statistic, 3), ", p =", round(p_value, 3), "(Normal)"))
      }
    })
    
    output$bp_test <- renderText({
      req(mlr_results$bp_test_result)
      
      test_result <- mlr_results$bp_test_result
      p_value <- test_result$p
      
      if(p_value < 0.001) {
        return(paste("χ² =", round(test_result$ChiSquare, 3), ", p < 0.001 (Heteroscedastic)"))
      } else if(p_value < 0.05) {
        return(paste("χ² =", round(test_result$ChiSquare, 3), ", p =", round(p_value, 3), "(Heteroscedastic)"))
      } else {
        return(paste("χ² =", round(test_result$ChiSquare, 3), ", p =", round(p_value, 3), "(Homoscedastic)"))
      }
    })
  })
}