# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)

# Data cleaning functions
# Function to clean percentage columns and handle NA values
clean_pct_column <- function(x) {
  x[x %in% c("NA", "N/A", "", "NULL")] <- NA
  x <- gsub("[%\\s]", "", as.character(x))
  as.numeric(x)
}

# Function to prepare data by cleaning all PCT columns
data_prep <- function(data) {
  pct_cols <- grep("_PCT$", names(data), value = TRUE)
  
  cleaned_data <- data %>%
    mutate(across(all_of(pct_cols), ~ifelse(. == "NA" | . == "", NA, .))) %>%
    mutate(across(all_of(pct_cols), clean_pct_column)) %>%
    filter(!is.na(.data[[pct_cols[1]]]))
  
  return(cleaned_data)
}

# Function to get and format PCT variable names for dropdowns
get_pct_vars <- function(data) {
  vars <- names(data)[grep("_PCT$", names(data))]
  names(vars) <- gsub("_", " ", vars)
  names(vars) <- str_wrap(names(vars), width = 40)
  vars
}

# UI Definition
# Custom CSS styling for the app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            /* Main styling elements */
            body {
                background-color: #FFFFFF;
                color: #333333;
                font-family: Arial, sans-serif;
                padding: 20px;
            }
            /* Container and layout styling */
            .container-fluid {
                max-width: 1400px;
                margin: 0 auto;
                padding: 0 20px;
            }
            /* Sidebar panel styling */
            .well {
                background-color: #F8F9FA;
                border: 1px solid #E9ECEF;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.05);
                margin-right: 20px;
                padding: 20px;
                min-width: 350px;
            }
            /* Dropdown styling */
            .selectize-input {
                width: 100% !important;
                min-width: 300px;
                max-width: 350px;
            }
            .selectize-dropdown {
                width: 100% !important;
                max-width: 350px;
            }
            /* Logo styling */
            .logo-container {
                text-align: center;
                margin-bottom: 30px;
                padding: 20px;
            }
            .logo {
                max-width: 400px;
                height: auto;
            }
            /* Header styling */
            h2 {
                color: #84C5A8;
                font-weight: bold;
            }
            /* Plot styling */
            #scatterplot {
                width: 100% !important;
                min-height: 600px;
            }
            /* Main panel spacing */
            .col-sm-8 {
                padding-left: 30px;
            }
        "))
  ),
  
  # Logo section
  fluidRow(
    column(12,
           div(class = "logo-container",
               img(src = "mcpsa_logo.png", class = "logo", alt = "MCPSA Logo")
           )
    )
  ),
  
  # Title section
  titlePanel(div(style = "color: #84C5A8;", "School Performance Comparison")),
  
  # Main layout
  sidebarLayout(
    # Left sidebar with controls
    sidebarPanel(width = 4,
                 div(style = "max-width: 350px;",
                     selectizeInput("xvar", "X-axis Variable:", 
                                    choices = NULL,
                                    options = list(dropdownParent = 'body')),
                     selectizeInput("yvar", "Y-axis Variable:", 
                                    choices = NULL,
                                    options = list(dropdownParent = 'body')),
                     selectInput("school_select", "Select School:", 
                                 choices = NULL)
                 )
    ),
    
    # Main panel with plot
    mainPanel(width = 8,
              plotOutput("scatterplot")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Load data
  apr_2024 <- suppressWarnings(read_excel("data/2024 Mo APR .xlsx"))
  
  # Update variable selection dropdowns
  observe({
    pct_vars <- get_pct_vars(apr_2024)
    updateSelectizeInput(session, "xvar", choices = pct_vars)
    updateSelectizeInput(session, "yvar", choices = pct_vars)
  })
  
  # Update school selection dropdown with sorted names
  observe({
    filtered_schools <- apr_2024 %>%
      data_prep() %>%
      filter(TYPE %in% c("Charter", "Average")) %>%
      pull(DISTRICT_NAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "school_select", choices = filtered_schools)
  })
  
  # Create scatter plot
  output$scatterplot <- renderPlot({
    req(input$xvar, input$yvar)
    
    # Prepare and filter data
    plot_data <- suppressWarnings(data_prep(apr_2024)) %>%
      filter(!is.na(.data[[input$xvar]]), !is.na(.data[[input$yvar]]))
    
    # Calculate axis limits
    x_min <- max(0, floor(min(plot_data[[input$xvar]], na.rm = TRUE) - 10))
    y_min <- max(0, floor(min(plot_data[[input$yvar]], na.rm = TRUE) - 10))
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = .data[[input$xvar]],
                    y = .data[[input$yvar]])) +
      # Add points with color and transparency
      geom_point(aes(color = TYPE, alpha = TYPE), size = 4) +
      # Set color scheme
      scale_color_manual(values = c(
        "Average" = "#FFD700",
        "Charter" = "#84C5A8",
        "District" = "grey50"
      ), labels = c("Average", "Charter", "District"),
      name = "School Type") +
      # Set transparency
      scale_alpha_manual(values = c(
        "Average" = 1,
        "Charter" = 1,
        "District" = 0.15
      ), guide = "none") +
      # Configure axes
      scale_x_continuous(
        limits = c(x_min, 100), 
        breaks = seq(0, 100, 10),
        labels = function(x) paste0(x, "%")
      ) +
      scale_y_continuous(
        limits = c(y_min, 100), 
        breaks = seq(0, 100, 10),
        labels = function(x) paste0(x, "%")
      ) +
      # Add highlighted point for selected school
      {if (!is.null(input$school_select)) {
        geom_point(
          data = filter(plot_data, 
                        DISTRICT_NAME == input$school_select),
          color = "#E84855",
          size = 4
        )
      }} +
      # Apply theme and styling
      theme_minimal() +
      theme(
        text = element_text(family = "Arial"),
        plot.title = element_text(size = 16, face = "bold", color = "#333333"),
        axis.title = element_text(size = 12, color = "#666666"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#E9ECEF"),
        plot.background = element_rect(fill = "white", color = NA)
      ) +
      # Add labels
      labs(x = gsub("_", " ", input$xvar),
           y = gsub("_", " ", input$yvar))
    
    print(p)
  }, height = 600)
}

# Run the app
shinyApp(ui = ui, server = server)