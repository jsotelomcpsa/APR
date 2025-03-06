# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)
library(ggrepel)  # For adding text labels that don't overlap

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
  # Create human-readable names
  readable_names <- gsub("_PCT$", "", vars)  # Remove PCT suffix
  readable_names <- gsub("_", " ", readable_names)  # Replace underscores with spaces
  # Capitalize first letter of each word
  readable_names <- sapply(strsplit(readable_names, " "), function(x) {
    paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
  })
  names(vars) <- readable_names
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
            /* Table styling */
            .percentile-table {
                width: 100%;
                border-collapse: collapse;
                margin-top: 20px;
            }
            .percentile-table th, .percentile-table td {
                padding: 8px;
                text-align: left;
                border-bottom: 1px solid #ddd;
            }
            .percentile-table th {
                background-color: #f2f2f2;
                font-weight: bold;
            }
            .percentile-table tr:hover {
                background-color: #f5f5f5;
            }
            .selected-school {
                font-weight: bold;
                color: #E84855;
            }
            .average-school {
                font-weight: bold;
                color: #FFD700;
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
                     selectizeInput("school_select", "Select Schools:", 
                                    choices = NULL, 
                                    multiple = TRUE,
                                    options = list(
                                      placeholder = 'Select one or more schools',
                                      plugins = list('remove_button')
                                    ))
                 )
    ),
    
    # Main panel with plot and percentile table
    mainPanel(width = 8,
              plotOutput("scatterplot"),
              div(class = "table-container",
                  tableOutput("percentile_table")
              )
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
  
  # Update school selection dropdown with sorted names - "Average" at the top
  observe({
    all_schools <- apr_2024 %>%
      data_prep() %>%
      filter(TYPE %in% c("Charter", "Average")) %>%
      pull(DISTRICT_NAME) %>%
      unique()
    
    # Put Average at the top, then sort all others alphabetically
    average_school <- all_schools[grepl("Average", all_schools)]
    other_schools <- setdiff(all_schools, average_school) %>% sort()
    
    filtered_schools <- c(average_school, other_schools)
    
    updateSelectizeInput(session, "school_select", choices = filtered_schools)
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
    
    # Create readable axis labels
    x_label <- gsub("_PCT$", "", input$xvar)  # Remove PCT suffix
    x_label <- gsub("_", " ", x_label)  # Replace underscores with spaces
    # Capitalize first letter of each word
    x_label <- sapply(strsplit(x_label, " "), function(x) {
      paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
    })
    
    y_label <- gsub("_PCT$", "", input$yvar)
    y_label <- gsub("_", " ", y_label)
    y_label <- sapply(strsplit(y_label, " "), function(x) {
      paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
    })
    
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
      # Add highlighted points for selected schools
      {if (!is.null(input$school_select) && length(input$school_select) > 0) {
        geom_point(
          data = filter(plot_data, 
                        DISTRICT_NAME %in% input$school_select),
          color = "#E84855",
          size = 5
        )
      }} +
      # Add labels for selected schools
      {if (!is.null(input$school_select) && length(input$school_select) > 0) {
        geom_text_repel(
          data = filter(plot_data, DISTRICT_NAME %in% input$school_select),
          aes(label = DISTRICT_NAME),
          size = 4,
          point.padding = 0.5,
          box.padding = 0.5,
          force = 2,
          color = "#E84855"
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
      labs(x = paste0(x_label, " (%)"),
           y = paste0(y_label, " (%)"))
    
    print(p)
  }, height = 600)
  
  # Create percentile table for selected schools
  output$percentile_table <- renderTable({
    req(input$xvar, input$yvar, input$school_select)
    
    if (length(input$school_select) == 0) {
      return(NULL)
    }
    
    # Prepare data
    plot_data <- suppressWarnings(data_prep(apr_2024)) %>%
      filter(!is.na(.data[[input$xvar]]), !is.na(.data[[input$yvar]]))
    
    # Calculate percentiles for all schools
    x_percentiles <- ecdf(plot_data[[input$xvar]])
    y_percentiles <- ecdf(plot_data[[input$yvar]])
    
    # Get data for selected schools
    selected_schools <- plot_data %>%
      filter(DISTRICT_NAME %in% input$school_select) %>%
      mutate(
        x_value = .data[[input$xvar]],
        y_value = .data[[input$yvar]],
        x_percentile = round(x_percentiles(x_value) * 100, 1),
        y_percentile = round(y_percentiles(y_value) * 100, 1)
      ) %>%
      select(DISTRICT_NAME, TYPE, x_value, x_percentile, y_value, y_percentile)
    
    # Create readable column names
    x_name <- gsub("_PCT$", "", input$xvar)
    x_name <- gsub("_", " ", x_name)
    x_name <- sapply(strsplit(x_name, " "), function(x) {
      paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
    })
    
    y_name <- gsub("_PCT$", "", input$yvar)
    y_name <- gsub("_", " ", y_name)
    y_name <- sapply(strsplit(y_name, " "), function(x) {
      paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
    })
    
    # Format the table
    result_table <- selected_schools %>%
      mutate(
        `School Name` = DISTRICT_NAME,
        `School Type` = TYPE,
        `X Value` = paste0(round(x_value, 1), "%"),
        `X Percentile` = paste0(x_percentile, "%"),
        `Y Value` = paste0(round(y_value, 1), "%"),
        `Y Percentile` = paste0(y_percentile, "%")
      ) %>%
      select(`School Name`, `School Type`, `X Value`, `X Percentile`, `Y Value`, `Y Percentile`)
    
    # Rename columns with actual metric names
    names(result_table)[3:6] <- c(
      paste0(x_name, " Value"), 
      paste0(x_name, " Percentile"),
      paste0(y_name, " Value"), 
      paste0(y_name, " Percentile")
    )
    
    return(result_table)
  }, sanitize.text.function = function(x) x)
}

# Run the app
shinyApp(ui = ui, server = server)
