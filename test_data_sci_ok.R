# ----- LIBS ---------
library(shiny)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(readr)

# ----- DATA ---------

# Dummy data CSV
df <- read.csv("data.csv", header = TRUE)

# ----- SHINY UI ---------
ui <- fluidPage(
  titlePanel("Customer Retention"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(df$date_consultation), end = max(df$date_consultation),
                     format = "yyyy/m/dd"),
      selectInput("viewType", "Select View Type:",
                  choices = c("New Clients Cabinet", "New Clients Groupe", "Revenue", "Cohort monthly"),
                  selected = "New Clients Cabinet")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DTOutput("dataTable")),
        tabPanel("Chart", plotOutput("chart"))
      )
    )
  )
)

# ----- SHINY SERVER ---------
server <- function(input, output) {
  filteredData <- reactive({
    startDate <- as.Date(input$dateRange[1], format = "%Y/%m/%d")
    endDate <- as.Date(input$dateRange[2], format = "%Y/%m/%d")
    
    df_filtered <- df %>%
      filter(date_consultation >= startDate & date_consultation <= endDate)
    
    return(df_filtered)
  })
  
  output$dataTable <- renderDT({
    if (input$viewType == "New Clients Cabinet") {
      datatable(filteredData() %>%
                  filter(numero_consultation_payante == 1),
                options = list(pageLength = 5))
    } else if (input$viewType == "New Clients Groupe") {
      datatable(filteredData() %>%
                  filter(numero_consultation_groupe == 1),
                options = list(pageLength = 5))
    } else if (input$viewType == "Cohort monthly") {
      datatable(render_cohort_table(filteredData()))
    } else {
      datatable(filteredData(),
                options = list(pageLength = 5))
    }
  })
  
  output$chart <- renderPlot({
    if (input$viewType == "New Clients Cabinet") {
      ggplot(filteredData() %>% filter(numero_consultation_payante == 1),
             aes(x = date_consultation)) +
        geom_bar(stat = "count", fill = "skyblue") +
        labs(title = "New Clients (Cabinet) Over Time", x = "Date", y = "Count")
    } else if (input$viewType == "New Clients Groupe") {
      ggplot(filteredData() %>% filter(numero_consultation_groupe == 1),
             aes(x = date_consultation)) +
        geom_bar(stat = "count", fill = "lightcoral") +
        labs(title = "New Clients (Groupe) Over Time", x = "Date", y = "Count")
    } else {
      ggplot(filteredData(), aes(x = date_consultation, y = montant)) +
        geom_line(color = "purple") +
        labs(title = "Revenue Over Time", x = "Date", y = "Total Revenue")
    }
  })
  # ****** Process data for cohort   ****** #
  render_cohort_table <- function(df) {
    # Check for required columns
    required_columns <- c("date_consultation", "id_client", "numero_consultation_payante", "numero_consultation_groupe")
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      stop("Missing required columns: ", paste(missing_columns, collapse=", "))
    }
    # Convert date_consultation to Date type if it's not already
    df$date_consultation <- as.Date(df$date_consultation, format="%Y-%m-%d")
    
    # Remove rows with NA in date_consultation
    df <- df %>% filter(!is.na(date_consultation))
    
    # Identify new clients and link to cohorts
    new_clients <- df %>%
      filter(numero_consultation_payante == 1 | numero_consultation_groupe == 1) %>%
      mutate(Cohort = floor_date(date_consultation, "month")) %>%
      group_by(id_client) %>%
      summarise(Cohort = min(Cohort), .groups = 'drop')
    
    # Count the number of new clients per cohort
    new_clients_per_cohort <- new_clients %>%
      group_by(Cohort) %>%
      summarise(NewClients = n(), .groups = 'drop')
    
    # Merge this back with the original df to get a complete dataframe including all months
    df <- df %>%
      left_join(new_clients, by = "id_client")
    
    # For each cohort, count the activity each month
    cohort_activity <- df %>%
      group_by(Cohort, Month = floor_date(date_consultation, "month")) %>%
      summarise(ActiveClients = n_distinct(id_client), .groups = 'drop')
    
    # Create the cohort table with months as columns and cohorts as rows
    cohort_table <- cohort_activity %>%
      pivot_wider(names_from = Month, values_from = ActiveClients, values_fill = list(ActiveClients = 0)) %>%
      left_join(new_clients_per_cohort, by = "Cohort") %>%
      arrange(Cohort)
    
    # Convert Cohort to character for consistent data type
    cohort_table$Cohort <- as.character(cohort_table$Cohort)
    
    # Apply the condition to set values to NA for months before the cohort's start month
    month_cols <- setdiff(names(cohort_table), c("Cohort", "NewClients"))
    for (col in month_cols) {
      cohort_table[[col]] <- ifelse(cohort_table$Cohort > col, NA, cohort_table[[col]])
    }
    
    # Sum across all rows for each column
    cohort_table <- cohort_table %>%
      mutate(Total = rowSums(select(., -Cohort, -NewClients), na.rm = TRUE))
    
    # Check if the dataframe is ungrouped before summarization
    cohort_table <- ungroup(cohort_table)
    total_row <- cohort_table %>%
      summarise(across(-Cohort, ~sum(.x, na.rm = TRUE)))
    
    # Redesign and cleaning to have a table compliant with Tmk table shown as requirement (img w/ last table)
    total_row$Cohort <- "Total"
    
    cohort_table <- bind_rows(cohort_table, total_row) # Combine the total row with the existing table
    
    cohort_table <- select(cohort_table, -NewClients) # Exclude the NewClients column from the final table
    
    cohort_table <- cohort_table %>% filter(Cohort != "NA") # Filter out rows where 'Cohort' is NA
    
    return(cohort_table)
  }
}

# ----- RUN ---------
shinyApp(ui = ui, server = server)
