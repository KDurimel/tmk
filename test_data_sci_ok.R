# ----- LIBS ---------
library(shiny)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(readr)
library(shinydashboard)
library(ggplot2)
library(plotly)

# ----- DATA ---------

# Dummy data CSV
df <- read.csv("data.csv", header = TRUE)

# ----- SHINY UI ---------
ui <- dashboardPage(
  dashboardHeader(title = "Customer Retention Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Dashboard alt", tabName = "dashboard_2", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          box(
            title = "Date Range Selection",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            dateRangeInput("dateRange", "Select Date Range:",
                           start = Sys.Date() - 30, end = Sys.Date(),
                           format = "yyyy-mm-dd")
          ),
          box(
            title = "View Type",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("viewType", "Select View Type:",
                        choices = c("New Clients Cabinet", "New Clients Groupe", "Revenue", "Cohort monthly"),
                        selected = "New Clients Cabinet")
          )
        ),
        fluidRow(
          box(DTOutput("dataTable"), width = 12, title = "Data Table", status = "primary", solidHeader = TRUE, collapsible = TRUE),
          box(plotlyOutput("chart"), width = 12, title = "Chart", status = "primary", solidHeader = TRUE, collapsible = TRUE)
        )
      ),
      tabItem(tabName = "dashboard_2",
        # Todo maybe
      )
    )
  )
)


# ----- SHINY SERVER ---------
server <- function(input, output, session) {
  filteredData <- reactive({
    startDate <- as.Date(input$dateRange[1], format = "%Y/%m/%d")
    endDate <- as.Date(input$dateRange[2], format = "%Y/%m/%d")
    
    df_filtered <- df %>%
      filter(date_consultation >= startDate & date_consultation <= endDate)
    
    return(df_filtered)
  })
  
  # Observe changes in viewType and update dateRange for "Cohort monthly"
  observe({
    if (input$viewType == "Cohort monthly") {
      updateDateRangeInput(session, "dateRange", start = "2023-01-01", end = "2023-12-31")
    }
  })
  
  output$dataTable <- renderDT({
    df_to_show <- if (input$viewType == "New Clients Cabinet") {
      filteredData() %>% filter(numero_consultation_payante == 1)
    } else if (input$viewType == "New Clients Groupe") {
      filteredData() %>% filter(numero_consultation_groupe == 1)
    } else if (input$viewType == "Cohort monthly") {
      render_cohort_table(filteredData())
    } else {
      filteredData()
    }
    if (input$viewType == "Cohort monthly") {
      datatable(df_to_show, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 15,
        lengthMenu = list(c(5, 10, 15, 25, 50, -1), c('5', '10', '15', '25', '50', 'All')),
        autoWidth = TRUE,
        responsive = TRUE,
        options = list(pageLength = 15, lengthMenu = list(c(5, 10, 15, 25, 50, -1), c('5', '10', '15', '25', '50', 'All'))),
        class = 'cell-border stripe'
      ))
    } else {
      datatable(df_to_show, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      pageLength = 5,
      lengthMenu = list(c(5, 10, 15, 25, 50, -1), c('5', '10', '15', '25', '50', 'All')),
      autoWidth = TRUE,
      responsive = TRUE,
      class = 'cell-border stripe'
    ))
    }
  })
  
  output$chart <- renderPlotly({
    # Base plot with ggplot2
    p <- ggplot() + theme_minimal()
    
    if (input$viewType == "New Clients Cabinet") {
      p <- ggplot(filteredData() %>% filter(numero_consultation_payante == 1), aes(x = date_consultation)) +
        geom_bar(stat = "count", fill = "skyblue") +
        labs(title = "New Clients (Cabinet) Over Time", x = "Date", y = "Count")
    } else if (input$viewType == "New Clients Groupe") {
      p <- ggplot(filteredData() %>% filter(numero_consultation_groupe == 1), aes(x = date_consultation)) +
        geom_bar(stat = "count", fill = "lightcoral") +
        labs(title = "New Clients (Groupe) Over Time", x = "Date", y = "Count")
    } else {
      p <- ggplot(filteredData(), aes(x = date_consultation, y = montant)) +
        geom_line(color = "purple") +
        labs(title = "Revenue Over Time", x = "Date", y = "Total Revenue")
    }
    
    # Convert ggplot object to plotly
    ggplotly(p)
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
