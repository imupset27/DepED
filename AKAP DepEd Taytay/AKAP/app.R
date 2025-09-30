#install.packages("googlesheets4")
library(shiny)
library(googlesheets4)
library(dplyr)

# Authorize googlesheets4 with the service account JSON
#gs4_auth(path = "path/to/your/service-account-file.json")

# Google Sheet ID
sheet_id <- "1y6oAl3bKtWEMa4kWC4b6Jq-f9MiNB_kOrS6GQzRYqPY"

# Read data from Google Sheet
read_sheet_data <- function() {
  read_sheet(sheet_id)
}

# Append data to Google Sheet
append_sheet_data <- function(new_data) {
  sheet_append(sheet_id, new_data)
}

ui <- fluidPage(
  titlePanel("Shiny App with Google Sheets"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("new_data", "New Data:", ""),
      actionButton("add", "Add Data")
    ),
    
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(read_sheet_data())
  
  observeEvent(input$add, {
    new_data <- tibble(Column1 = input$new_data)
    append_sheet_data(new_data)
    data(read_sheet_data())
  })
  
  output$table <- renderTable({
    data()
  })
}

shinyApp(ui, server)
