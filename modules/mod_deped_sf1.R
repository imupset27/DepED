# modules/lrn_extractor.R
library(shiny)
library(readxl)
library(DT)
library(openxlsx)
library(stringr)

# -------------------- UI --------------------
mod_deped_sf1_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Extract LRN, Name, and Gender"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Excel File", accept = c(".xlsx", ".xls")),
        downloadButton(ns("download_excel"), "Download Excel"),
        downloadButton(ns("download_csv"), "Download CSV")
      ),
      mainPanel(
        DTOutput(ns("table"))
      )
    )
  )
}


# -------------------- SERVER --------------------
mod_deped_sf1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      req(input$file)
      
      # Skip first 3 rows to get proper headers
      df <- read_excel(input$file$datapath, skip = 3)
      
      # ✅ LRN = col1, Name = col3, Gender = col7
      df_subset <- df[, c(1, 3, 7)]
      names(df_subset) <- c("LRN", "Name", "Gender")
      
      # Keep rows with either LRN or Name
      df_clean <- df_subset[!(is.na(df_subset$LRN) & is.na(df_subset$Name)), ]
      
      # Trim spaces
      df_clean$LRN    <- str_trim(as.character(df_clean$LRN))
      df_clean$Name   <- str_trim(as.character(df_clean$Name))
      df_clean$Gender <- str_trim(as.character(df_clean$Gender))
      
      # Standardize Gender (M/F only)
      df_clean$Gender <- toupper(df_clean$Gender)
      df_clean$Gender <- ifelse(df_clean$Gender %in% c("M", "MALE"), "M",
                                ifelse(df_clean$Gender %in% c("F", "FEMALE"), "F", NA))
      
      # Keep valid LRNs = exactly 12 digits
      df_clean$LRN <- ifelse(grepl("^\\d{12}$", df_clean$LRN), df_clean$LRN, NA)
      
      # Remove unwanted rows in Name
      remove_patterns <- c("TOTAL MALE", "TOTAL FEMALE", "COMBINED", "NAME")
      df_clean <- df_clean[!grepl(paste(remove_patterns, collapse = "|"), 
                                  toupper(df_clean$Name)), ]
      
      # ✅ Strict removal of empty rows: if both LRN and Name are missing
      df_clean <- df_clean[!( (is.na(df_clean$LRN) | df_clean$LRN == "") & 
                                (is.na(df_clean$Name) | df_clean$Name == "") ), ]
      
      # === Add FormattedName column (3-part split with multi-word middle) ===
      df_clean$FormattedName <- toupper(df_clean$Name)
      
      df_clean$FormattedName <- sapply(df_clean$FormattedName, function(fullname) {
        parts <- str_split(fullname, ",")[[1]]
        if (length(parts) == 3) {
          last   <- str_trim(parts[1])   # LASTNAME
          first  <- str_trim(parts[2])   # FIRSTNAME
          middle <- str_trim(parts[3])   # MIDDLENAME
          
          if (middle == "-" | middle == "") {
            paste0(last, ", ", first)
          } else {
            # Split middle name into words (e.g., "DELA CRUZ")
            middle_parts <- str_split(middle, "\\s+")[[1]]
            initials <- paste0(substr(middle_parts, 1, 1), collapse = "")
            paste0(last, ", ", first, " ", initials, ".")
          }
        } else {
          fullname  # fallback if malformed
        }
      })
      
      return(df_clean)
    })
    
    # Show cleaned table
    output$table <- renderDT({
      datatable(data(), options = list(pageLength = 20))
    })
    
    # Download as Excel (LRN always text)
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("LRN_Name_Gender_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- data()
        
        # ✅ Ensure LRN is text
        df$LRN <- as.character(df$LRN)
        
        wb <- createWorkbook()
        addWorksheet(wb, "Data")
        
        # Write data
        writeData(wb, "Data", df)
        
        # ✅ Apply text format to entire LRN column
        text_style <- createStyle(numFmt = "@")
        addStyle(
          wb, sheet = "Data",
          style = text_style,
          cols = 1, rows = 2:(nrow(df) + 1),
          gridExpand = TRUE, stack = TRUE
        )
        
        # Lock column width
        setColWidths(wb, "Data", cols = 1, widths = 20)
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # Download as CSV
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("LRN_Name_Gender_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- data()
        df$LRN <- as.character(df$LRN)  # ✅ Keep as text in CSV
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}

