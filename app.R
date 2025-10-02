library(shiny)
library(readxl)
library(tidyverse)
library(openxlsx)
library(auth0)

ui <- fluidPage(
  titlePanel("📚 Consolidated Student Grades (Long Format)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Upload Multiple Excel Files", 
                multiple = TRUE, accept = c(".xlsx", ".xls")),
      actionButton("process", "Extract & Consolidate"),
      br(), br(),
      downloadButton("download_excel", "📥 Download Consolidated Data"),
      br(),
      br(),
      
      tags$a("📄 Download Template File", 
             href = "template.zip", 
             target = "_blank", 
             download = NA, 
             class = "btn btn-primary")
      
    ),
    
    mainPanel(
      h3("Preview of Consolidated Data"),
      tableOutput("preview")
    )
  )
)

server <- function(input, output, session) {
  
  subject_order <- c("FILIPINO", "ENGLISH", "MATHEMATICS", "SCIENCE", 
                     "ARALING PANLIPUNAN", "EPP", "MAPEH", "MUSIC", "ARTS", 
                     "PHYSICAL EDUCATION", "HEALTH","EDUKASYON SA PAGPAPAKATAO")
  
  merged_data <- eventReactive(input$process, {
    req(input$files)
    
    read_one_file <- function(path) {
      sheets_all <- tryCatch(excel_sheets(path), error = function(e) character(0))
      if (!("SUMMARY OF QUARTERLY GRADES" %in% sheets_all)) return(NULL)
      
      q_matches <- str_extract(sheets_all, "_Q[1-4]")
      q_matches <- q_matches[!is.na(q_matches)]
      if (length(q_matches) == 0) {
        q_match_file <- str_extract(path, "_Q[1-4]")
        q_num <- if (!is.na(q_match_file)) as.integer(str_extract(q_match_file, "[1-4]")) else NA_integer_
      } else {
        q_num <- as.integer(str_extract(q_matches[1], "[1-4]"))
      }
      quarter_col <- if (is.na(q_num)) NA_character_ else paste0("Q", q_num)
      
      subject_name <- tryCatch({
        sn <- read_excel(path, sheet = "INPUT DATA", range = "AI7:AI7",
                         col_names = FALSE) %>% pull(1) %>% as.character() %>% str_squish() %>% toupper()
        if (length(sn) == 0 || is.na(sn) || sn == "") "Unknown_Subject" else sn
      }, error = function(e) "Unknown_Subject")
      
      summary_raw <- tryCatch({
        read_excel(path, sheet = "SUMMARY OF QUARTERLY GRADES",
                   range = "B13:AB113", col_names = FALSE)
      }, error = function(e) NULL)
      if (is.null(summary_raw)) return(NULL)
      if (ncol(summary_raw) < 27) return(NULL)
      
      summary_selected <- summary_raw[, c(1,2,3,7,11,15,19,23,27)]
      colnames(summary_selected) <- c("Gender", "LRN", "Name", "Q1","Q2","Q3","Q4","Final","Remark")
      
      base <- summary_selected %>%
        mutate(Name = str_squish(replace_na(as.character(Name), ""))) %>%
        filter(Name != "", Name != "0") %>%
        filter(!toupper(Name) %in% c("MALE","FEMALE")) %>%
        mutate(LRN = as.character(LRN))
      
      if (toupper(subject_name) == "MAPEH") {
        expanded <- pmap_dfr(
          list(
            Gender = base$Gender,
            LRN = base$LRN,
            Name = base$Name,
            Q1 = base$Q1,
            Q2 = base$Q2,
            Q3 = base$Q3,
            Q4 = base$Q4,
            Final = base$Final,
            Remark = base$Remark
          ),
          function(Gender, LRN, Name, Q1, Q2, Q3, Q4, Final, Remark) {
            subjects <- c("MAPEH", "MUSIC", "ARTS", "PHYSICAL EDUCATION", "HEALTH")
            df <- tibble(
              Gender = Gender,
              LRN = LRN,
              Name = Name,
              Subject = subjects,
              Q1 = NA_real_, Q2 = NA_real_, Q3 = NA_real_, Q4 = NA_real_,
              Final = NA_real_,
              Remark = Remark
            )
            values <- c(Final, Q1, Q2, Q3, Q4)
            if (!is.na(quarter_col)) {
              df[[quarter_col]] <- values
            } else {
              df$Final[1] <- Final
            }
            df %>% mutate(
              Q1 = round(as.numeric(Q1), 1),
              Q2 = round(as.numeric(Q2), 1),
              Q3 = round(as.numeric(Q3), 1),
              Q4 = round(as.numeric(Q4), 1),
              Final = round(as.numeric(Final), 1)
            )
          }
        )
        return(expanded %>% mutate(Subject = toupper(Subject)))
      } else {
        out <- base %>%
          mutate(
            Subject = toupper(subject_name),
            Q1 = round(as.numeric(Q1), 1),
            Q2 = round(as.numeric(Q2), 1),
            Q3 = round(as.numeric(Q3), 1),
            Q4 = round(as.numeric(Q4), 1),
            Final = round(as.numeric(Final), 1),
            Remark = as.character(Remark)
          )
        return(out)
      }
    }
    
    paths <- input$files$datapath
    results <- map(paths, possibly(read_one_file, otherwise = NULL))
    all_data <- compact(results)
    if (length(all_data) == 0) return(tibble())
    
    merged <- bind_rows(all_data) %>%
      mutate(Gender = as.character(Gender),
             Subject = toupper(Subject)) %>%
      group_by(LRN, Subject) %>%
      summarise(
        Name   = Name,
        Gender = first(na.omit(Gender)),
        Q1 = first(na.omit(Q1)),
        Q2 = first(na.omit(Q2)),
        Q3 = first(na.omit(Q3)),
        Q4 = first(na.omit(Q4)),
        Final = first(na.omit(Final)),
        Remark = first(na.omit(Remark)),
        .groups = "drop"
      )
    
    
    # --- Fix inconsistent Name and Gender per LRN ---
    name_map <- merged %>%
      group_by(LRN) %>%
      summarise(Name = first(na.omit(Name)), .groups = "drop")
    
    gender_map <- merged %>%
      group_by(LRN) %>%
      summarise(Gender = first(na.omit(Gender)), .groups = "drop")
    
    merged <- merged %>%
      select(-Name, -Gender) %>%
      left_join(name_map, by = "LRN") %>%
      left_join(gender_map, by = "LRN")
    
    # --- Summarise per LRN and Subject ---
    merged <- merged %>%
      group_by(LRN, Subject) %>%
      summarise(
        Name   = first(Name),
        Gender = first(Gender),
        Q1 = first(na.omit(Q1)),
        Q2 = first(na.omit(Q2)),
        Q3 = first(na.omit(Q3)),
        Q4 = first(na.omit(Q4)),
        Final = first(na.omit(Final)),
        Remark = first(na.omit(Remark)),
        .groups = "drop"
      )
    
    
    
    
    # --- Compute MAPEH Final & Remark only if all 4 quarters present ---
    merged <- merged %>%
      group_by(LRN) %>%
      mutate(
        Final = ifelse(
          Subject == "MAPEH" & !is.na(Q1) & !is.na(Q2) & !is.na(Q3) & !is.na(Q4),
          round((as.numeric(Q1) + as.numeric(Q2) + as.numeric(Q3) + as.numeric(Q4)) / 4, 0),
          Final
        ),
        Remark = ifelse(
          Subject == "MAPEH" & !is.na(Final),
          ifelse(as.numeric(Final) >= 75, "PASSED", "FAILED"),
          Remark
        )
      ) %>%
      ungroup()
    
    # --- Add averages per student (excluding MAPEH sub-subjects) ---
    merged_with_avg <- merged %>%
      group_by(LRN) %>%
      group_split() %>%
      map_dfr(~{
        df <- .
        numeric_cols <- df %>%
          filter(!Subject %in% c("MUSIC", "ARTS", "PHYSICAL EDUCATION", "HEALTH")) %>%
          select(Q1, Q2, Q3, Q4, Final)
        
        avg_whole <- colMeans(numeric_cols, na.rm = TRUE)
        avg_whole <- ifelse(is.nan(avg_whole), NA, round(avg_whole, 0))
        
        avg_dec <- colMeans(numeric_cols, na.rm = TRUE)
        avg_dec <- ifelse(is.nan(avg_dec), NA, round(avg_dec, 2))
        
        student_name <- first(na.omit(df$Name))
        student_gender <- first(na.omit(df$Gender))
        
        avg_rows <- tibble(
          Gender = student_gender,
          LRN = df$LRN[1],
          Name = student_name,
          Subject = c("Average (whole)", "Average (2 dec)"),
          Q1 = c(avg_whole["Q1"], avg_dec["Q1"]),
          Q2 = c(avg_whole["Q2"], avg_dec["Q2"]),
          Q3 = c(avg_whole["Q3"], avg_dec["Q3"]),
          Q4 = c(avg_whole["Q4"], avg_dec["Q4"]),
          Final = c(avg_whole["Final"], avg_dec["Final"]),
          Remark = c(
            ifelse(!is.na(avg_whole["Final"]), ifelse(avg_whole["Final"] >= 75, "PASSED", "FAILED"), NA),
            ifelse(!is.na(avg_dec["Final"]), ifelse(avg_dec["Final"] >= 75, "PASSED", "FAILED"), NA)
          )
        )
        bind_rows(df, avg_rows)
      }) %>%
      ungroup()
    
    # --- Final formatting & sorting ---
    merged_final <- merged_with_avg %>%
      mutate(Gender = toupper(Gender)) %>%
      mutate(Gender = factor(Gender, levels = c("M", "F"))) %>%
      mutate(Subject = factor(Subject, levels = unique(c(subject_order, unique(Subject))))) %>%
      arrange(Gender, Name, Subject) %>%
      group_by(LRN) %>%
      mutate(
        LRN = ifelse(row_number() == 1, LRN, NA_character_),
        Name = ifelse(row_number() == 1, Name, NA_character_),
        Gender = ifelse(row_number() == 1, as.character(Gender), NA_character_)
      ) %>%
      
      ungroup() %>%
      select(Gender, LRN, Name, Subject, Q1, Q2, Q3, Q4, Final, Remark)
    
    merged_final
  })
  
  
  
  
  output$preview <- renderTable({
    head(merged_data(), 20)
  }, rownames = FALSE)
  
  output$download_excel <- downloadHandler(
    filename = function() paste0("Consolidated_Grades_Long_", Sys.Date(), ".xlsx"),
    content = function(file) {
      data <- merged_data()
      if (nrow(data) == 0) {
        wb <- createWorkbook()
        addWorksheet(wb, "Grades")
        saveWorkbook(wb, file, overwrite = TRUE)
        return()
      }
      
      excel_data <- data[, -(1:2)]
      wb <- createWorkbook()
      addWorksheet(wb, "Grades")
      writeData(wb, sheet = "Grades", x = excel_data, startRow = 1, startCol = 1)
      setColWidths(wb, sheet = "Grades", cols = 1:ncol(excel_data), widths = "auto")
      
      thick_top    <- createStyle(border = "top", borderStyle = "thick")
      thick_bottom <- createStyle(border = "bottom", borderStyle = "thick")
      thick_left   <- createStyle(border = "left", borderStyle = "thick")
      thick_right  <- createStyle(border = "right", borderStyle = "thick")
      gray_fill    <- createStyle(fgFill = "#F2F2F2")
      header_style <- createStyle(
        fontColour = "black", fgFill = "#D9D9D9",
        halign = "center", textDecoration = "bold",
        border = "bottom", borderStyle = "thick"
      )
      left_align   <- createStyle(halign = "left")
      
      # Excel number formatting
      whole_style   <- createStyle(numFmt = "0", halign = "center")
      two_dec_style <- createStyle(numFmt = "0.00", halign = "center")
      
      addStyle(wb, "Grades", header_style, rows = 1, cols = 1:ncol(excel_data), gridExpand = TRUE)
      addStyle(wb, "Grades", left_align, rows = 2:(nrow(excel_data)+1), cols = c(1,2), gridExpand = TRUE)
      
      # Apply whole number format for grades
      addStyle(wb, "Grades", whole_style, rows = 2:(nrow(excel_data)+1), cols = 3:7, gridExpand = TRUE, stack = TRUE)
      # Apply 2-decimals format for "Average (2 dec)" row
      avg2_rows <- which(excel_data$Subject == "Average (2 dec)") + 1
      if(length(avg2_rows) > 0){
        addStyle(wb, "Grades", two_dec_style, rows = avg2_rows, cols = 3:7, gridExpand = TRUE, stack = TRUE)
      }
      
      student_starts <- which(!is.na(excel_data$Name)) + 1
      last_row <- nrow(excel_data) + 1
      
      if (length(student_starts) == 0) {
        addStyle(wb, "Grades", thick_bottom, rows = last_row, cols = 1:ncol(excel_data), gridExpand = TRUE, stack = TRUE)
      } else {
        for (i in seq_along(student_starts)) {
          start_row <- student_starts[i]
          end_row <- if (i < length(student_starts)) student_starts[i + 1] - 1 else last_row
          if (i %% 2 == 0) {
            addStyle(wb, "Grades", gray_fill, rows = start_row:end_row, cols = 1:ncol(excel_data),
                     gridExpand = TRUE, stack = TRUE)
          }
          addStyle(wb, "Grades", thick_top, rows = start_row, cols = 1:ncol(excel_data), gridExpand = TRUE, stack = TRUE)
          addStyle(wb, "Grades", thick_left, rows = start_row:end_row, cols = 1, gridExpand = TRUE, stack = TRUE)
          addStyle(wb, "Grades", thick_right, rows = start_row:end_row, cols = ncol(excel_data), gridExpand = TRUE, stack = TRUE)
        }
        addStyle(wb, "Grades", thick_bottom, rows = last_row, cols = 1:ncol(excel_data), gridExpand = TRUE, stack = TRUE)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

#auth0::shinyAppAuth0(ui = ui, server = server)
shinyApp(ui = ui, server = server)
