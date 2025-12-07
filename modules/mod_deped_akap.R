
# mod_deped_akap.R
# AKAP Multi-File Validator & Regenerator (BASELINE DATA only)
# Author: EMLStat with GEN AI (auto-process; single auto-download ZIP; frequency inside ZIP)
# Last updated: 2025-12-04

# IMPORTANT:
# - Host app must load: shiny, readxl, openxlsx, dplyr, stringi, DT, tibble
# - All IDs are namespaced via NS(id)
# - File paths use unique suffix per session/run to avoid conflicts

# =========================
# CONFIG / CONSTANTS
# =========================
mod_deped_akap_constants <- local({
  list(
    TEMPLATE_PATH = "akap_template.xlsx", # set to NULL if not using a template
    TEMPLATE_SHEET = "BASELINE DATA",
    CANONICAL_BN_NAMES = c(
      "Level",               # B
      "School",              # C
      "Grade",               # D
      "Section",             # E
      "Teacher",             # F
      "Learner",             # G
      "Gender",              # H
      "AKAP Classification", # I
      "4Ps Beneficiary?",    # J
      "Feeding Program Beneficiary?", # K
      "Academic Support Needed?",     # L
      "Non-Academic Support Needed?", # M
      "Status of Student as of Nov 2025" # N
    ),
    RECLASS_SET = c(
      "Absenteeism",
      "Lagging Behind",
      "Non-Submission Of Output",
      "Misbehavior",
      "Working Students",
      "Health Problems",
      "Early Pregnancy"
    )
  )
})

# =========================
# HELPERS (pure functions)
# =========================
normalize_trim <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- ""
  s <- stringi::stri_replace_all_regex(
    s,
    pattern = "[\\p{Zs}\\u00A0\\u200B\\u200C\\u200D\\u2060]",
    replacement = " ",
    vectorize_all = FALSE
  )
  stringi::stri_trim_both(s)
}
is_effectively_empty <- function(x) {
  s <- normalize_trim(x)
  s <- ifelse(s %in% c("-", "—", ".", "N/A", "NA"), "", s)
  stringi::stri_isempty(s)
}
sanitize_teacher_name <- function(name) {
  s <- normalize_trim(name)
  stringi::stri_trans_totitle(stringi::stri_trans_tolower(s))
}
sanitize_learner_names <- function(name) {
  if (is.na(name)) return("")
  s <- normalize_trim(name)
  if (isTRUE(stringi::stri_isempty(s))) return("")
  parts <- unlist(strsplit(s, "[,;/]+"))
  parts <- normalize_trim(parts)
  parts <- parts[!stringi::stri_isempty(parts)]
  if (length(parts) == 0) return("")
  parts <- stringi::stri_trans_totitle(stringi::stri_trans_tolower(parts))
  paste(parts, collapse = ", ")
}
sanitize_school_name <- function(name) {
  s <- normalize_trim(name)
  stringi::stri_trans_toupper(s)
}
sanitize_classification <- function(x) {
  s <- normalize_trim(x)
  stringi::stri_trans_totitle(stringi::stri_trans_tolower(s))
}
compute_reclassification <- function(classification_tc) {
  ifelse(
    classification_tc %in% mod_deped_akap_constants$RECLASS_SET,
    classification_tc, "Other Reason"
  )
}
safe_fragment <- function(x) {
  s <- as.character(x)
  if (is.na(s) || isTRUE(stringi::stri_isempty(stringi::stri_trim_both(s)))) s <- "UNKNOWN"
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- gsub("\\s+", "_", s, perl = TRUE)
  s <- gsub("[/\\\\?%*:\\\"<>]", "-", s, perl = TRUE)
  substr(s, 1, 120)
}
get_col_ix <- function(df, target_name) {
  ix <- which(tolower(names(df)) == tolower(target_name))
  if (length(ix) == 0) NA_integer_ else ix[1]
}
bn_matrix <- function(df_BN) {
  df_BN <- as.data.frame(df_BN, stringsAsFactors = FALSE)
  mat <- vapply(df_BN, function(col) {
    s <- normalize_trim(col)
    s <- ifelse(s %in% c("-", "—", ".", "N/A", "NA"), "", s)
    s
  }, FUN.VALUE = character(nrow(df_BN)), USE.NAMES = FALSE)
  if (!is.matrix(mat)) mat <- matrix(mat, ncol = 1)
  mat
}
compute_valid_flags <- function(df_all) {
  df_bn_only <- df_all[, mod_deped_akap_constants$CANONICAL_BN_NAMES, drop = FALSE]
  mat <- bn_matrix(df_bn_only)
  rowSums(mat != "") == ncol(mat)
}

# Enforce canonical column names (by order) on a single file's B..N block
enforce_canonical_bn_names <- function(df_BN) {
  expected <- length(mod_deped_akap_constants$CANONICAL_BN_NAMES)
  if (ncol(df_BN) != expected) {
    stop(paste0("Expected ", expected, " columns but found ", ncol(df_BN), "."))
  }
  names(df_BN) <- mod_deped_akap_constants$CANONICAL_BN_NAMES
  df_BN
}

# >>> Grade standardization & frequency helpers
grade_categories <- c("Kinder", paste0("Grade ", 1:12), "ALS", "SNED", "UNKNOWN")
standardize_grade <- function(x) {
  s <- normalize_trim(x)
  s_low <- stringi::stri_trans_tolower(s)
  s_low <- gsub("[^a-z0-9]+", " ", s_low, perl = TRUE)
  s_low <- stringi::stri_trim_both(s_low)
  if (s_low %in% c("k", "kinder", "kindergarten")) return("Kinder")
  if (grepl("\\bals\\b", s_low)) return("ALS")
  if (grepl("\\bsned\\b", s_low)) return("SNED")
  m <- regmatches(s_low, regexpr("\\b(grade\\s*)?([1-9]|1[0-2])\\b", s_low))
  if (length(m) > 0) {
    n <- as.integer(gsub("[^0-9]", "", m))
    if (!is.na(n) && n >= 1 && n <= 12) return(paste0("Grade ", n))
  }
  "UNKNOWN"
}
compute_wide_freq <- function(df_rows, school_col_name, category_vec, category_levels) {
  school <- as.character(df_rows[[school_col_name]])
  school[is_effectively_empty(school)] <- "UNKNOWN"
  cat_fac <- factor(category_vec, levels = category_levels)
  tab <- xtabs(~ school + cat_fac)
  mat <- as.data.frame.matrix(tab)
  if (nrow(mat) > 0) {
    mat <- mat[order(rownames(mat)), , drop = FALSE]
  } else {
    mat <- as.data.frame(matrix(0L, nrow = 0, ncol = length(category_levels)))
    colnames(mat) <- category_levels
  }
  mat[is.na(mat)] <- 0L
  total_row <- colSums(mat)
  mat <- rbind(mat, TOTAL = total_row)
  data.frame(School = rownames(mat), mat, row.names = NULL, check.names = FALSE)
}
# <<< end helpers

# =========================
# TEMPLATE-AWARE WRITERS
# =========================
write_data_via_template <- function(template_path, sheet_name, df_data, out_path) {
  if (is.null(template_path) || !file.exists(template_path)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet = sheet_name, x = df_data, startCol = 2, startRow = 1, colNames = TRUE)
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
    return(out_path)
  }
  wb <- openxlsx::loadWorkbook(template_path)
  if (!(sheet_name %in% names(wb))) {
    stop(paste0("Template '", template_path, "' does not contain sheet '", sheet_name, "'."))
  }
  openxlsx::writeData(wb, sheet = sheet_name, x = df_data, startCol = 2, startRow = 2, colNames = FALSE)
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  out_path
}
write_merged_valid_workbook_template <- function(template_path, sheet_name, df_BN_valid, out_path) {
  write_data_via_template(template_path, sheet_name, df_BN_valid, out_path)
  out_path
}

# =========================
# ZIP HELPER
# =========================
make_zip <- function(files, zip_path) {
  if (length(files) == 0) return(NULL)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile = zip_path, files = files, include_directories = FALSE)
    return(zip_path)
  } else {
    owd <- getwd(); on.exit(setwd(owd), add = TRUE)
    tmp <- tempfile("zipwd"); dir.create(tmp)
    file.copy(files, tmp); setwd(tmp)
    utils::zip(zipfile = basename(zip_path), files = basename(files))
    file.copy(basename(zip_path), zip_path, overwrite = TRUE)
    return(zip_path)
  }
}

# =========================
# MODULE UI
# =========================
mod_deped_akap_ui <- function(id, title = "AKAP Data Manager") {
  ns <- NS(id)
  fluidPage(
    titlePanel(title),
    # JS helper to click a single hidden download link once
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('akapAutoDownloadOnce', function(id){
         if (!id) return;
         var el = document.getElementById(id);
         if (el) { el.click(); }
       });"
    )),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("files"), "Upload Excel files (.xlsx)", accept = c(".xlsx"), multiple = TRUE),
        checkboxInput(ns("drop_empty_teacher"), "Drop rows with empty Teacher", value = F),
        helpText(HTML(
          paste(
            "Reads only <b>BASELINE DATA</b> in each file.",
            "<br/>Drops Column A; keeps remaining headers.",
            "<br/>Removes fully empty rows.",
            "<br/>New column <b>Reclassification</b> created.",
            "<br/>Outputs grouped by <b>LEVEL and SCHOOL</b>."
          )
        )),
        tags$hr(),
        uiOutput(ns("status")),
        tags$hr(),
        # Hidden download link (ZIP only)
        tags$div(style = "display:none;",
                 downloadLink(ns("dl_zip"), "Download AKAP Valid Cases (ZIP)")
        )
      ),
      mainPanel(
        uiOutput(ns("summary_ui")),
        uiOutput(ns("sample_unvalid_ui")),
        uiOutput(ns("freq_by_school_ui"))
      )
    )
  )
}

# =========================
# MODULE SERVER
# =========================
mod_deped_akap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    const <- mod_deped_akap_constants
    
    make_unique_suffix <- function() {
      paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(100000:999999, 1))
    }
    
    rv <- reactiveValues(
      df_all_BN = NULL,
      valid_flags = NULL,
      summary_df = NULL,
      removed_empty_bn = 0,
      removed_no_teacher = 0,
      zip_path = NULL,
      out_dir = NULL,
      freq_grade_by_school = NULL,
      freq_reclass_by_school = NULL,
      freq_combined_long = NULL,
      last_auto_stamp = NULL # guard: ensures one auto-download per processing run
    )
    
    output$status <- renderUI({
      if (is.null(rv$df_all_BN)) {
        tags$div(class = "text-muted", "No files processed yet. Reports will be be visible after successful upload.")
      } else {
        valid_n <- sum(rv$valid_flags)
        total_n <- nrow(rv$df_all_BN)
        tags$div(
          tags$b("Files processed: "), length(input$files$datapath), br(),
          tags$b("Rows removed (missing Teacher): "), rv$removed_no_teacher, br(),
          tags$b("Rows kept: "), total_n, br(),
          tags$b("VALID rows: "), valid_n, " (", sprintf("%.1f%%", 100 * valid_n / total_n), ")"
        )
      }
    })
    
    # === Auto-process immediately on upload (and when checkbox toggles) ===
    observeEvent(list(input$files, input$drop_empty_teacher), {
      req(input$files)
      tryCatch({
        # Reset guard at start of a new run
        rv$last_auto_stamp <- NULL
        
        paths <- input$files$datapath
        cleaned_list <- list()
        removed_empty_total <- 0
        removed_no_teacher_total <- 0
        
        for (p in paths) {
          sheets <- readxl::excel_sheets(p)
          if (!("BASELINE DATA" %in% sheets)) {
            showNotification(paste0("Skipped file (no 'BASELINE DATA'): ", basename(p)),
                             type = "error", duration = 5)
            next
          }
          df_full <- tryCatch({
            readxl::read_excel(p, sheet = "BASELINE DATA", col_names = TRUE)
          }, error = function(e) {
            showNotification(paste0("Failed to read: ", basename(p), " -> ", e$message),
                             type = "error", duration = 6)
            NULL
          })
          if (is.null(df_full)) next
          df_full <- as.data.frame(df_full, stringsAsFactors = FALSE)
          
          if (ncol(df_full) < 14) {
            showNotification(paste0("Skipped file (less than A..N columns): ", basename(p)),
                             type = "error", duration = 6)
            next
          }
          
          df_BN <- df_full[, 2:14, drop = FALSE]
          df_BN <- enforce_canonical_bn_names(df_BN)
          
          mat_BN <- bn_matrix(df_BN)
          keep_rows_bn <- rowSums(mat_BN != "") > 0
          removed_empty_total <- removed_empty_total + sum(!keep_rows_bn)
          df_BN <- df_BN[keep_rows_bn, , drop = FALSE]
          
          col_teacher <- get_col_ix(df_BN, "Teacher")
          if (is.na(col_teacher)) {
            showNotification(paste0("Skipped file (no 'Teacher' column): ", basename(p)),
                             type = "error", duration = 6)
            next
          }
          if (isTRUE(input$drop_empty_teacher)) {
            teacher_is_empty <- is_effectively_empty(df_BN[[col_teacher]])
            removed_no_teacher_total <- removed_no_teacher_total + sum(teacher_is_empty)
            df_BN <- df_BN[!teacher_is_empty, , drop = FALSE]
          }
          
          df_BN[[col_teacher]] <- vapply(df_BN[[col_teacher]], sanitize_teacher_name, character(1))
          col_learner <- get_col_ix(df_BN, "Learner")
          if (!is.na(col_learner)) {
            df_BN[[col_learner]] <- vapply(df_BN[[col_learner]], sanitize_learner_names, character(1))
          }
          
          col_school <- get_col_ix(df_BN, "School")
          if (is.na(col_school)) {
            showNotification(paste0("Skipped file (no 'School' column): ", basename(p)),
                             type = "error", duration = 6)
            next
          }
          df_BN[[col_school]] <- vapply(df_BN[[col_school]], sanitize_school_name, character(1))
          
          col_class <- get_col_ix(df_BN, "AKAP Classification")
          if (is.na(col_class)) {
            showNotification(paste0("Skipped file (no 'AKAP Classification' column): ", basename(p)),
                             type = "error", duration = 6)
            next
          }
          df_BN[[col_class]] <- vapply(df_BN[[col_class]], sanitize_classification, character(1))
          df_BN$Reclassification <- compute_reclassification(df_BN[[col_class]])
          
          df_BN$SourceFile <- basename(p)
          cleaned_list[[length(cleaned_list) + 1]] <- df_BN
        }
        
        if (length(cleaned_list) == 0) {
          showNotification("No valid files were processed. Please check your uploads.",
                           type = "error", duration = 7)
          return(invisible(NULL))
        }
        
        df_all <- dplyr::bind_rows(cleaned_list)
        valid_flags <- compute_valid_flags(df_all)
        
        col_level <- get_col_ix(df_all, "Level")
        col_school <- get_col_ix(df_all, "School")
        level_vec <- as.character(df_all[[col_level]])
        school_vec <- as.character(df_all[[col_school]])
        
        summary_df <- tibble::tibble(
          Level = ifelse(is_effectively_empty(level_vec), "UNKNOWN", level_vec),
          School = ifelse(is_effectively_empty(school_vec), "UNKNOWN", school_vec),
          VALIDITY = ifelse(valid_flags, "VALID", "UNVALID")
        ) |>
          dplyr::group_by(Level, School) |>
          dplyr::summarize(
            Total = dplyr::n(),
            `Valid Cases` = sum(VALIDITY == "VALID"),
            `Invalid Cases` = sum(VALIDITY == "UNVALID"),
            `Valid %` = round(100 * `Valid Cases` / Total, 1),
            .groups = "drop"
          ) |>
          dplyr::arrange(Level, School)
        
        rv$df_all_BN <- df_all
        rv$valid_flags <- valid_flags
        rv$summary_df <- summary_df
        rv$removed_empty_bn <- removed_empty_total
        rv$removed_no_teacher <- removed_no_teacher_total
        
        # Outputs grouped by LEVEL + SCHOOL (VALID rows only; includes Reclassification)
        unique_suffix <- make_unique_suffix()
        out_dir <- tempfile(paste0("akap_level_school_xlsx_", unique_suffix))
        dir.create(out_dir, showWarnings = FALSE)
        
        df_valid_all <- df_all[valid_flags, , drop = FALSE]
        if (nrow(df_valid_all) == 0) {
          showNotification("No VALID rows found across uploads; ZIP will include only an empty VALID_ALL.",
                           type = "warning", duration = 6)
        }
        
        # >>> Frequency computations (VALID rows only)
        col_grade <- get_col_ix(df_valid_all, "Grade")
        col_school_valid <- get_col_ix(df_valid_all, "School")
        grade_std <- if (!is.na(col_grade)) {
          vapply(df_valid_all[[col_grade]], standardize_grade, character(1))
        } else {
          rep("UNKNOWN", nrow(df_valid_all))
        }
        freq_grade_by_school <- compute_wide_freq(
          df_rows = df_valid_all,
          school_col_name = names(df_valid_all)[col_school_valid],
          category_vec = grade_std,
          category_levels = grade_categories
        )
        reclass_levels <- c(mod_deped_akap_constants$RECLASS_SET, "Other Reason")
        reclass_vec <- as.character(df_valid_all$Reclassification)
        reclass_vec[is.na(reclass_vec) | is_effectively_empty(reclass_vec)] <- "Other Reason"
        freq_reclass_by_school <- compute_wide_freq(
          df_rows = df_valid_all,
          school_col_name = names(df_valid_all)[col_school_valid],
          category_vec = reclass_vec,
          category_levels = reclass_levels
        )
        
        # Store frequency tables for the DT
        rv$freq_grade_by_school <- freq_grade_by_school
        rv$freq_reclass_by_school <- freq_reclass_by_school
        
        # Combined long table for display
        grade_long <- reshape(
          freq_grade_by_school,
          idvar = "School",
          varying = grade_categories,
          v.names = "Count",
          timevar = "Category",
          times = grade_categories,
          direction = "long"
        ); grade_long$Report <- "Grade Level"
        reclass_long <- reshape(
          freq_reclass_by_school,
          idvar = "School",
          varying = reclass_levels,
          v.names = "Count",
          timevar = "Category",
          times = reclass_levels,
          direction = "long"
        ); reclass_long$Report <- "Reclassification"
        freq_combined_long <- rbind(
          grade_long[, c("Report", "School", "Category", "Count")],
          reclass_long[, c("Report", "School", "Category", "Count")]
        )
        rv$freq_combined_long <- freq_combined_long[
          order(freq_combined_long$Report, freq_combined_long$School, freq_combined_long$Category), ]
        
        # Group files by Level + School
        df_valid_all <- df_valid_all |>
          dplyr::mutate(.Level = as.character(df_valid_all[[col_level]]),
                        .School = as.character(df_valid_all[[col_school]])) |>
          dplyr::group_by(.Level, .School)
        groups <- dplyr::group_split(df_valid_all)
        keys <- dplyr::group_keys(df_valid_all)
        
        files_written <- character(0)
        used_names <- character(0)
        
        for (i in seq_along(groups)) {
          g <- groups[[i]] |>
            dplyr::ungroup() |>
            dplyr::select(-.Level, -.School, -SourceFile)  # keep Reclassification
          
          lvl <- safe_fragment(keys$.Level[i])
          schl <- safe_fragment(keys$.School[i])
          fname <- file.path(out_dir, paste0(lvl, "_", schl, ".xlsx"))
          k <- 2
          while (basename(fname) %in% basename(used_names)) {
            fname <- file.path(out_dir, paste0(lvl, "_", schl, " (", k, ").xlsx"))
            k <- k + 1
          }
          used_names <- c(used_names, fname)
          
          write_data_via_template(const$TEMPLATE_PATH, const$TEMPLATE_SHEET, g, fname)
          
          # Append group-level frequency sheets
          grp_school_col <- get_col_ix(g, "School")
          grp_grade_col <- get_col_ix(g, "Grade")
          grp_school_vec <- as.character(g[[grp_school_col]])
          grp_grade_std <- if (!is.na(grp_grade_col)) {
            vapply(g[[grp_grade_col]], standardize_grade, character(1))
          } else { rep("UNKNOWN", nrow(g)) }
          grp_freq_grade <- compute_wide_freq(
            df_rows = data.frame(School = grp_school_vec),
            school_col_name = "School",
            category_vec = grp_grade_std,
            category_levels = grade_categories
          )
          grp_reclass_levels <- c(mod_deped_akap_constants$RECLASS_SET, "Other Reason")
          grp_reclass_vec <- as.character(g$Reclassification)
          grp_reclass_vec[is.na(grp_reclass_vec) | is_effectively_empty(grp_reclass_vec)] <- "Other Reason"
          grp_freq_reclass <- compute_wide_freq(
            df_rows = data.frame(School = grp_school_vec),
            school_col_name = "School",
            category_vec = grp_reclass_vec,
            category_levels = grp_reclass_levels
          )
          
          wb_grp <- openxlsx::loadWorkbook(fname)
          openxlsx::addWorksheet(wb_grp, "Frequency_GRADE")
          openxlsx::writeData(wb_grp, "Frequency_GRADE", grp_freq_grade)
          openxlsx::addWorksheet(wb_grp, "Frequency_RECLASS")
          openxlsx::writeData(wb_grp, "Frequency_RECLASS", grp_freq_reclass)
          openxlsx::saveWorkbook(wb_grp, fname, overwrite = TRUE)
          
          files_written <- c(files_written, fname)
        }
        
        # Merged VALID_ALL.xlsx with frequency sheets
        merged_path <- file.path(out_dir, "VALID_ALL.xlsx")
        merged_df <- df_valid_all |>
          dplyr::ungroup() |>
          dplyr::select(-.Level, -.School, -SourceFile)
        write_merged_valid_workbook_template(const$TEMPLATE_PATH, const$TEMPLATE_SHEET, merged_df, merged_path)
        
        wb_merged <- openxlsx::loadWorkbook(merged_path)
        openxlsx::addWorksheet(wb_merged, "Frequency_GRADE")
        openxlsx::writeData(wb_merged, "Frequency_GRADE", rv$freq_grade_by_school)
        openxlsx::addWorksheet(wb_merged, "Frequency_RECLASS")
        openxlsx::writeData(wb_merged, "Frequency_RECLASS", rv$freq_reclass_by_school)
        openxlsx::saveWorkbook(wb_merged, merged_path, overwrite = TRUE)
        files_written <- c(files_written, merged_path)
        
        # >>> Build Frequency workbook (standalone) and include it in ZIP
        freq_path <- file.path(out_dir, "AKAP_Frequency_by_SCHOOL.xlsx")
        wb_freq <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb_freq, "GradeLevel_by_SCHOOL")
        openxlsx::writeData(wb_freq, "GradeLevel_by_SCHOOL", rv$freq_grade_by_school)
        openxlsx::addWorksheet(wb_freq, "Reclass_by_SCHOOL")
        openxlsx::writeData(wb_freq, "Reclass_by_SCHOOL", rv$freq_reclass_by_school)
        openxlsx::saveWorkbook(wb_freq, freq_path, overwrite = TRUE)
        if (file.exists(freq_path)) {
          files_written <- c(files_written, freq_path)
        } else {
          showNotification("Frequency workbook could not be saved; it will not be in the ZIP.",
                           type = "warning", duration = 6)
        }
        
        # Create ZIP
        rv$out_dir <- out_dir
        rv$zip_path <- file.path(tempdir(), paste0("akap_", unique_suffix, "_LEVEL_SCHOOL_VALID.zip"))
        zip_created <- make_zip(files_written, rv$zip_path)
        
        if (is.null(zip_created) || !file.exists(rv$zip_path)) {
          showNotification("ZIP could not be created.", type = "error", duration = 7)
          return(invisible(NULL))
        } else {
          showNotification("ZIP created successfully.", type = "message", duration = 4)
        }
        
        showNotification(
          paste0("Done: 'AKAP Classification' set to Title Case; 'Reclassification' added. ",
                 "Created per-LEVEL_SCHOOL outputs + VALID_ALL.xlsx and Frequency workbook (in ZIP)."),
          type = "message", duration = 6
        )
        
        # === Single auto-download (ZIP only), guarded so it fires ONCE per run ===
        if (is.null(rv$last_auto_stamp) || rv$last_auto_stamp != unique_suffix) {
          rv$last_auto_stamp <- unique_suffix
          session$onFlushed(function() {
            # Single click, once per registration
            session$sendCustomMessage("akapAutoDownloadOnce", session$ns("dl_zip"))
          }, once = TRUE)
        }
        
      }, error = function(e) {
        showNotification(paste0("Unexpected error: ", e$message), type = "error", duration = 8)
        return(invisible(NULL))
      })
    }, ignoreInit = TRUE)
    
    # Summary table
    output$summary_ui <- renderUI({
      req(rv$summary_df)   # only show after file upload
      tagList(
        h4(HTML("<b>Summary of Cases</b>")),
        DT::renderDT({
          DT::datatable(
            rv$summary_df,
            rownames = FALSE,
            options = list(
              lengthMenu = list(c(5, 10, 25, 50, 100), c('5', '10', '25', '50', '100')),
              pageLength = 5,
              dom = "rtip"
            )
          )
        }, server = FALSE),
        tags$hr()
      )
    })
    
    # Sample of UNVALID rows
    output$sample_unvalid_ui <- renderUI({
      req(rv$summary_df)   # only show after file upload
      tagList(
        h4(HTML("<b>List of Invalid Cases</b>")),
        #h5(HTML("<b>Note:</b> Cases with missing teacher not included")),
        DT::renderDT({
          req(rv$df_all_BN, rv$valid_flags)
          unv <- rv$df_all_BN[!rv$valid_flags, , drop = FALSE]
          # Remove last column
          unv <- unv[, -ncol(unv), drop = FALSE]
          DT::datatable(
            head(unv, 25),
            rownames = FALSE,
            options = list(
              lengthMenu = list(c(5, 10, 25, 50, 100), c('5', '10', '25', '50', '100')),
              pageLength = 5,
              scrollX = TRUE,
              dom = "rtip"
              
            )
          )
        }, server = FALSE),
        tags$hr()
      )
      
      
    })
    
    # Frequency report table
     output$freq_by_school_ui <- renderUI({
      req(rv$summary_df)   # only show after file upload
      tagList(
        h4(HTML("<b>Counts by Grade Level and AKAP (Re)Classification</b>")),
        h5(HTML("<b>Note:</b> This report is included in the downloaded zip file.")),
        DT::renderDT({
          req(rv$freq_combined_long)
          DT::datatable(
            rv$freq_combined_long,
            rownames = FALSE,
            options = list(
              lengthMenu = list(c(5, 10, 25, 50, 100), c('5', '10', '25', '50', '100')),
              pageLength = 5,
              scrollX = TRUE,
              dom = "rtip"
            )
          )
        }, server = FALSE)
        
      )
      
      
      
      
    })
    
    
    # === Single download handler (ZIP), with guards ===
    output$dl_zip <- downloadHandler(
      filename = function() "AKAP_LEVEL_SCHOOL_VALID.zip",
      content = function(file) {
        req(rv$zip_path)
        validate(need(file.exists(rv$zip_path), "ZIP not found yet. Please try again."))
        ok <- file.copy(rv$zip_path, file, overwrite = TRUE)
        validate(need(ok && file.exists(file), "Failed to prepare ZIP for download."))
      }
    )
    # keep ZIP handler alive even if hidden
    outputOptions(output, "dl_zip", suspendWhenHidden = FALSE)
  })
}

# =========================
# NOTE:
# - Only the ZIP auto-download is triggered once per processing run.
# - The Frequency workbook is written to out_dir and included inside the ZIP.
# - If your browser blocks auto-downloads, it may prompt once to allow them. After allowing, it will work smoothly.
# =========================
