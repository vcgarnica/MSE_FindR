# SHINY DASHBOARD - MSE FINDR
# Packages library =================================================

library("shiny")
library("shinydashboard")
library("tidyverse")
library("data.table")
library("DT")
library("fresh")
library("readr")
library("jsonlite")
library("httr")
library("rvest")
library("stringr")

# ============================================================================
# GOOGLE SHEETS VISITOR COUNTER - SETUP INSTRUCTIONS
# ============================================================================
# 1. Create a new Google Sheet
# 2. In cell A1 type: count   |   In cell A2 type: 0
# 3. Go to File > Share > Publish to web (this makes it readable)
# 4. Also click Share > "Anyone with the link" > set to "Editor"
#    (this allows the app to write to it)
# 5. Copy the Sheet ID from the URL:
#    https://docs.google.com/spreadsheets/d/SHEET_ID_HERE/edit
# 6. Paste it below:
# ============================================================================

GSHEET_ID <- "YOUR_GOOGLE_SHEET_ID_HERE"  # <-- REPLACE THIS

# Helper functions for Google Sheets counter (no auth needed for public sheets)
get_visitor_count <- function(sheet_id) {
  tryCatch({
    url <- paste0(
      "https://docs.google.com/spreadsheets/d/", sheet_id,
      "/gviz/tq?tqx=out:csv"
    )
    data <- read.csv(url(url), stringsAsFactors = FALSE)
    count <- as.numeric(data[1, 1])
    if (is.na(count)) 0 else count
  }, error = function(e) {
    message("Could not read visitor count from Google Sheets: ", e$message)
    # Fall back to local file
    count_file <- "visitor_count.txt"
    if (file.exists(count_file)) as.numeric(readLines(count_file, warn = FALSE)) else 0
  })
}

update_visitor_count <- function(sheet_id, new_count) {
  tryCatch({
    count_file <- "visitor_count.txt"
    writeLines(as.character(new_count), count_file)
  }, error = function(e) {
    message("Could not update visitor count: ", e$message)
  })
}

# ============================================================================

APPS_SCRIPT_URL <- "https://script.google.com/macros/s/AKfycbyrgIGJ0rRArn4LtdzeAeOlosQMtvMRzcX2O5RtwDfJO_loRL0z7v4sRsinhr47mU6J/exec"

# Persistent counter functions using Apps Script
get_count_from_api <- function() {
  if (APPS_SCRIPT_URL == "") {
    # Fallback to local file if Apps Script not configured
    count_file <- "visitor_count.txt"
    if (file.exists(count_file)) {
      return(as.numeric(readLines(count_file, warn = FALSE)))
    }
    return(0)
  }
  
  tryCatch({
    res <- httr::GET(paste0(APPS_SCRIPT_URL, "?action=get"), 
                     httr::timeout(5))
    if (httr::status_code(res) == 200) {
      data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      return(as.numeric(data$count))
    }
    return(0)
  }, error = function(e) {
    message("Counter API read error: ", e$message)
    count_file <- "visitor_count.txt"
    if (file.exists(count_file)) as.numeric(readLines(count_file, warn = FALSE)) else 0
  })
}

increment_count_api <- function() {
  if (APPS_SCRIPT_URL == "") {
    # Fallback to local file
    count_file <- "visitor_count.txt"
    current <- if (file.exists(count_file)) as.numeric(readLines(count_file, warn = FALSE)) else 0
    current <- current + 1
    writeLines(as.character(current), count_file)
    return(current)
  }
  
  tryCatch({
    res <- httr::GET(paste0(APPS_SCRIPT_URL, "?action=increment"), 
                     httr::timeout(5))
    if (httr::status_code(res) == 200) {
      data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      return(as.numeric(data$count))
    }
    return(NULL)
  }, error = function(e) {
    message("Counter API increment error: ", e$message)
    # Local fallback
    count_file <- "visitor_count.txt"
    current <- if (file.exists(count_file)) as.numeric(readLines(count_file, warn = FALSE)) else 0
    current <- current + 1
    writeLines(as.character(current), count_file)
    return(current)
  })
}


# Functions =================================================

get_MSD <- function(value, letter) {
  require(data.table)
  
  value <- as.numeric(value)
  letter <- as.character(letter)
  
  # Input validation
  if (length(value) != length(letter)) {
    warning("Value and letter vectors must have same length")
    return(data.table(n = NA, V2 = NA, V3 = NA, range = NA, inter = NA))
  }
  
  if (length(value) < 2) {
    warning("Need at least 2 treatments for MSD calculation")
    return(data.table(n = NA, V2 = NA, V3 = NA, range = NA, inter = NA))
  }
  
  letter <- letter[order(value, decreasing = TRUE)]
  value <- value[order(value, decreasing = TRUE)]
  n <- abs(apply(combn(value, 2), 2, diff))
  l <- combn(letter, 2)
  dt <- as.data.table(t(rbind(n, l)))
  dt$n <- as.numeric(dt$n)
  dt$range <- sequence((length(value) - 1):1) + 1
  dt <- dt[order(dt$n), ]
  dt$inter <- !sapply(gsub(" ", "", paste(dt$V2, dt$V3)),
                      function(x) any(str_count(x, letters) > 1))
  dt[dt == ""] <- NA
  dt <- na.omit(dt)
  
  if (nrow(dt) == 0) {
    warning("No valid letter comparisons found")
    return(data.table(n = NA, V2 = NA, V3 = NA, range = NA, inter = NA))
  }
  
  if (all(dt$V2 == dt$V3) || length(rle(dt$inter)$lengths) == 1) {
    data <- as.data.table(rbind(rep(NA, ncol(dt)), rep(NA, ncol(dt))))
    colnames(data) <- colnames(dt)
    return(data)
  } else {
    msd_1 <- dt[inter == FALSE, .SD[which.max(n)], by = inter]
    msd_2 <- dt[inter == TRUE, .SD[which.min(n)], by = inter]
    msd <- rbind(msd_1, msd_2)
  }
  
  return(msd)
}

get_MSD_SP <- function(value, letter, group) {
  require(data.table)
  
  value <- as.numeric(value)
  letter <- as.character(letter)
  
  n <- abs(apply(combn(value, 2), 2, diff))
  l <- combn(letter, 2)
  k <- combn(group, 2)
  dt <- as.data.table(t(rbind(n, l, k)))
  dt$n <- as.numeric(dt$n)
  dt <- dt[order(dt$n), ]
  dt$inter <- !sapply(gsub(" ", "", paste(dt$V2, dt$V3)),
                      function(x) any(str_count(x, letters) > 1))
  dt[dt == ""] <- NA
  dt <- na.omit(dt)
  dt <- dt[dt$V4 == dt$V5, ]
  
  if (nrow(dt) == 0) {
    warning("No valid comparisons found within groups")
    data <- as.data.table(rbind(rep(NA, 5), rep(NA, 5)))
    colnames(data) <- c("n", "V2", "V3", "V4", "V5")
    return(data)
  }
  
  if (all(dt$V2 == dt$V3) || length(rle(dt$inter)$lengths) == 1) {
    data <- as.data.table(rbind(rep(NA, ncol(dt)), rep(NA, ncol(dt))))
    colnames(data) <- colnames(dt)
    return(data)
  } else {
    msd_1 <- dt[inter == FALSE, .SD[which.max(n)], by = inter]
    msd_2 <- dt[inter == TRUE, .SD[which.min(n)], by = inter]
    msd <- rbind(msd_1, msd_2)
  }
  
  return(msd)
}

# Validation function
validate_inputs <- function(data, means_col, letters_col) {
  errors <- c()
  
  if (any(is.na(data[[means_col]]))) {
    errors <- c(errors, "Means column contains missing values")
  }
  
  if (any(data[[letters_col]] == "" | is.na(data[[letters_col]]))) {
    errors <- c(errors, "Letters column contains empty or missing values")
  }
  
  if (!is.numeric(data[[means_col]])) {
    errors <- c(errors, "Means column must be numeric")
  }
  
  if (length(errors) > 0) {
    return(list(valid = FALSE, message = paste(errors, collapse = "\n")))
  }
  
  return(list(valid = TRUE, message = ""))
}

# MSE calculation functions ---------------------------------------------------------------

Fisher_MSE <- function(msd, rep, alpha, df) {
  mse <- round(0.5 * unique(rep) * (msd / qt(1 - unique(alpha) / 2, unique(df)))^2, 3)
  return(mse)
}

Tukey_MSE <- function(msd, rep, alpha, df, n_factor) {
  mse <- round(unique(rep) * (msd / qtukey(1 - unique(alpha), unique(n_factor), unique(df)))^2, 3)
  return(mse)
}

Bonferroni_MSE <- function(msd, rep, alpha, df, n_factor) {
  k <- choose(unique(n_factor), 2)
  mse <- round(0.5 * unique(rep) * (msd / qt(1 - ((unique(alpha) / 2) / k), unique(df)))^2, 3)
  return(mse)
}

Sidak_MSE <- function(msd, rep, alpha, df, n_factor) {
  k <- choose(unique(n_factor), 2)
  mse <- round(0.5 * unique(rep) * (msd / qt(1 - (1 - (unique(alpha)) / 2)^(1 / k), unique(df)))^2, 3)
  return(mse)
}

Scheffe_MSE <- function(msd, rep, alpha, df, n_factor) {
  mse <- round((unique(rep) * msd^2) / (2 * (unique(n_factor) - 1) * qf(1 - unique(alpha), unique(n_factor) - 1, unique(df))), 3)
  return(mse)
}

# Two-way functions
Fisher_MSE_ab <- function(msd, rep, alpha, df, n_other_factor) {
  mse <- round(0.5 * unique(rep) * unique(n_other_factor) * (msd / qt(1 - alpha / 2, df))^2, 3)
  return(mse)
}

Tukey_MSE_ab <- function(msd, rep, alpha, df, n_factor, n_other_factor) {
  mse <- round(unique(n_other_factor) * unique(rep) * (msd / qtukey(1 - unique(alpha), unique(n_factor), unique(df)))^2, 3)
  return(mse)
}

Bonferroni_MSE_ab <- function(msd, rep, alpha, df, n_factor, n_other_factor) {
  k <- choose(unique(n_factor), 2)
  mse <- round(0.5 * unique(n_other_factor) * unique(rep) * (msd / qt(1 - ((unique(alpha) / 2) / k), unique(df)))^2, 3)
  return(mse)
}

Sidak_MSE_ab <- function(msd, rep, alpha, df, n_factor, n_other_factor) {
  k <- choose(unique(n_factor), 2)
  mse <- round(0.5 * unique(n_other_factor) * unique(rep) * (msd / qt(1 - (1 - (unique(alpha)) / 2)^(1 / k), unique(df)))^2, 3)
  return(mse)
}

Scheffe_MSE_ab <- function(msd, rep, alpha, df, n_factor, n_other_factor) {
  mse <- round((unique(rep) * unique(n_other_factor) * msd^2) / (2 * (unique(n_factor) - 1) * qf(1 - unique(alpha), unique(n_factor) - 1, unique(df))), 3)
  return(mse)
}

# Design and separator lists
exp_des <- list(
  "Completely randomized design (CRD)",
  "Randomized complete block design (RCBD)",
  "Latin square",
  "Two-way complete factorial as a CRD",
  "Two-way complete factorial as a RCBD",
  "Split-plot as a CRD",
  "Split-plot as a RCBD"
)

separators <- list(
  "Fisher's LSD",
  "Tukey's HSD",
  "Bonferroni correction for multiple comparison",
  "Sidak correction for multiple comparison",
  "Scheffe's"
)

# Example files
rcbd_fisher_raw <- "https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/rcbd_fisher.csv"
latin_fisher_raw <- "https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/latin_fisher.csv"
factorial_rcbd_a_fisher_raw <- "https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/factorial_rcbd_a_fisher.csv"
factorial_rcbd_ab_scheffe_raw <- "https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/factorial_rcbd_ab_scheffe.csv"
sp_rcbd_a_sidak_raw <- "https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/sp_rcbd_a_sidak.csv"
sp_rcbd_b_bonferroni_raw <- "https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/sp_rcbd_b_bonferroni.csv"
sp_rcbd_b_within_a_tukey_raw <- "https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/sp_rcbd_b_within_a_tukey.csv"

# Custom CSS
custom_css <- "
  .btn-link-modern {
    background-color: transparent !important;
    color: #0072B2 !important;
    border: 1px solid #0072B2 !important;
    padding: 6px 12px !important;
    font-size: 13px !important;
    margin-left: 8px !important;
    border-radius: 4px !important;
  }
  
  .btn-link-modern:hover {
    background-color: #0072B2 !important;
    color: white !important;
  }
  
  .alert-warning {
    background-color: #fcf8e3;
    border-color: #faebcc;
    color: #8a6d3b;
    padding: 15px;
    margin-bottom: 20px;
    border: 1px solid transparent;
    border-radius: 4px;
  }
  
  .alert-info {
    background-color: #d9edf7;
    border-color: #bce8f1;
    color: #31708f;
    padding: 15px;
    margin-bottom: 20px;
    border: 1px solid transparent;
    border-radius: 4px;
  }
  
  .quick-start-step {
    padding: 12px 0;
    border-bottom: 1px solid #e2e8f0;
  }
  
  .quick-start-step:last-child {
    border-bottom: none;
  }
  
  .quick-start-step strong {
    color: #0072B2;
    font-size: 15px;
  }
  
  .download-section {
    background-color: #f8f9fa;
    padding: 20px;
    border-radius: 4px;
    margin: 20px 0;
  }
  
  .download-section a {
    display: block;
    padding: 8px 0;
    color: #0072B2;
    text-decoration: none;
  }
  
  .download-section a:hover {
    color: #005a8c;
    text-decoration: underline;
  }
  
  .feature-card {
    background-color: #ffffff;
    border-left: 4px solid #0072B2;
    padding: 20px;
    margin-bottom: 20px;
    border-radius: 4px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .feature-card h5 {
    color: #0072B2;
    margin-top: 0;
    font-weight: 600;
  }
  
  .feature-card ul {
    margin-bottom: 0;
  }
  
  .btn-load-example {
    background-color: #0072B2 !important;
    color: white !important;
    border: none !important;
    padding: 10px 20px !important;
    font-size: 14px !important;
    border-radius: 4px !important;
  }
  
  .btn-load-example:hover {
    background-color: #005a8c !important;
    color: white !important;
  }
  
  .section-header {
    color: #0072B2;
    border-bottom: 2px solid #0072B2;
    padding-bottom: 10px;
    margin-top: 30px;
    margin-bottom: 20px;
  }
  
  .visitor-stats {
    background-color: #f8f9fa;
    padding: 20px;
    border-radius: 4px;
    border-left: 4px solid #0072B2;
    margin: 20px 0;
  }
  
  .visitor-stats h5 {
    color: #0072B2;
    margin-top: 0;
    margin-bottom: 15px;
    font-weight: 600;
  }
  
  .visitor-stats ul {
    margin-bottom: 0;
    padding-left: 20px;
  }
  
  .visitor-stats li {
    margin-bottom: 8px;
    font-size: 14px;
  }
  
  .visitor-stats strong {
    color: #0072B2;
  }
"

# Tab Content =========================================================================

# Disclosure tab ----------------------------------------------
disclosure_tab <- tabItem(
  tabName = "Disclosure",
  tags$head(tags$style(HTML(custom_css))),
  
  fluidRow(
    column(12,
           div(style = "text-align: center; margin-bottom: 30px;",
               img(src = "logo.png", height = 150, width = 550)
           )
    )
  ),
  
  fluidRow(
    column(12,
           div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 4px;",
               p(strong("MSE FINDR"), " is an R Shiny application that estimates residual variance (mean square error) from balanced experimental studies reporting treatment means and post-hoc letter results.")
           )
    )
  ),
  
  fluidRow(
    column(12,
           h3(class = "section-header", "Key Features")
    )
  ),
  
  fluidRow(
    column(6,
           div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 4px;",
               h5("Multiple Experimental Designs"),
               tags$ul(
                 tags$li("Completely Randomized Design (CRD)"),
                 tags$li("Randomized Complete Block Design (RCBD)"),
                 tags$li("Latin Square"),
                 tags$li("Two-way Factorial (CRD & RCBD)"),
                 tags$li("Split-plot (CRD & RCBD)")
               )
           )
    ),
    column(6,
           div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 4px;",
               h5("Post-hoc Tests Supported"),
               tags$ul(
                 tags$li("Fisher's Least Significant Difference (LSD)"),
                 tags$li("Tukey's Honestly Significant Difference (HSD)"),
                 tags$li("Scheff\u00e9's Test"),
                 tags$li("Bonferroni Correction"),
                 tags$li("\u0160id\u00e1k Correction")
               )
           )
    )
  ),
  
  fluidRow(
    column(12,
           h3(class = "section-header", "Quick Start Guide")
    )
  ),
  
  fluidRow(
    box(
      title = NULL,
      width = 12,
      solidHeader = FALSE,
      div(class = "quick-start-step",
          p(strong("Step 1:"), " Organize trials with the same experimental design, post-hoc test, and significance level",
            actionButton("goto_step1", "View Tutorial \u2192",
                         onclick = "window.open('https://github.com/vcgarnica/MSE_FindR#step-1---compile-trials-with-the-same-information', '_blank')",
                         class = "btn-link-modern"))),
      div(class = "quick-start-step",
          p(strong("Step 2:"), " Create a CSV file with the required columns for your design",
            actionButton("goto_step2", "View Tutorial \u2192",
                         onclick = "window.open('https://github.com/vcgarnica/MSE_FindR#step-2---understanding-how-information-is-organized-and-creating-the-csv-input-file', '_blank')",
                         class = "btn-link-modern"))),
      div(class = "quick-start-step",
          p(strong("Step 3:"), " Upload your CSV file in the 'Upload File' tab")),
      div(class = "quick-start-step",
          p(strong("Step 4:"), " Select your experimental design and assign columns in the 'Estimator' tab")),
      div(class = "quick-start-step",
          p(strong("Step 5:"), " Click 'Estimate' to calculate MSE and download your results"))
    )
  ),
  
  fluidRow(
    column(12,
           h3(class = "section-header", "Resources")
    )
  ),
  
  fluidRow(
    column(6,
           div(class = "feature-card",
               h5("Documentation"),
               p("A comprehensive tutorial and evaluation of MSE FINDR is available on our",
                 tags$a(href = "https://github.com/vcgarnica/MSE_FindR", target = "_blank", "GitHub repository", style = "color: #0072B2;"), ".")
           )
    ),
    column(6,
           div(class = "feature-card",
               h5("Example Files"),
               p("Download example CSV files to explore how MSE FINDR works:"),
               downloadLink("rcbd_fisher", "\u2022 RCBD with Fisher's LSD"), br(),
               downloadLink("latin_fisher", "\u2022 Latin Square with Fisher's LSD"), br(),
               downloadLink("factorial_rcbd_a_fisher", "\u2022 2-way Factorial RCBD (Factor A)"), br(),
               downloadLink("factorial_rcbd_ab_scheffe", "\u2022 2-way Factorial RCBD (A\u00d7B)"), br(),
               downloadLink("sp_rcbd_a_sidak", "\u2022 Split-plot RCBD (Main-plot)"), br(),
               downloadLink("sp_rcbd_b_bonferroni", "\u2022 Split-plot RCBD (Sub-plot)"), br(),
               downloadLink("sp_rcbd_b_within_a_tukey", "\u2022 Split-plot RCBD (B within A)")
           )
    )
  ),
  
  fluidRow(
    column(12,
           div(style = "background-color: #f8f9fa; padding: 30px; border-radius: 4px; text-align: center; margin: 20px 0;",
               p("Load an example dataset to see how MSE FINDR works", style = "font-size: 16px; margin-bottom: 20px;"),
               actionButton("load_example_btn", "Load Example Data",
                            icon = icon("table"),
                            class = "btn-load-example")
           )
    )
  ),
  
  fluidRow(
    column(12,
           h3(class = "section-header", "Citation")
    )
  ),
  
  fluidRow(
    column(12,
           div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 4px;",
               p(strong("Garnica, V. C., Shah, D. A., Esker, P. D., Ojiambo, P. S."), " (2024). MSE FINDR: A Shiny R Application to Estimate Mean Square Error Using Treatment Means and Post-hoc Test Results. ", em("Plant Disease."), " doi: ",
                 tags$a(href = "https://doi.org/10.1094/PDIS-11-23-2519-SR", target = "_blank", "10.1094/PDIS-11-23-2519-SR", style = "color: #0072B2;"))
           )
    )
  ),
  
  fluidRow(
    column(12,
           h3(class = "section-header", "Development Team")
    )
  ),
  
  fluidRow(
    column(3,
           div(class = "feature-card",
               p(strong("Vinicius Garnica"), br(),
                 "The Ohio State University", br(),
                 tags$a(href = "mailto:garnica.vinicius@gmail.com", "garnica.vinicius@gmail.com", style = "color: #0072B2;"))
           )
    ),
    column(3,
           div(class = "feature-card",
               p(strong("Denis Shah"), br(),
                 "Kansas State University", br(),
                 tags$a(href = "mailto:denisshah2331@gmail.com", "denisshah2331@gmail.com", style = "color: #0072B2;"))
           )
    ),
    column(3,
           div(class = "feature-card",
               p(strong("Paul Esker"), br(),
                 "Pennsylvania State University", br(),
                 tags$a(href = "mailto:pde6@psu.edu", "pde6@psu.edu", style = "color: #0072B2;"))
           )
    ),
    column(3,
           div(class = "feature-card",
               p(strong("Peter Ojiambo"), br(),
                 "North Carolina State University", br(),
                 tags$a(href = "mailto:pojiamb@ncsu.edu", "pojiamb@ncsu.edu", style = "color: #0072B2;"))
           )
    )
  ),
  
  fluidRow(
    column(12,
           h3(class = "section-header", "Disclaimer")
    )
  ),
  
  fluidRow(
    column(12,
           div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 4px;",
               p("MSE FINDR was developed to help researchers estimate MSE from balanced experiments supported by the platform. We welcome feedback and suggestions but make no guarantee of correctness, reliability, or utility of results if incorrect selections are made during MSE estimation."),
               p("MSE FINDR is freely accessible, and the source code is hosted at ",
                 tags$a(href = "https://github.com/vcgarnica/MSE-FindR", target = "_blank", "https://github.com/vcgarnica/MSE-FindR", style = "color: #0072B2;"), ".")
           )
    )
  ),
  
  fluidRow(
    column(12,
           h3(class = "section-header", "License")
    )
  ),
  
  fluidRow(
    column(12,
           div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 4px;",
               p(strong("MIT License")),
               p("Copyright \u00a9 2022\u20132026 Vinicius Garnica and others"),
               p("Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:"),
               p("The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software."),
               p("THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.")
           )
    )
  )
)

# Upload file tab ----------------------------------------------
upload_tab <- tabItem(
  tabName = "FileUpload",
  tags$head(tags$style(HTML(custom_css))),
  fluidPage(
    h4("Upload file"),
    sidebarPanel(
      fileInput('file1', 'Choose file to upload:',
                accept = c('text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain', '.csv', '.tsv')),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"')
    ),
    mainPanel(
      column(width = 12,
             DT::dataTableOutput('contents'))
    )
  )
)

# Estimator tab --------------------------------------------------------------------
estimator_tab <- tabItem(
  tabName = "Estimator",
  tags$head(tags$style(HTML(custom_css))),
  fluidPage(
    fluidRow(
      box(
        title = "Design", width = 6, solidHeader = TRUE,
        fluidRow(
          column(8,
                 selectInput("exp_design", 'Experimental design', choices = exp_des),
                 selectInput("mean_sep", "Choose post hoc test", choices = separators),
                 sliderInput('alpha', "Significance level ", 0.05, min = 0.01, max = 0.10))
        ),
        br(),
        br(),
        actionButton("go_button", "Estimate", icon = icon("calculator"), class = "btn-success"),
        br(),
        br(),
        br(),
        uiOutput("downloadData")
      ),
      box(
        title = "Instructions",
        width = 6,
        solidHeader = TRUE,
        status = "info",
        uiOutput("design_help")
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = "input.exp_design == 'Completely randomized design (CRD)' || input.exp_design == 'Randomized complete block design (RCBD)'",
        box(
          title = "Column assignment", width = 12, solidHeader = TRUE,
          column(3, selectInput("trial_id", label = tags$span("Trial identifier number", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Each trial in designated folder should be assigned a unique value \u2013 numerical values only")), NULL)),
          column(3, selectInput("factor_A", label = tags$span("Factor A", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for factor A treatments \u2013 either numerical or categorical")), NULL)),
          column(2, selectInput("replicates", label = tags$span("Number of replicates or blocks", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for replicates or blocks. Only one value per trial - numerical")), NULL)),
          column(2, selectInput("means", label = tags$span("Means", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for treatment means. No post-hoc letters allowed - numerical")), NULL)),
          column(2, selectInput("letters", label = tags$span("Post hoc test letters", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for post-hoc letters. No treatment means allowed - categorical")), NULL))
        )
      ),
      conditionalPanel(
        condition = "input.exp_design == 'Latin square'",
        box(
          title = "Column assignment", width = 12, solidHeader = TRUE,
          column(3, selectInput("trial_id_ls", label = tags$span("Trial identifier number", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Each trial in designated folder should be assigned a unique value \u2013 numerical values only")), NULL)),
          column(3, selectInput("factor_A_ls", label = tags$span("Factor A", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for factor A treatments \u2013 either numerical or categorical")), NULL)),
          column(3, selectInput("means_ls", label = tags$span("Means", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for treatment means. No post-hoc letters allowed - numerical")), NULL)),
          column(3, selectInput("letters_ls", label = tags$span("Post hoc test letters", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for post-hoc letters. No treatment means allowed - categorical")), NULL))
        )
      ),
      conditionalPanel(
        condition = "input.exp_design == 'Two-way complete factorial as a CRD' || input.exp_design == 'Two-way complete factorial as a RCBD' ",
        box(
          title = "Column assignment", width = 12, solidHeader = TRUE,
          column(12,
                 fluidRow(
                   column(3, selectInput("trial_id_ab", label = tags$span("Trial identifier number", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Each trial in designated folder should be assigned a unique value \u2013 numerical values only")), NULL)),
                   column(3, selectInput("variation_ab", label = tags$span("Source of variation", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "When interaction is not significant & comparisons were performed for the main effect (one factor omitted), select A or B (used interchangeably in factorial designs). When interaction is significant & comparisons were performed for the interaction, select A x B")), choices = c("A or B", "A x B"))),
                   column(3, selectInput("replicates_ab", label = tags$span("Number of replicates or blocks", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for replicates or blocks. Only one value per trial - numerical")), NULL)),
                   column(3, selectInput("means_ab", label = tags$span("Means", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for treatment means. No post-hoc letters allowed - numerical")), NULL))
                 ),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.variation_ab == 'A or B'",
                     column(6, selectInput("factor_A_aa", label = tags$span("Factor A or B", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for either factor A or factor B treatments, used interchangeably in factorial designs \u2013 either numerical or categorical")), NULL)),
                     column(6, selectInput("factor_B_aa", label = tags$span("Number of levels in omitted factor", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "If trial reports were carried out as a two-way factorial and only treatment means and post-hoc test for one factor are reported, users must indicate the number of levels of the omitted factor \u2013 numerical")), NULL))
                   ),
                   conditionalPanel(
                     condition = "input.variation_ab == 'A x B'",
                     column(6, selectInput("factor_A_ab", label = tags$span("Factor A", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for factor A treatments \u2013 either numerical or categorical")), NULL)),
                     column(6, selectInput("factor_B_ab", label = tags$span("Factor B", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for factor B treatments \u2013 either numerical or categorical")), NULL))
                   ),
                   column(12, selectInput("letters_ab", label = tags$span("Post hoc test letters", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for post-hoc letters. No treatment means allowed - categorical")), NULL))
                 )
          )
        )
      ),
      conditionalPanel(
        condition = "input.exp_design == 'Split-plot as a CRD'  || input.exp_design == 'Split-plot as a RCBD'",
        box(
          title = "Column assignment", width = 12, solidHeader = TRUE,
          column(12,
                 fluidRow(
                   column(3, selectInput("trial_id_sp", label = tags$span("Trial identifier number", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Each trial in designated folder should be assigned a unique value \u2013 numerical values only")), NULL)),
                   column(3, selectInput("variation_sp", label = tags$span("Source of variation", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "When interaction is not significant & comparisons were performed for the main effect (one factor omitted) & were assigned to main-plot units, select A (main-plot). Similarly, when interaction is not significant & comparisons were performed for the main effect (one factor omitted) & were assigned to sub-plot units, select B (sub-plot). When interaction is significant & trials report treatment means and post-hoc test results for the interaction & comparisons were performed among main-plot levels within common subplot levels, select B (sub-plot) within A (main-plot)")), choices = c("A (main-plot)", "B (sub-plot)", "B (sub-plot) within A (main-plot)"))),
                   column(3, selectInput("replicates_sp", label = tags$span("Number of replicates or blocks", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for replicates or blocks. Only one value per trial - numerical")), NULL)),
                   column(3, selectInput("means_sp", label = tags$span("Means", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for treatment means. No post-hoc letters allowed - numerical")), NULL))
                 ),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.variation_sp == 'A (main-plot)'",
                     column(6, selectInput("factor_A_sp", label = tags$span("Factor A (main-plot)", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for factor A treatments assigned to the main-plot units \u2013 either numerical or categorical")), NULL)),
                     column(6, selectInput("factor_B_omit_sp", label = tags$span("Number of levels in omitted factor", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "If trial reports were carried out as a two-way split-plot and only treatment means and post-hoc test for one factor are reported, users must indicate the number of levels of the omitted factor \u2013 numerical")), NULL))
                   ),
                   conditionalPanel(
                     condition = "input.variation_sp == 'B (sub-plot)'",
                     column(6, selectInput("factor_A_omit_sp", label = tags$span("Number of levels in omitted factor", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "If trial reports were carried out as a two-way split-plot and only treatment means and post-hoc test for one factor are reported, users must indicate the number of levels of the omitted factor \u2013 numerical")), NULL)),
                     column(6, selectInput("factor_B_sp", label = tags$span("Factor B (sub-plot)", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for factor B treatments assigned to the sub-plot units \u2013 either numerical or categorical")), NULL))
                   ),
                   conditionalPanel(
                     condition = "input.variation_sp == 'B (sub-plot) within A (main-plot)'",
                     column(6, selectInput("factor_A_sp_ab", label = tags$span("Factor A (main-plot)", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for factor A treatments assigned to the main-plot units \u2013 either numerical or categorical")), NULL)),
                     column(6, selectInput("factor_B_sp_ab", label = tags$span("Factor B (sub-plot)", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for factor B treatments assigned to the sub-plot units \u2013 either numerical or categorical")), NULL))
                   ),
                   column(12, selectInput("letters_sp", label = tags$span("Post hoc test letters", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "Column for post-hoc letters. No treatment means allowed - categorical")), NULL))
                 )
          )
        )
      )
    ),
    br(),
    column(12,
           DT::dataTableOutput('contents2')
    )
  )
)

# Visitor Statistics Tab ----------------------------------------------
visitor_tab <- tabItem(
  tabName = "VisitorStats",
  tags$head(tags$style(HTML(custom_css))),
  
  fluidRow(
    column(12,
           h3(class = "section-header", "Usage and Impact")
    )
  ),
  
  fluidRow(
    uiOutput("visitor_stats")
  )
)

# SideBar content =========================================================================

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1D3",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

sideBar_content <- dashboardSidebar(
  use_theme(mytheme),
  sidebarMenu(
    id = "sidebar_menu",
    menuItem("Disclosure", tabName = "Disclosure"),
    menuItem("Upload file", tabName = "FileUpload"),
    menuItem("Estimator", tabName = "Estimator"),
    menuItem("Usage statistics", tabName = "VisitorStats")
  )
)

# BODY content ------------------------------------------------------------------------------

body_content <- dashboardBody(
  tags$head(tags$style(HTML(custom_css))),
  tabItems(
    disclosure_tab,
    upload_tab,
    estimator_tab,
    visitor_tab
  )
)

# UI =========================================================================

ui <- dashboardPage(
  dashboardHeader(title = "MSE FINDR"),
  sideBar_content,
  body_content
)

# Server =========================================================================
server <- function(input, output, session) {
  
  # Initialize data storage
  rv <- reactiveValues(data = NULL)
  
  # ---------------------------------------------------------------
  # VISITOR COUNTER - runs once per session on app load
  # ---------------------------------------------------------------
  visitor_count <- reactiveVal(0)
  
  observe({
    if (is.null(session$userData$counted)) {
      new_count <- increment_count_api()
      if (!is.null(new_count)) {
        visitor_count(new_count)
      }
      session$userData$counted <- TRUE
    }
  })
  
  # Load sheet from file upload
  sheet <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    }
    inFile <- input$file1
    tbl <- read.csv(inFile$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote,
                    stringsAsFactors = FALSE)
    rv$data <- tbl
    tbl
  })
  
  # Display uploaded data
  output$contents <- DT::renderDataTable({
    DT::datatable(rv$data, options = list(searching = FALSE, scrollX = TRUE))
  })
  
  # Update select inputs
  observe({
    value <- names(sheet())
    updateSelectInput(session, "get_let_mean", choices = value)
  })
  
  observe({
    value <- names(rv$data)
    updateSelectInput(session, "trial_id", choices = value)
    updateSelectInput(session, "replicates", choices = value)
    updateSelectInput(session, "factor_A", choices = value)
    updateSelectInput(session, "means", choices = value)
    updateSelectInput(session, "letters", choices = value)
  })
  
  observe({
    value <- names(rv$data)
    updateSelectInput(session, "trial_id_ls", choices = value)
    updateSelectInput(session, "factor_A_ls", choices = value)
    updateSelectInput(session, "means_ls", choices = value)
    updateSelectInput(session, "letters_ls", choices = value)
  })
  
  observe({
    value <- names(rv$data)
    updateSelectInput(session, "trial_id_ab", choices = value)
    updateSelectInput(session, "replicates_ab", choices = value)
    updateSelectInput(session, "factor_A_aa", choices = value)
    updateSelectInput(session, "factor_B_aa", choices = value)
    updateSelectInput(session, "factor_A_ab", choices = value)
    updateSelectInput(session, "factor_B_ab", choices = value)
    updateSelectInput(session, "means_ab", choices = value)
    updateSelectInput(session, "letters_ab", choices = value)
  })
  
  observe({
    value <- names(rv$data)
    updateSelectInput(session, "trial_id_sp", choices = value)
    updateSelectInput(session, "replicates_sp", choices = value)
    updateSelectInput(session, "factor_A_sp", choices = value)
    updateSelectInput(session, "factor_B_omit_sp", choices = value)
    updateSelectInput(session, "factor_B_sp", choices = value)
    updateSelectInput(session, "factor_A_omit_sp", choices = value)
    updateSelectInput(session, "factor_A_sp_ab", choices = value)
    updateSelectInput(session, "factor_B_sp_ab", choices = value)
    updateSelectInput(session, "means_sp", choices = value)
    updateSelectInput(session, "letters_sp", choices = value)
  })
  
  # Design-specific help text
  output$design_help <- renderUI({
    req(input$exp_design)
    
    help_text <- switch(
      input$exp_design,
      "Completely randomized design (CRD)" =
        tagList(
          p(strong("Required CSV columns:")),
          tags$ul(
            tags$li("Trial ID (numerical)"),
            tags$li("Factor A (numerical or categorical)"),
            tags$li("Number of replicates (numerical, same for all treatments in a trial)"),
            tags$li("Treatment means (numerical)"),
            tags$li("Post-hoc letters (categorical)")
          )
        ),
      "Randomized complete block design (RCBD)" =
        tagList(
          p(strong("Required CSV columns:")),
          tags$ul(
            tags$li("Trial ID (numerical)"),
            tags$li("Factor A (numerical or categorical)"),
            tags$li("Number of blocks (numerical, same for all treatments in a trial)"),
            tags$li("Treatment means (numerical)"),
            tags$li("Post-hoc letters (categorical)")
          )
        ),
      "Latin square" =
        tagList(
          p(strong("Required CSV columns:")),
          tags$ul(
            tags$li("Trial ID (numerical)"),
            tags$li("Factor A (numerical or categorical)"),
            tags$li("Treatment means (numerical)"),
            tags$li("Post-hoc letters (categorical)")
          ),
          p(em("Note: Number of rows, columns, and treatments must be equal"))
        ),
      "Two-way complete factorial as a CRD" =
        tagList(
          p(strong("If A\u00d7B interaction is NOT significant:")),
          tags$ul(
            tags$li("Include compared factor (A or B)"),
            tags$li("Specify number of levels in omitted factor")
          ),
          p(strong("If A\u00d7B interaction IS significant:")),
          tags$ul(
            tags$li("Include both Factor A and Factor B columns")
          ),
          p(em("See tutorial for detailed instructions"))
        ),
      "Two-way complete factorial as a RCBD" =
        tagList(
          p(strong("If A\u00d7B interaction is NOT significant:")),
          tags$ul(
            tags$li("Include compared factor (A or B)"),
            tags$li("Specify number of levels in omitted factor")
          ),
          p(strong("If A\u00d7B interaction IS significant:")),
          tags$ul(
            tags$li("Include both Factor A and Factor B columns")
          ),
          p(em("See tutorial for detailed instructions"))
        ),
      "Split-plot as a CRD" =
        tagList(
          div(class = "alert alert-warning",
              p(strong("\u26a0 IMPORTANT:")),
              p("Specify whether comparisons are for:"),
              tags$ul(
                tags$li("A (main-plot)"),
                tags$li("B (sub-plot)"),
                tags$li("B within A")
              ),
              p(strong("Misspecification will result in incorrect MSE!"))
          ),
          p(em("Main and sub-plot assignments are NOT interchangeable in split-plot designs"))
        ),
      "Split-plot as a RCBD" =
        tagList(
          div(class = "alert alert-warning",
              p(strong("\u26a0 IMPORTANT:")),
              p("Specify whether comparisons are for:"),
              tags$ul(
                tags$li("A (main-plot)"),
                tags$li("B (sub-plot)"),
                tags$li("B within A")
              ),
              p(strong("Misspecification will result in incorrect MSE!"))
          ),
          p(em("Main and sub-plot assignments are NOT interchangeable in split-plot designs"))
        ),
      p("")
    )
    
    help_text
  })
  
  # Example data loader
  observeEvent(input$load_example_btn, {
    showModal(modalDialog(
      title = "Load Example Dataset",
      size = "l",
      selectInput("example_choice", "Choose example:",
                  choices = c(
                    "RCBD with Fisher's LSD" = "rcbd_fisher",
                    "Latin Square with Fisher's LSD" = "latin_fisher",
                    "2-way factorial RCBD (A present, B omitted)" = "factorial_rcbd_a",
                    "2-way factorial RCBD (A\u00d7B present)" = "factorial_rcbd_ab",
                    "Split-plot RCBD (A main-plot)" = "sp_rcbd_a",
                    "Split-plot RCBD (B sub-plot)" = "sp_rcbd_b",
                    "Split-plot RCBD (B within A)" = "sp_rcbd_b_within_a"
                  )),
      hr(),
      h5("Preview:"),
      DT::dataTableOutput("example_preview"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_load", "Load This Example", class = "btn-primary")
      )
    ))
  })
  
  output$example_preview <- DT::renderDataTable({
    req(input$example_choice)
    
    example_url <- switch(
      input$example_choice,
      "rcbd_fisher" = rcbd_fisher_raw,
      "latin_fisher" = latin_fisher_raw,
      "factorial_rcbd_a" = factorial_rcbd_a_fisher_raw,
      "factorial_rcbd_ab" = factorial_rcbd_ab_scheffe_raw,
      "sp_rcbd_a" = sp_rcbd_a_sidak_raw,
      "sp_rcbd_b" = sp_rcbd_b_bonferroni_raw,
      "sp_rcbd_b_within_a" = sp_rcbd_b_within_a_tukey_raw
    )
    
    preview_data <- read.csv(url(example_url), stringsAsFactors = FALSE)
    DT::datatable(head(preview_data, 10),
                  options = list(dom = 't', scrollX = TRUE))
  })
  
  # FIX: Use read.csv (not read_csv) for consistent types with file upload path
  observeEvent(input$confirm_load, {
    example_url <- switch(
      input$example_choice,
      "rcbd_fisher" = rcbd_fisher_raw,
      "latin_fisher" = latin_fisher_raw,
      "factorial_rcbd_a" = factorial_rcbd_a_fisher_raw,
      "factorial_rcbd_ab" = factorial_rcbd_ab_scheffe_raw,
      "sp_rcbd_a" = sp_rcbd_a_sidak_raw,
      "sp_rcbd_b" = sp_rcbd_b_bonferroni_raw,
      "sp_rcbd_b_within_a" = sp_rcbd_b_within_a_tukey_raw
    )
    
    # Use read.csv to match the file upload pathway exactly
    rv$data <- read.csv(url(example_url), stringsAsFactors = FALSE)
    removeModal()
    
    updateTabItems(session, "sidebar_menu", "FileUpload")
    
    showNotification(
      "Example data loaded successfully!",
      type = "message",
      duration = 5
    )
  })
  
  # Data filtering function - ALL MATH IDENTICAL TO ORIGINAL
  # FIX: Removed validate(need(input$file1...)) — now works with both
  #       file upload and example loader pathways
  data_filtered <- reactive({
    req(rv$data)
    
    dt <- rv$data
    
    if (input$exp_design == "Completely randomized design (CRD)" || input$exp_design == "Randomized complete block design (RCBD)") {
      dt <- dt %>%
        dplyr::group_by(.data[[input$trial_id]]) %>%
        dplyr::mutate(
          n_factor_A = length(unique(.data[[input$factor_A]])),
          n_replicates = .data[[input$replicates]],
          df_error = if (input$exp_design == "Completely randomized design (CRD)") {
            n_factor_A * (n_replicates - 1)
          } else {
            (n_factor_A - 1) * (n_replicates - 1)
          }
        )
    } else if (input$exp_design == "Latin square") {
      dt <- dt %>%
        dplyr::group_by(.data[[input$trial_id_ls]]) %>%
        dplyr::mutate(
          n_factor_A = length(unique(.data[[input$factor_A_ls]])),
          n_replicates = length(unique(.data[[input$factor_A_ls]])),
          df_error = (n_factor_A - 1) * (n_factor_A - 2)
        )
    } else if (input$exp_design == "Two-way complete factorial as a CRD" || input$exp_design == "Two-way complete factorial as a RCBD") {
      if (input$variation_ab == "A or B") {
        dt <- dt %>%
          dplyr::group_by(.data[[input$trial_id_ab]]) %>%
          dplyr::mutate(
            n_factor_A = length(unique(.data[[input$factor_A_aa]])),
            n_factor_B = (.data[[input$factor_B_aa]]),
            n_replicates = (.data[[input$replicates_ab]])
          )
      } else if (input$variation_ab == "A x B") {
        dt <- dt %>%
          dplyr::group_by(.data[[input$trial_id_ab]]) %>%
          dplyr::mutate(
            n_factor_A = length(unique(.data[[input$factor_A_ab]])),
            n_factor_B = length(unique(.data[[input$factor_B_ab]])),
            n_replicates = (.data[[input$replicates_ab]])
          )
      }
      
      if (input$exp_design == "Two-way complete factorial as a CRD") {
        dt <- dt %>%
          dplyr::group_by(.data[[input$trial_id_ab]]) %>%
          dplyr::mutate(df_error = n_factor_A * n_factor_B * (n_replicates - 1))
      } else if (input$exp_design == "Two-way complete factorial as a RCBD") {
        dt <- dt %>%
          dplyr::group_by(.data[[input$trial_id_ab]]) %>%
          dplyr::mutate(df_error = (n_factor_A * n_factor_B - 1) * (n_replicates - 1))
      }
    } else if (input$exp_design == "Split-plot as a CRD" || input$exp_design == "Split-plot as a RCBD") {
      if (input$variation_sp == "A (main-plot)") {
        dt <- dt %>%
          dplyr::group_by(.data[[input$trial_id_sp]]) %>%
          dplyr::mutate(
            n_factor_A = length(unique(.data[[input$factor_A_sp]])),
            n_factor_B = (.data[[input$factor_B_omit_sp]]),
            n_replicates = (.data[[input$replicates_sp]])
          )
      } else if (input$variation_sp == "B (sub-plot)") {
        dt <- dt %>%
          dplyr::group_by(.data[[input$trial_id_sp]]) %>%
          dplyr::mutate(
            n_factor_A = (.data[[input$factor_A_omit_sp]]),
            n_factor_B = length(unique(.data[[input$factor_B_sp]])),
            n_replicates = (.data[[input$replicates_sp]])
          )
      } else if (input$variation_sp == "B (sub-plot) within A (main-plot)") {
        dt <- dt %>%
          dplyr::group_by(.data[[input$trial_id_sp]]) %>%
          dplyr::mutate(
            n_factor_A = length(unique(.data[[input$factor_A_sp_ab]])),
            n_factor_B = length(unique(.data[[input$factor_B_sp_ab]])),
            n_replicates = (.data[[input$replicates_sp]])
          )
      }
      
      if (input$variation_sp == "A (main-plot)") {
        if (input$exp_design == "Split-plot as a CRD") {
          dt <- dt %>%
            dplyr::group_by(.data[[input$trial_id_sp]]) %>%
            dplyr::mutate(df_error = n_factor_A * (n_replicates - 1))
        } else if (input$exp_design == "Split-plot as a RCBD") {
          dt <- dt %>%
            dplyr::group_by(.data[[input$trial_id_sp]]) %>%
            dplyr::mutate(df_error = (n_factor_A - 1) * (n_replicates - 1))
        }
      } else if (input$variation_sp == "B (sub-plot)" || input$variation_sp == "B (sub-plot) within A (main-plot)") {
        dt <- dt %>%
          dplyr::group_by(.data[[input$trial_id_sp]]) %>%
          dplyr::mutate(df_error = n_factor_A * (n_replicates - 1) * (n_factor_B - 1))
      }
    }
    
    return(dt)
  })
  
  # Main calculation function - ALL MATH IDENTICAL TO ORIGINAL
  addData <- eventReactive(input$go_button, {
    dt <- data_filtered()
    
    if (input$exp_design == "Completely randomized design (CRD)" || input$exp_design == "Randomized complete block design (RCBD)") {
      dt <- dt %>%
        dplyr::group_by(.data[[input$trial_id]]) %>%
        mutate(
          msd = mean(get_MSD(.data[[input$means]], .data[[input$letters]])$n),
          MSE = switch(
            input$mean_sep,
            "Fisher's LSD" = Fisher_MSE(msd, n_replicates, input$alpha, df_error),
            "Tukey's HSD" = Tukey_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A),
            "Bonferroni correction for multiple comparison" = Bonferroni_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A),
            "Sidak correction for multiple comparison" = Sidak_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A),
            "Scheffe's" = Scheffe_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A)
          )
        ) %>%
        select(-msd, -n_factor_A, -n_replicates)
      
    } else if (input$exp_design == "Latin square") {
      dt <- dt %>%
        dplyr::group_by(.data[[input$trial_id_ls]]) %>%
        mutate(
          msd = mean(get_MSD(.data[[input$means_ls]], .data[[input$letters_ls]])$n),
          MSE = switch(
            input$mean_sep,
            "Fisher's LSD" = Fisher_MSE(msd, n_replicates, input$alpha, df_error),
            "Tukey's HSD" = Tukey_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A),
            "Bonferroni correction for multiple comparison" = Bonferroni_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A),
            "Sidak correction for multiple comparison" = Sidak_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A),
            "Scheffe's" = Scheffe_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A)
          )
        ) %>%
        select(-msd, -n_factor_A, -n_replicates)
      
    } else if (input$exp_design == "Two-way complete factorial as a CRD" || input$exp_design == "Two-way complete factorial as a RCBD") {
      dt <- dt %>%
        dplyr::group_by(.data[[input$trial_id_ab]]) %>%
        mutate(
          msd = mean(get_MSD(.data[[input$means_ab]], .data[[input$letters_ab]])$n),
          MSE = if (input$variation_ab == "A or B") {
            switch(
              input$mean_sep,
              "Fisher's LSD" = Fisher_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B),
              "Tukey's HSD" = Tukey_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
              "Bonferroni correction for multiple comparison" = Bonferroni_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
              "Sidak correction for multiple comparison" = Sidak_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
              "Scheffe's" = Scheffe_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B)
            )
          } else {
            switch(
              input$mean_sep,
              "Fisher's LSD" = Fisher_MSE(msd, n_replicates, input$alpha, df_error),
              "Tukey's HSD" = Tukey_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A * n_factor_B),
              "Bonferroni correction for multiple comparison" = Bonferroni_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A * n_factor_B),
              "Sidak correction for multiple comparison" = Sidak_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A * n_factor_B),
              "Scheffe's" = Scheffe_MSE(msd, n_replicates, input$alpha, df_error, n_factor_A * n_factor_B)
            )
          }
        ) %>%
        select(-msd, -n_factor_A, -n_factor_B, -n_replicates)
      
    } else if (input$exp_design == "Split-plot as a CRD" || input$exp_design == "Split-plot as a RCBD") {
      dt <- dt %>%
        dplyr::group_by(.data[[input$trial_id_sp]]) %>%
        mutate(
          msd = mean(get_MSD(.data[[input$means_sp]], .data[[input$letters_sp]])$n),
          MSE = switch(
            input$variation_sp,
            "A (main-plot)" = switch(
              input$mean_sep,
              "Fisher's LSD" = Fisher_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B),
              "Tukey's HSD" = Tukey_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
              "Bonferroni correction for multiple comparison" = Bonferroni_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
              "Sidak correction for multiple comparison" = Sidak_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
              "Scheffe's" = Scheffe_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B)
            ),
            "B (sub-plot)" = switch(
              input$mean_sep,
              "Fisher's LSD" = Fisher_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A),
              "Tukey's HSD" = Tukey_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B, n_factor_A),
              "Bonferroni correction for multiple comparison" = Bonferroni_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B, n_factor_A),
              "Sidak correction for multiple comparison" = Sidak_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B, n_factor_A),
              "Scheffe's" = Scheffe_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B, n_factor_A)
            ),
            "B (sub-plot) within A (main-plot)" = switch(
              input$mean_sep,
              "Fisher's LSD" = Fisher_MSE(msd, n_replicates, input$alpha, df_error),
              "Tukey's HSD" = Tukey_MSE(msd, n_replicates, input$alpha, df_error, n_factor_B),
              "Bonferroni correction for multiple comparison" = Bonferroni_MSE(msd, n_replicates, input$alpha, df_error, n_factor_B),
              "Sidak correction for multiple comparison" = Sidak_MSE(msd, n_replicates, input$alpha, df_error, n_factor_B),
              "Scheffe's" = Scheffe_MSE(msd, n_replicates, input$alpha, df_error, n_factor_B)
            )
          )
        ) %>%
        select(-msd, -n_factor_A, -n_factor_B, -n_replicates)
    }
    
    return(dt)
  })
  
  # Display results
  output$contents2 <- DT::renderDataTable({
    DT::datatable(addData(), options = list(
      searching = FALSE,
      pageLength = 20,
      scrollX = TRUE
    ))
  })
  
  # Download handlers
  output$downloadData01 <- downloadHandler(
    filename = function() {
      paste("result_MSEFindR.csv", sep = "")
    },
    content = function(file) {
      write.csv(addData(), file)
    }
  )
  
  # FIX: Use req(rv$data) instead of req(input$file1) so download works
  #      with both file upload and example loader
  output$downloadData <- renderUI({
    req(rv$data, addData())
    downloadButton("downloadData01")
  })
  
  # Download example files
  output$latin_fisher <- downloadHandler(
    filename = function() { "latin_fisher.csv" },
    content = function(file) { write.csv(read.csv(url(latin_fisher_raw), stringsAsFactors = FALSE), file) }
  )
  
  output$rcbd_fisher <- downloadHandler(
    filename = function() { "rcbd_fisher.csv" },
    content = function(file) { write.csv(read.csv(url(rcbd_fisher_raw), stringsAsFactors = FALSE), file) }
  )
  
  output$factorial_rcbd_a_fisher <- downloadHandler(
    filename = function() { "factorial_rcbd_a_fisher.csv" },
    content = function(file) { write.csv(read.csv(url(factorial_rcbd_a_fisher_raw), stringsAsFactors = FALSE), file) }
  )
  
  output$factorial_rcbd_ab_scheffe <- downloadHandler(
    filename = function() { "factorial_rcbd_ab_scheffe.csv" },
    content = function(file) { write.csv(read.csv(url(factorial_rcbd_ab_scheffe_raw), stringsAsFactors = FALSE), file) }
  )
  
  output$sp_rcbd_a_sidak <- downloadHandler(
    filename = function() { "sp_rcbd_a_sidak.csv" },
    content = function(file) { write.csv(read.csv(url(sp_rcbd_a_sidak_raw), stringsAsFactors = FALSE), file) }
  )
  
  output$sp_rcbd_b_bonferroni <- downloadHandler(
    filename = function() { "sp_rcbd_b_bonferroni.csv" },
    content = function(file) { write.csv(read.csv(url(sp_rcbd_b_bonferroni_raw), stringsAsFactors = FALSE), file) }
  )
  
  output$sp_rcbd_b_within_a_tukey <- downloadHandler(
    filename = function() { "sp_rcbd_b_within_a_tukey.csv" },
    content = function(file) { write.csv(read.csv(url(sp_rcbd_b_within_a_tukey_raw), stringsAsFactors = FALSE), file) }
  )
  
  # ---------------------------------------------------------------
  # VISITOR STATS TAB - renders the counter and Dimensions badge
  # ---------------------------------------------------------------
  output$visitor_stats <- renderUI({
    current_count <- visitor_count()
    
    tagList(
      fluidRow(
        column(6,
               div(class = "visitor-stats",
                   style = "padding: 20px; border-radius: 10px; background: #fdfdfd; border: 1px solid #eee; text-align: center; height: 180px;",
                   h5(icon("users"), " Total Tool Reach"),
                   div(style = "font-size: 48px; color: #0072B2; font-weight: bold; margin-top: 20px;",
                       format(current_count, big.mark = ",")),
                   p("Visitors since January 2026", style = "color: #888;")
               )
        ),
        column(6,
               div(class = "visitor-stats",
                   style = "padding: 20px; border-radius: 10px; background: #fdfdfd; border: 1px solid #eee; text-align: center; height: 180px;",
                   h5(icon("quote-left"), " Live Citation Impact"),
                   div(style = "margin-top: 15px;",
                       HTML('<span class="__dimensions_badge_embed__"
                                 data-doi="10.1094/PDIS-11-23-2519-SR"
                                 data-style="large_rectangle"
                                 data-legend="hover-right"></span>
                           <script async src="https://badge.dimensions.ai/badge.js" charset="utf-8"></script>')
                   )
               )
        )
      ),
      fluidRow(
        column(12,
               div(style = "margin-top: 20px; padding: 10px; border-top: 1px solid #eee;",
                   p(icon("info-circle"), " Citation data is synchronized in real-time with the Dimensions.ai global database.")
               )
        )
      )
    )
  })
  
}

# Run shiny app ---------------------------------------------------------------------------
shinyApp(ui, server)