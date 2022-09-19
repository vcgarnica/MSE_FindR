# SHINY DASHBOARD - MSE_Finder

# A tool for estimating the pooled standard deviation (a.i. mean square error (MSE) from ANOVAs) 
# from published literature using available information. Assumes constant variance and treatments have the same number of replications.


# Packages library =================================================

# load packages

library("shiny")
library("shinydashboard")
library("tidyverse")
library("lubridate")
library("data.table")
library("DT")
library("dashboardthemes")
library ("readr")



# Functions =================================================

get_MSD<-function(value,letter){
  require(data.table)
  letter<-letter[order(value,decreasing = T)]
  value<-value[order(value,decreasing = T)]
  n<-abs(apply(combn(value,2), 2, diff))
  l<-combn(letter,2)
  dt<-as.data.table(t(rbind(n,l)))
  dt$n<-as.numeric(dt$n)
  dt$range=sequence((length(value)-1):1)+1
  dt<-dt[order(dt$n),]
  dt$inter <- !sapply(gsub(" ", "", paste(dt$V2, dt$V3)), function(x) any(str_count(x, letters)>1))
  dt[dt==""]<-NA
  dt<-na.omit(dt)
  if(all(dt$V2==dt$V3) || length(rle(dt$inter)$lengths)==1){
    data<-as.data.table(rbind(rep(NA,ncol(dt)),rep(NA,ncol(dt))))
    colnames(data) <- colnames(dt)
    return(data)}
  else{
    msd_1<-dt[inter==FALSE, .SD[which.max(n)], by=inter] 
    msd_2<-dt[inter==TRUE, .SD[which.min(n)], by=inter]
    msd<-rbind(msd_1,msd_2)}
  return(msd) 
}

get_MSD_SP<-function(value,letter,group){
  require(data.table)
  n<-abs(apply(combn(value,2), 2, diff))
  l<-combn(letter,2)
  k<-combn(group,2)
  dt<-as.data.table(t(rbind(n,l,k)))
  dt$n<-as.numeric(dt$n)
  dt<-dt[order(dt$n),]
  dt$inter <- !sapply(gsub(" ", "", paste(dt$V2, dt$V3)), function(x) any(str_count(x, letters)>1))
  dt[dt==""]<-NA
  dt<-na.omit(dt)
  dt<-dt[dt$V4==dt$V5,]
  if(all(dt$V2==dt$V3) || length(rle(dt$inter)$lengths)==1){
    data<-as.data.table(rbind(rep(NA,ncol(dt)),rep(NA,ncol(dt))))
    colnames(data) <- colnames(dt)
    return(data)}
  else{
    msd_1<-dt[inter==FALSE, .SD[which.max(n)], by=inter] 
    msd_2<-dt[inter==TRUE, .SD[which.min(n)], by=inter]
    msd<-rbind(msd_1,msd_2)}
  return(msd) 
}

# Expands compacted letters into full length strings (ex. a-d into abcd)
expand_letters<-function(string1) {
  paste(letters[do.call(`:`, as.list(match( strsplit(string1, "-")[[1]],letters)))], collapse="")
}

# Obtain and expand letters associated with means
get_letters<-function(variable){
  if(anyNA(variable)){
    return(NA_real_)}
  else{
    l1<-str_replace_all(variable, "[\r\n]", "") 
    l2<-gsub("[[:digit:]]","",l1)
    l3<-gsub("[.]","",l2)
    l4<-as.character(str_replace_all(l3, fixed(" "), ""))
    i1 <- grep('-',l4)
    l4[i1]<-sapply(l4[i1], expand_letters)
    return(l4)
  }
}

# Obtain values associated with means
get_values<-function(variable){
  if(anyNA(variable)){
    return(NA_real_)}
  else{
    v1<-gsub("[-[:alpha:]]", "\\1",variable)
    v2<-str_replace_all(v1, fixed(" "), "")
    value<-as.numeric(v2)
    return(value)}
}

# ---------------------------------------------------------------

Fisher_MSE<- function(msd,rep,alpha,df){
  mse<-0.5*unique(rep)*(msd/qt(1-unique(alpha)/2,unique(df)))^2
  return(mse)}
Tukey_MSE<- function(msd,rep,alpha,df,n_factor){
  mse<-unique(rep)*(msd/qtukey(1-unique(alpha),unique(n_factor),unique(df)))^2
  return(mse)
}
Bonferroni_MSE<- function(msd,rep,alpha,df,n_factor){
  k<-choose(unique(n_factor),2)
  mse<-0.5*unique(rep)*(msd/qt(1-((unique(alpha)/2)/k),unique(df)))^2
  return(mse)
}
Sidak_MSE<- function(msd,rep,alpha,df,n_factor){
  k<-choose(unique(n_factor),2)
  mse<-0.5*unique(rep)*(msd/qt(1-(1-(unique(alpha))/2)^(1/k),unique(df)))^2
  return(mse)
}
Scheffe_MSE<- function(msd,rep,alpha,df,n_factor){
  mse<-(unique(rep)*msd^2)/(2*(unique(n_factor)-1)*qf(1-unique(alpha),unique(n_factor)-1,unique(df)))
  return(mse)
}

# ---------------------------------------------------------------

Fisher_MSE_ab<- function(msd,rep,alpha,df,n_other_factor){
  mse<-0.5*unique(rep)*unique(n_other_factor)*(msd/qt(1-alpha/2,df))^2
  return(mse)
}
Tukey_MSE_ab<- function(msd,rep,alpha,df,n_factor,n_other_factor){
  mse<-unique(n_other_factor)*unique(rep)*(msd/qtukey(1-unique(alpha),unique(n_factor),unique(df)))^2
  return(mse)
}
Bonferroni_MSE_ab<- function(msd,rep,alpha,df,n_factor,n_other_factor){
  k<-choose(unique(n_factor),2)
  mse<-0.5*unique(n_other_factor)*unique(rep)*(msd/qt(1-((unique(alpha)/2)/k),unique(df)))^2
  return(mse)
}
Sidak_MSE_ab<- function(msd,rep,alpha,df,n_factor,n_other_factor){
  k<-choose(unique(n_factor),2)
  mse<-0.5*unique(n_other_factor)*unique(rep)*(msd/qt(1-(1-(unique(alpha))/2)^(1/k),unique(df)))^2
  return(mse)
}
Scheffe_MSE_ab<- function(msd,rep,alpha,df,n_factor,n_other_factor){
  mse<-(unique(rep)*unique(n_other_factor)*msd^2)/(2*(unique(n_factor)-1)*qf(1-unique(alpha),unique(n_factor)-1,unique(df)))
  return(mse)
}

# ---------------------------------------------------------------

# Experimental designs
exp_des <- list("Completely randomized design (CRD)",
                "Randomized complete block design (RCBD)",
                "Latin square",
                "Two-way complete factorial as a CRD",
                "Two-way complete factorial as a RCBD",
                "Split-plot as a CRD",
                "Split-plot as a RCBD")

separators <- list("Fisher's LSD",
                   "Tukey's HSD",
                   "Bonferroni correction for multiple comparison",
                   "Sidak correction for multiple comparison",
                   "Scheffe's")



# Example files
rcbd_fisher_raw="https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/rcbd_fisher.csv"
latin_fisher_raw="https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/latin_fisher.csv"
factorial_rcbd_a_fisher_raw="https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/factorial_rcbd_a_fisher.csv"
factorial_rcbd_ab_scheffe_raw="https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/factorial_rcbd_ab_scheffe.csv"
sp_rcbd_a_sidak_raw="https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/sp_rcbd_a_sidak.csv"
sp_rcbd_b_bonferroni_raw="https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/sp_rcbd_b_bonferroni.csv"
sp_rcbd_b_within_a_tukey_raw="https://raw.githubusercontent.com/vcgarnica/MSE_FindR/main/Example%20files/sp_rcbd_b_within_a_tukey.csv"


# Tab Content =========================================================================

# Framework tab ----------------------------------------------

disclosure_tab <- tabItem(
  tabName = "Disclosure",
  img(src = "logo.png", height =80, width = 290,style="display: block; margin-left: auto; margin-right: auto;"),
  br(),
  p("MSE FindR is a R Shiny application developed to help researchers obtain estimates of the mean square error (MSE) from balanced studies reporting treatment means and post hoc letter results."),
  p("Developed collaboratively by plant pathologists from North Carolina State University, Pennsylvania State University, and Kansas State University, MSE FindR supports a variety of experimental designs and post hoc tests,
    including Fisher’s LSD, Tukey’s HSD, Scheffé's test, Bonferroni, and Šidák corrections for multiple comparisons."),
  br(),
  h4("Tutorial"),
    p("A comprehensive evaluation of the tool and a walk-through tutorial
      on how to use MSE FindR can be found", tags$a(href="https://github.com/vcgarnica/MSE_FindR","here.")),
  br(),
  h4("Example files"),
  p(downloadLink("latin_fisher", "Latin Square with Fisher's LSD test"), br(),
    downloadLink("rcbd_fisher", "RCBD with Fisher's LSD test"), br(),
    downloadLink("factorial_rcbd_a_fisher", "2-way factorial as a RCBD with Fisher's LSD (A present, B ommitted)"), br(),
    downloadLink("factorial_rcbd_ab_scheffe", "2-way factorial as a RCBD with Scheffe test (A x B present)"), br(),
    downloadLink("sp_rcbd_a_sidak", "Split-plot as a RCBD with Sidak correction (A mainplot present, B subplot ommitted)"), br(),
    downloadLink("sp_rcbd_b_bonferroni", "Split-plot as a RCBD with Bonferroni correction (A mainplot omitted, B subplot present)"), br(),
    downloadLink("sp_rcbd_b_within_a_tukey", "Split-plot as a RCBD with Tukey HSD (B subplot within A mainplot present)")),
  br(),
  h4("Citation"),
  p("Garnica, V.C., Shah, D.A., Esker, P., Ojiambo, P.S. (2022) Got Fisher's LSD or Tukey's HSD?: a R Shiny app tool for recovering variance in designed experiments when only mean and post-hoc tests are reported. APS Meeting, 6-10 August 2022."),
  br(),
  h4("Credits"),
  p("Vinicius C. Garnica (garnica.vinicius@gmail.com), Denis A. Shah (denisshah2331@gmail.com), Paul D. Esker (pde6@psu.edu), and Peter S. Ojiambo (pojiamb@ncsu.edu)."),
  br(),
  h4("Disclaimer"),
  p("MSE FindR was developed to help researchers from multiple disciplines estimate the MSE from balanced experiments supported by the platform. 
    We welcome feedback and suggestions about the usefulness of the application and make no guarantee of the correctness, reliability, or utility 
    of the results if incorrect selections are made during the steps of MSE estimation. MSE FindR is freely accessible, and the source code is hosted at https://github.com/vcgarnica/MSE-FindR.")
  )
  



# Upload file tab ----------------------------------------------

upload_tab <-     tabItem(tabName = "FileUpload",
                          fluidPage(
                            h4("Upload file"),
                            sidebarPanel(
                              fileInput('file1', 'Choose file to upload:',
                                        accept = c('text/csv',
                                                   'text/comma-separated-values',
                                                   'text/tab-separated-values',
                                                   'text/plain','.csv','.tsv')),
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
                                           selected = '"')),
                            mainPanel(
                              column(width = 12,
                              DT::dataTableOutput('contents'))
                            )
                          )
)


# Splitter tab ----------------------------------------------

splitter_tab <- tabItem(
  tabName = "Splitter",
  fluidPage(
    h4("Means and letters"),
    sidebarPanel(selectInput("get_let_mean",'Select column to be separated:',choices = NULL),
          br(),
          br(),
          actionButton("splitter", "Separate")),
  mainPanel(
    column(width = 12,DT::dataTableOutput('contents1'))
  )
  )
)


# Estimator tab --------------------------------------------------------------------
estimator_tab <-  tabItem(tabName = "Estimator",
                          fluidPage(
                            fluidRow(
                              box(title = "Design", width = 6, solidHeader = T,
                                  fluidRow(
                                    column(8, selectInput("exp_design",'Experimental design',choices = exp_des),
                                           selectInput("mean_sep","Choose post hoc test",choices = separators),
                                           sliderInput('alpha',"Significance level ",0.05, min = 0.01, max = 0.10))),
                                  br(),
                                  br(),
                                  actionButton("go_button", "Estimate"),
                                  br(),
                                  br(),
                                  br(),
                                  uiOutput("downloadData")),
                              conditionalPanel(
                                condition = "input.exp_design == 'Completely randomized design (CRD)' || input.exp_design == 'Randomized complete block design (RCBD)'",
                                box(title = "Select columns", width = 5, solidHeader = T,
                                    column(8, selectInput("trial_id", "Unique trial identifier number", NULL),
                                           selectInput("factor_A", "Factor A", NULL),
                                           selectInput("replicates", "Number of replicates or blocks", NULL),
                                           selectInput("means", "Means", NULL),
                                           selectInput("letters", "Post hoc test letters", NULL)))),
                              conditionalPanel(
                                condition = "input.exp_design == 'Latin square'",
                                box(title = "Specify Column", width = 5, solidHeader = T,
                                    column(8, selectInput("trial_id_ls", "Trial Identifier Number", NULL),
                                           selectInput("factor_A_ls", "Factor A", NULL),
                                           selectInput("means_ls", "Means", NULL),
                                           selectInput("letters_ls", "Post hoc test letters", NULL)))),
                              conditionalPanel(
                                condition = "input.exp_design == 'Two-way complete factorial as a CRD' || input.exp_design == 'Two-way complete factorial as a RCBD' ",
                                box(title = "Specify Column", width = 5, solidHeader = T, 
                                    column(8, selectInput("trial_id_ab", "Unique trial identifier number", NULL),
                                           selectInput("variation_ab", "Source of variation", choices = c("A","A x B")),
                                           conditionalPanel( condition = "input.variation_ab == 'A'",
                                                             fluidRow(column(8,selectInput("factor_A_aa", "Factor A", NULL))),
                                                             fluidRow(column(8,selectInput("factor_B_aa", "Number of levels factor B", NULL)))),
                                           conditionalPanel( condition = "input.variation_ab == 'A x B'", 
                                                             fluidRow(column(8,selectInput("factor_A_ab", "Factor A", NULL))),
                                                             fluidRow(column(8,selectInput("factor_B_ab", "Factor B", NULL)))),
                                           selectInput("replicates_ab", "Number of replicates", NULL),
                                           selectInput("means_ab", "Means", NULL),
                                           selectInput("letters_ab", "Post hoc test letters", NULL)))),
                              conditionalPanel(
                                condition = "input.exp_design == 'Split-plot as a CRD'  || input.exp_design == 'Split-plot as a RCBD'",
                                box(title = "Specify Column", width = 6, solidHeader = T, 
                                    column(8, selectInput("trial_id_sp", "Unique trial identifier number", NULL),
                                           selectInput("variation_sp", "Source of variation", choices = c("A","A within B")),
                                           conditionalPanel( condition = "input.variation_sp == 'A'", 
                                                             fluidRow(column(8,selectInput("factor_A_sp", "Factor A", NULL)),radioButtons("radio_sp", "Level",list("Main plot","Sub plot"), inline = TRUE, selected = "Main plot")),
                                                             fluidRow(column(8,selectInput("factor_B_sp", "Number of levels factor B", NULL)))),
                                           conditionalPanel( condition = "input.variation_sp == 'A within B'", 
                                                             fluidRow(column(8,selectInput("factor_A_sp_ab", "Factor A (Sub plot Level)", NULL))),
                                                             fluidRow(column(8,selectInput("factor_B_sp_ab", "Factor B (Main plot Level)", NULL)))),
                                           selectInput("replicates_sp", "Number of replicates", NULL),
                                           selectInput("means_sp", "Means", NULL),
                                           selectInput("letters_sp", "Post hoc test letters", NULL))))),
                            br(),
                            column(12,
                                   DT::dataTableOutput('contents2')
                            )
                          )
)



# SideBar content =========================================================================

sideBar_content <- dashboardSidebar(
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  sidebarMenu(
    menuItem("Disclosure", tabName = "Disclosure"),
    menuItem("Upload file", tabName = "FileUpload"),
    menuItem("Separate means and letters", tabName = "Splitter"),
    menuItem("Estimator", tabName = "Estimator")
  )
)

# BODY content ------------------------------------------------------------------------------

body_content <- dashboardBody(
  tabItems(
    disclosure_tab,
    splitter_tab,
    upload_tab,
    estimator_tab
  )
)

# UI =========================================================================

ui <-  dashboardPage(
  
  dashboardHeader(title = "MSE FindR"),
  ## Sidebar content
  sideBar_content,
  ## Body content
  body_content
)

# Server =========================================================================
server <- function(input, output,session) {
  
  rv = reactiveValues(data= NULL)
  
  sheet<-reactive({
    if(is.null(input$file1)){
      return(NULL)}
    inFile <- input$file1
    tbl <- read.csv(inFile$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote,
                    stringsAsFactors = FALSE)
    rv$data <- tbl
    tbl
  }) 
  
  output$contents <- DT::renderDataTable({
    DT::datatable(rv$data, options = list(searching = FALSE, scrollX = T))
  })
  
  
  observe({
    value <- names(sheet())
    updateSelectInput(session,"get_let_mean", choices = value)
  })
  
  
  observeEvent(input$splitter, {
    rv$data<-rv$data %>% mutate(clean_means=get_values(.data[[input$get_let_mean]]),
                                clean_letters=get_letters(.data[[input$get_let_mean]])) 
  })
  
  output$contents1 <- DT::renderDataTable({
    DT::datatable(rv$data, options = list(searching = FALSE, scrollX = T))
  })
  
  
  observe({
    value <- names(rv$data)
    updateSelectInput(session,"trial_id", choices = value)
    updateSelectInput(session,"replicates", choices =value)
    updateSelectInput(session,"factor_A", choices = value)
    updateSelectInput(session,"means", choices = value)
    updateSelectInput(session,"letters", choices = value)
  })
  
  observe({
    value <- names(rv$data)
    updateSelectInput(session,"trial_id_ls", choices = value)
    updateSelectInput(session,"factor_A_ls", choices =value)
    updateSelectInput(session,"means_ls", choices = value)
    updateSelectInput(session,"letters_ls", choices = value)
  })
  
  observe({
    value <- names(rv$data)
    updateSelectInput(session,"trial_id_ab", choices = value)
    updateSelectInput(session,"replicates_ab", choices =value)
    updateSelectInput(session,"factor_A_aa", choices = value)
    updateSelectInput(session,"factor_B_aa", choices = value)
    updateSelectInput(session,"factor_A_ab", choices = value)
    updateSelectInput(session,"factor_B_ab", choices = value)
    updateSelectInput(session,"means_ab", choices = value)
    updateSelectInput(session,"letters_ab", choices = value)
  })
  
  observe({
    value <- names(rv$data)
    updateSelectInput(session,"trial_id_sp", choices = value)
    updateSelectInput(session,"replicates_sp", choices =value)
    updateSelectInput(session,"factor_A_sp", choices = value)
    updateSelectInput(session,"factor_B_sp", choices = value)
    updateSelectInput(session,"factor_A_sp_ab", choices = value)
    updateSelectInput(session,"factor_B_sp_ab", choices = value)
    updateSelectInput(session,"means_sp", choices = value)
    updateSelectInput(session,"letters_sp", choices = value)
  })

  
  data_filtered<- reactive({
    req(rv$data)
    validate(need(input$file1 != "", "Data set must be uploaded"))
    
    # CDR, RCBD ---------------------------------------------------------------------------------------------------------
    if(input$exp_design == "Completely randomized design (CRD)" || input$exp_design == "Randomized complete block design (RCBD)") {
      dt<- rv$data %>% dplyr::group_by(.data[[input$trial_id]]) %>% dplyr::mutate(n_factor_A = length(unique(.data[[input$factor_A]])),
                                                                                 n_replicates=(.data[[input$replicates]]))
      
      if(input$exp_design == "Completely randomized design (CRD)") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id]]) %>% dplyr::mutate(df_error = n_factor_A*(n_replicates-1))}
      if(input$exp_design == "Randomized complete block design (RCBD)") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id]]) %>% dplyr::mutate(df_error = (n_factor_A-1)*(n_replicates-1))}
    }
    # Latin ---------------------------------------------------------------------------------------------------------   
      if(input$exp_design == "Latin square") {
        dt<- rv$data %>% dplyr::group_by(.data[[input$trial_id_ls]]) %>% dplyr::mutate(n_factor_A = length(unique(.data[[input$factor_A_ls]])),
                                                                                      n_replicates= length(unique(.data[[input$factor_A_ls]])),
                                                                                      df_error = (n_factor_A-1)*(n_factor_A-2))
    }
    
    
    # Factorials ---------------------------------------------------------------------------------------------------------    
    if(input$exp_design == "Two-way complete factorial as a CRD" || input$exp_design == "Two-way complete factorial as a RCBD"){
    if(input$variation_ab=="A"){
      dt<- rv$data %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% dplyr::mutate(n_factor_A = length(unique(.data[[input$factor_A_aa]])),
                                                                                    n_factor_B = (.data[[input$factor_B_aa]]),
                                                                                    n_replicates=(.data[[input$replicates_ab]]))}
      
      
    if(input$variation_ab=="A x B"){
      dt<- rv$data %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% dplyr::mutate(n_factor_A = length(unique(.data[[input$factor_A_ab]])),
                                                                                    n_factor_B = length(unique(.data[[input$factor_B_ab]])),
                                                                                    n_replicates=(.data[[input$replicates_ab]]))}   
      if(input$exp_design == "Two-way complete factorial as a CRD") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% dplyr::mutate(df_error = n_factor_A*n_factor_B*(n_replicates-1))}
      if(input$exp_design == "Two-way complete factorial as a RCBD") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% dplyr::mutate(df_error = (n_factor_A*n_factor_B-1)*(n_replicates-1))}
      
    }
    
    # Split Plot ---------------------------------------------------------------------------------------------------------    
    if(input$exp_design == "Split-plot as a CRD" || input$exp_design == "Split-plot as a RCBD"){
      if(input$variation_sp=="A"){
        dt<- rv$data %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% dplyr::mutate(n_factor_A = length(unique(.data[[input$factor_A_sp]])),
                                                                                      n_factor_B = (.data[[input$factor_B_sp]]),
                                                                                      n_replicates=(.data[[input$replicates_sp]]))}
      
      if(input$variation_sp=="A within B"){
        dt<- rv$data %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% dplyr::mutate(n_factor_A = length(unique(.data[[input$factor_A_sp_ab]])),
                                                                                      n_factor_B = length(unique(.data[[input$factor_B_sp_ab]])),
                                                                                      n_replicates=(.data[[input$replicates_sp]]))}   

      if(input$variation_sp=="A"){
        if(input$radio_sp=="Main plot") {      
        if(input$exp_design == "Split-plot as a CRD") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% dplyr::mutate(df_error = n_factor_A*(n_replicates-1))}
        if(input$exp_design == "Split-plot as a RCBD") { 
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% dplyr::mutate(df_error = (n_factor_A-1)*(n_replicates-1))}         
      }
        if(input$radio_sp=="Sub plot") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% dplyr::mutate(df_error = n_factor_B*(n_replicates-1)*(n_factor_A-1))}  
      }
      
      
      if(input$variation_sp=="A within B"){
       dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% dplyr::mutate(df_error = n_factor_A*(n_replicates-1)*(n_factor_B-1))
       }   
       
    }

    return(dt)
    
  })

  addData <- eventReactive(input$go_button, {
    # CDR and RCBD ---------------------------------------------------------------------------------------------------------
    if(input$exp_design == "Completely randomized design (CRD)" || input$exp_design == "Randomized complete block design (RCBD)"){
      dt<-data_filtered()  %>% dplyr::group_by(.data[[input$trial_id]]) %>% mutate(msd=mean(get_MSD(.data[[input$means]],.data[[input$letters]])$n))
      
      if(input$mean_sep == "Fisher's LSD") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id]]) %>% 
          dplyr::mutate(MSE = Fisher_MSE(msd,n_replicates,input$alpha,df_error)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates) }
      if(input$mean_sep == "Tukey's HSD") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id]]) %>% 
          dplyr::mutate(MSE = Tukey_MSE(msd,n_replicates,input$alpha,df_error,n_factor_A)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates) }
      if(input$mean_sep == "Bonferroni correction for multiple comparison") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id]]) %>% 
          dplyr::mutate(MSE = Bonferroni_MSE(msd,n_replicates,input$alpha,df_error,n_factor_A)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates)}
      if(input$mean_sep == "Sidak correction for multiple comparison") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id]]) %>% 
          dplyr::mutate(MSE = Sidak_MSE(msd,n_replicates,input$alpha,df_error,n_factor_A)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates)}       
      if(input$mean_sep == "Scheffe's") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id]]) %>% 
          dplyr::mutate(MSE = Scheffe_MSE(msd,n_replicates,input$alpha,df_error,n_factor_A)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates)}      
    }
    
    # Latin ---------------------------------------------------------------------------------------------------------   
    if(input$exp_design == "Latin square"){
      dt<-data_filtered()  %>% dplyr::group_by(.data[[input$trial_id_ls]]) %>% mutate(msd=mean(get_MSD(.data[[input$means_ls]],.data[[input$letters_ls]])$n))
      
      if(input$mean_sep == "Fisher's LSD") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ls]]) %>% 
          dplyr::mutate(MSE = Fisher_MSE(msd,n_replicates,input$alpha,df_error)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates)}
      if(input$mean_sep == "Tukey's HSD") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ls]]) %>% 
          dplyr::mutate(MSE = Tukey_MSE(msd,n_replicates,input$alpha,df_error,n_factor_A)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates)}
      if(input$mean_sep == "Bonferroni correction for multiple comparison") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ls]]) %>% 
          dplyr::mutate(MSE = Bonferroni_MSE(msd,n_replicates,input$alpha,df_error,n_factor_A)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates)}
      if(input$mean_sep == "Sidak correction for multiple comparison") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ls]]) %>% 
          dplyr::mutate(MSE = Sidak_MSE(msd,n_replicates,input$alpha,df_error,n_factor_A)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates) }       
      if(input$mean_sep == "Scheffe's") {
        dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ls]]) %>% 
          dplyr::mutate(MSE = Scheffe_MSE(msd,n_replicates,input$alpha,df_error,n_factor_A)) %>% dplyr::select(-msd,-n_factor_A,-n_replicates)}      
    }
    
    # Factorials ---------------------------------------------------------------------------------------------------------    
    if(input$exp_design == "Two-way complete factorial as a CRD" || input$exp_design == "Two-way complete factorial as a RCBD"){
      dt<-data_filtered()  %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% mutate(msd=mean(get_MSD(.data[[input$means_ab]],.data[[input$letters_ab]])$n))
      
      
      if(input$variation_ab=="A"){
        # A --------------------------------------------------------------------------------------------------------         
        if(input$mean_sep == "Fisher's LSD") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE =  Fisher_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Tukey's HSD") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE = Tukey_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_A,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Bonferroni correction for multiple comparison") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE = Bonferroni_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_A,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Sidak correction for multiple comparison") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE = Sidak_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_A,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}       
        if(input$mean_sep == "Scheffe's") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE = Scheffe_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_A,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}      
        
      }
      
      if(input$variation_ab=="A x B"){      
        # A x B --------------------------------------------------------------------------------------------------------
        if(input$mean_sep == "Fisher's LSD") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE =  Fisher_MSE(msd,n_replicates,input$alpha,df_error)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Tukey's HSD") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE = Tukey_MSE(msd,n_replicates,input$alpha,df_error,(n_factor_A*n_factor_B))) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Bonferroni correction for multiple comparison") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE = Bonferroni_MSE(msd,n_replicates,input$alpha,df_error,(n_factor_A*n_factor_B))) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Sidak correction for multiple comparison") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE = Sidak_MSE(msd,n_replicates,input$alpha,df_error,(n_factor_A*n_factor_B))) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}       
        if(input$mean_sep == "Scheffe's") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_ab]]) %>% 
            dplyr::mutate(MSE = Scheffe_MSE(msd,n_replicates,input$alpha,df_error,(n_factor_A*n_factor_B))) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        
      }
    }  
    
    # Split Plot ---------------------------------------------------------------------------------------------------------    
    if(input$exp_design == "Split-plot as a CRD" || input$exp_design == "Split-plot as a RCBD"){
      
      # A Main plot --------------------------------------------------------------------------------------------------------      
      if(input$variation_sp=="A"){
      dt<-data_filtered()  %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% mutate(msd=mean(get_MSD(.data[[input$means_sp]],.data[[input$letters_sp]])$n))
     
        if(input$mean_sep == "Fisher's LSD") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE =  Fisher_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Tukey's HSD") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE = Tukey_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_A,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Bonferroni correction for multiple comparison") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE = Bonferroni_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_A,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Sidak correction for multiple comparison") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE = Sidak_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_A,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}       
        if(input$mean_sep == "Scheffe's") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE = Scheffe_MSE_ab(msd,n_replicates,input$alpha,df_error,n_factor_A,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}      
      }
      

      # A within B  --------------------------------------------------------------------------------------------------------      
      if(input$variation_sp=="A within B"){ 
      dt<-data_filtered()  %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>%
        mutate(msd=mean(get_MSD_SP(.data[[input$means_sp]],.data[[input$letters_sp]],.data[[input$factor_A_sp_ab]])$n))
      

        if(input$mean_sep == "Fisher's LSD") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE =  Fisher_MSE(msd,n_replicates,input$alpha,df_error)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Tukey's HSD") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE = Tukey_MSE(msd,n_replicates,input$alpha,df_error,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Bonferroni correction for multiple comparison") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE = Bonferroni_MSE(msd,n_replicates,input$alpha,df_error,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        if(input$mean_sep == "Sidak correction for multiple comparison") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE = Sidak_MSE(msd,n_replicates,input$alpha,df_error,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}       
        if(input$mean_sep == "Scheffe's") {
          dt<- dt %>% dplyr::group_by(.data[[input$trial_id_sp]]) %>% 
            dplyr::mutate(MSE = Scheffe_MSE(msd,n_replicates,input$alpha,df_error,n_factor_B)) %>% dplyr::select(-msd,-n_factor_A,-n_factor_B,-n_replicates)}
        
      }
    }
    
    return(dt)
    
  })
  
  output$contents2 <- DT::renderDataTable({
       DT::datatable(addData(), options = list(searching = FALSE, 
                                            pageLength = 20,
                                            scrollX = T))
  })

  
  output$downloadData01 <- downloadHandler(
    filename = function() {paste("result_SDFinder.csv",sep="")},
    content = function(file) {write.csv(addData(), file)}
  )
  
  
  output$downloadData <- renderUI({
    req(input$file1, addData())
    downloadButton("downloadData01")
  })
  
  # Download example files
  output$latin_fisher <- downloadHandler(
    filename = function() {
      paste("latin_fisher.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(latin_fisher_raw)), file)
    }
  )  

  output$rcbd_fisher <- downloadHandler(
    filename = function() {
      paste("rcbd_fisher.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(rcbd_fisher_raw)), file)
    }
  )
  
  output$factorial_rcbd_a_fisher <- downloadHandler(
    filename = function() {
      paste("factorial_rcbd_a_fisher.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(factorial_rcbd_a_fisher_raw)), file)
    }
  )
  
  output$factorial_rcbd_ab_scheffe <- downloadHandler(
    filename = function() {
      paste("factorial_rcbd_ab_scheffe.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(factorial_rcbd_ab_scheffe_raw)), file)
    }
  )
  output$sp_rcbd_a_sidak <- downloadHandler(
    filename = function() {
      paste("sp_rcbd_a_sidak.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(sp_rcbd_a_sidak_raw)), file)
    }
  )
  
  output$sp_rcbd_b_bonferroni <- downloadHandler(
    filename = function() {
      paste("sp_rcbd_b_bonferroni.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(sp_rcbd_b_bonferroni_raw)), file)
    }
  )
  output$sp_rcbd_b_within_a_tukey <- downloadHandler(
    filename = function() {
      paste("sp_rcbd_b_within_a_tukey.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(sp_rcbd_b_within_a_tukey_raw)), file)
    }
  )


}



# Run shiny app ---------------------------------------------------------------------------
shinyApp(ui, server)
