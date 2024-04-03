# SHINY DASHBOARD - MSE FINDR


# Packages library =================================================

# load packages

library("shiny")
library("shinydashboard")
library("tidyverse")
library("data.table")
library("DT")
library("fresh")
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


# ---------------------------------------------------------------

Fisher_MSE<- function(msd,rep,alpha,df){
  mse<-round(0.5*unique(rep)*(msd/qt(1-unique(alpha)/2,unique(df)))^2,3)
  return(mse)}
Tukey_MSE<- function(msd,rep,alpha,df,n_factor){
  mse<-round(unique(rep)*(msd/qtukey(1-unique(alpha),unique(n_factor),unique(df)))^2,3)
  return(mse)
}
Bonferroni_MSE<- function(msd,rep,alpha,df,n_factor){
  k<-choose(unique(n_factor),2)
  mse<-round(0.5*unique(rep)*(msd/qt(1-((unique(alpha)/2)/k),unique(df)))^2,3)
  return(mse)
}
Sidak_MSE<- function(msd,rep,alpha,df,n_factor){
  k<-choose(unique(n_factor),2)
  mse<-round(0.5*unique(rep)*(msd/qt(1-(1-(unique(alpha))/2)^(1/k),unique(df)))^2,3)
  return(mse)
}
Scheffe_MSE<- function(msd,rep,alpha,df,n_factor){
  mse<-round((unique(rep)*msd^2)/(2*(unique(n_factor)-1)*qf(1-unique(alpha),unique(n_factor)-1,unique(df))),3)
  return(mse)
}

# ---------------------------------------------------------------

Fisher_MSE_ab<- function(msd,rep,alpha,df,n_other_factor){
  mse<-round(0.5*unique(rep)*unique(n_other_factor)*(msd/qt(1-alpha/2,df))^2,3)
  return(mse)
}
Tukey_MSE_ab<- function(msd,rep,alpha,df,n_factor,n_other_factor){
  mse<-round(unique(n_other_factor)*unique(rep)*(msd/qtukey(1-unique(alpha),unique(n_factor),unique(df)))^2,3)
  return(mse)
}
Bonferroni_MSE_ab<- function(msd,rep,alpha,df,n_factor,n_other_factor){
  k<-choose(unique(n_factor),2)
  mse<-round(0.5*unique(n_other_factor)*unique(rep)*(msd/qt(1-((unique(alpha)/2)/k),unique(df)))^2,3)
  return(mse)
}
Sidak_MSE_ab<- function(msd,rep,alpha,df,n_factor,n_other_factor){
  k<-choose(unique(n_factor),2)
  mse<-round(0.5*unique(n_other_factor)*unique(rep)*(msd/qt(1-(1-(unique(alpha))/2)^(1/k),unique(df)))^2,3)
  return(mse)
}
Scheffe_MSE_ab<- function(msd,rep,alpha,df,n_factor,n_other_factor){
  mse<-round((unique(rep)*unique(n_other_factor)*msd^2)/(2*(unique(n_factor)-1)*qf(1-unique(alpha),unique(n_factor)-1,unique(df))),3)
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
  img(src = "logo.png", height =150, width = 550,style="display: block; margin-left: auto; margin-right: auto;"),
  br(),
  p("MSE FINDR is a R Shiny application developed to help researchers obtain estimates of the residual variance (i.e., mean square error [MSE]) from balanced studies reporting treatment means and post hoc letter results."),
  p("Developed collaboratively by plant pathologists from North Carolina State University, Pennsylvania State University, and Kansas State University, MSE FINDR supports a variety of experimental designs and post hoc tests,
    including Fisher’s LSD, Tukey’s HSD, Scheffé's test, Bonferroni, and Šidák corrections for multiple comparisons."),
  br(),
  h4("Tutorial"),
  p("A comprehensive evaluation of the tool and a walk-through tutorial on how to use MSE FINDR can be found", tags$a(href="https://github.com/vcgarnica/MSE_FindR","here.")),
  br(),
  h4("Example files"),
  p(downloadLink("latin_fisher", "Latin Square with Fisher's LSD test"), br(),
    downloadLink("rcbd_fisher", "RCBD with Fisher's LSD test"), br(),
    downloadLink("factorial_rcbd_a_fisher", "2-way factorial as a RCBD with Fisher's LSD (A present, B ommitted)"), br(),
    downloadLink("factorial_rcbd_ab_scheffe", "2-way factorial as a RCBD with Scheffe test (A x B present)"), br(),
    downloadLink("sp_rcbd_a_sidak", "Split-plot as a RCBD with Sidak correction (A main-plot present, B sub-plot ommitted)"), br(),
    downloadLink("sp_rcbd_b_bonferroni", "Split-plot as a RCBD with Bonferroni correction (A main-plot omitted, B sub-plot present)"), br(),
    downloadLink("sp_rcbd_b_within_a_tukey", "Split-plot as a RCBD with Tukey HSD B (sub-plot) within A (main-plot)")),
  br(),
  h4("Citation"),
  p("Garnica, V. C., Shah, D. A., Esker, P. D., Ojiambo, P. S. (2024). MSE FINDR: A Shiny R Application to Estimate Mean Square Error Using Treatment Means and Post-hoc Test Results. Plant Disease. doi: 10.1094/PDIS-11-23-2519-SR."),
  br(),
  h4("Credits"),
  p("Vinicius Garnica (garnica.vinicius@gmail.com), Denis Shah (denisshah2331@gmail.com), Paul Esker (pde6@psu.edu), and Peter Ojiambo (pojiamb@ncsu.edu)."),
  br(),
  h4("Disclaimer"),
  p("MSE FINDR was developed to help researchers from multiple disciplines estimate the MSE from balanced experiments supported by the platform. 
    We welcome feedback and suggestions about the usefulness of the application and make no guarantee of the correctness, reliability, or utility 
    of the results if incorrect selections are made during the steps of MSE estimation. MSE FINDR is freely accessible, and the source code is hosted at https://github.com/vcgarnica/MSE-FindR."),
  br(),
  h4("License"),
  p("MIT License"),
  br(), 
  p("Copyright (c) 2022 Vinicius Garnica and others"),
  br(),
  p("Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:"),
  br(),
  p("The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software."),
  br(),
  p("THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.")
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
                                box(title = "Column assignment", width = 5, solidHeader = T,
                                    column(8, selectInput("trial_id", label = tags$span("Trial identifier number",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                         style = "color:#0072B2;",
                                                                                                                         title = "Each trial in designated folder should be assigned a unique value – numerical values only")),NULL),
                                           selectInput("factor_A", label = tags$span("Factor A",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                       style = "color:#0072B2;",
                                                                                                       title = "Column for factor A treatments – either numerical or categorical")),NULL),
                                           selectInput("replicates",label = tags$span("Number of replicates or blocks",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                              style = "color:#0072B2;",
                                                                                                                              title = "Column for replicates or blocks. Only one value per trial - numerical")),NULL),
                                           selectInput("means", label = tags$span("Means",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                 style = "color:#0072B2;",
                                                                                                 title = "Column for treatment means. No post-hoc letters allowed - numerical")),NULL),
                                           selectInput("letters", label = tags$span("Post hoc test letters",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                   style = "color:#0072B2;",
                                                                                                                   title = "Column for post-hoc letters. No treatment means allowed - categorical")),NULL)))),
                              conditionalPanel(
                                condition = "input.exp_design == 'Latin square'",
                                box(title = "Column assignment", width = 5, solidHeader = T,
                                    column(8, selectInput("trial_id_ls", label = tags$span("Trial identifier number",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                            style = "color:#0072B2;",
                                                                                                                            title = "Each trial in designated folder should be assigned a unique value – numerical values only")),NULL),
                                           selectInput("factor_A_ls", label = tags$span("Factor A",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                          style = "color:#0072B2;",
                                                                                                          title = "Column for factor A treatments – either numerical or categorical")),NULL),
                                           selectInput("means_ls", label = tags$span("Means",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                    style = "color:#0072B2;",
                                                                                                    title = "Column for treatment means. No post-hoc letters allowed - numerical")),NULL),
                                           selectInput("letters_ls", label = tags$span("Post hoc test letters",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                      style = "color:#0072B2;",
                                                                                                                      title = "Column for post-hoc letters. No treatment means allowed - categorical")),NULL)))),
                              conditionalPanel(
                                condition = "input.exp_design == 'Two-way complete factorial as a CRD' || input.exp_design == 'Two-way complete factorial as a RCBD' ",
                                box(title = "Column assignment", width = 5, solidHeader = T, 
                                    column(8, selectInput("trial_id_ab", label = tags$span("Trial identifier number",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                            style = "color:#0072B2;",
                                                                                                                            title = "Each trial in designated folder should be assigned a unique value – numerical values only")),NULL),
                                           selectInput("variation_ab", label = tags$span("Source of variation",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                      style = "color:#0072B2;",
                                                                                                                      title = "When interaction is not significant & comparisons were performed for the main effect (one factor omitted), select A or B (used interchangeably in factorial designs). When interaction is significant & comparisons were performed for the interaction, select A x B")),choices = c("A or B","A x B")),
                                           conditionalPanel( condition = "input.variation_ab == 'A or B'",
                                                             fluidRow(column(8,selectInput("factor_A_aa", label = tags$span("Factor A or B",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                                   style = "color:#0072B2;",
                                                                                                                                                   title = "Column for either factor A or factor B treatments, used interchangeably in factorial designs – either numerical or categorical")),NULL))),
                                                             fluidRow(column(8,selectInput("factor_B_aa", label = tags$span("Number of levels in omitted factor",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                                                        style = "color:#0072B2;",
                                                                                                                                                                        title = "If trial reports were carried out as a two-way factorial and only treatment means and post-hoc test for one factor are reported, users must indicate the number of levels of the omitted factor – numerical")),NULL)))),
                                           conditionalPanel( condition = "input.variation_ab == 'A x B'", 
                                                             fluidRow(column(8,selectInput("factor_A_ab", label = tags$span("Factor A",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                              style = "color:#0072B2;",
                                                                                                                                              title = "Column for factor A treatments – either numerical or categorical")),NULL))),
                                                             fluidRow(column(8,selectInput("factor_B_ab", label = tags$span("Factor B",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                              style = "color:#0072B2;",
                                                                                                                                              title = "Column for factor B treatments – either numerical or categorical")),NULL)))),
                                           selectInput("replicates_ab", label = tags$span("Number of replicates or blocks",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                  style = "color:#0072B2;",
                                                                                                                                  title = "Column for replicates or blocks. Only one value per trial - numerical")),NULL),
                                           selectInput("means_ab",label = tags$span("Means",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                   style = "color:#0072B2;",
                                                                                                   title = "Column for treatment means. No post-hoc letters allowed - numerical")),NULL),
                                           selectInput("letters_ab", label = tags$span("Post hoc test letters",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                      style = "color:#0072B2;",
                                                                                                                      title = "Column for post-hoc letters. No treatment means allowed - categorical")),NULL)))),
                              conditionalPanel(
                                condition = "input.exp_design == 'Split-plot as a CRD'  || input.exp_design == 'Split-plot as a RCBD'",
                                box(title = "Column assignment", width = 6, solidHeader = T, 
                                    column(8, selectInput("trial_id_sp", label = tags$span("Trial identifier number",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                            style = "color:#0072B2;",
                                                                                                                            title = "Each trial in designated folder should be assigned a unique value – numerical values only")),NULL),
                                           selectInput("variation_sp", label = tags$span("Source of variation",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                      style = "color:#0072B2;",
                                                                                                                      title = "When interaction is not significant & comparisons were performed for the main effect (one factor omitted) & were assinged to main-plot units, select A (main-plot). Similarly, when interaction is not significant & comparisons were performed for the main effect (one factor omitted) & were assinged to sub-plot units, select B (sub-plot). When interaction is significant & trials report treatment means and post-hoc test results for the interaction & comparisons were performed among main-plot levels within common subplot levels, select B (sub-plot) within A (main-plot)")),choices = c("A (main-plot)","B (sub-plot)","B (sub-plot) within A (main-plot)")),
                                           conditionalPanel( condition = "input.variation_sp == 'A (main-plot)'", 
                                                             fluidRow(column(8,selectInput("factor_A_sp", label = tags$span("Factor A (main-plot)",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                                          style = "color:#0072B2;",
                                                                                                                                                          title = "Column for factor A treatments assigned to the main-plot units – either numerical or categorical")),NULL))),
                                                             fluidRow(column(8,selectInput("factor_B_omit_sp", label = tags$span("Number of levels in omitted factor",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                                                             style = "color:#0072B2;",
                                                                                                                                                                             title = "If trial reports were carried out as a two-way split-plot and only treatment means and post-hoc test for one factor are reported, users must indicate the number of levels of the omitted factor – numerical")),NULL)))),                                           
                                           conditionalPanel( condition = "input.variation_sp == 'B (sub-plot)'", 
                                                             fluidRow(column(8,selectInput("factor_A_omit_sp", label = tags$span("Number of levels in omitted factor",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                                                             style = "color:#0072B2;",
                                                                                                                                                                             title = "If trial reports were carried out as a two-way split-plot and only treatment means and post-hoc test for one factor are reported, users must indicate the number of levels of the omitted factor – numerical")),NULL))),
                                                             fluidRow(column(8,selectInput("factor_B_sp", label = tags$span("Factor B (sub-plot)",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                                         style = "color:#0072B2;",
                                                                                                                                                         title = "Column for factor B treatments assigned to the sub-plot units – either numerical or categorical")),NULL)))),                                       
                                           conditionalPanel( condition = "input.variation_sp == 'B (sub-plot) within A (main-plot)'", 
                                                             fluidRow(column(8,selectInput("factor_A_sp_ab", label = tags$span("Factor A (main-plot)",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                                             style = "color:#0072B2;",
                                                                                                                                                             title = "Column for factor A treatments assigned to the main-plot units – either numerical or categorical")),NULL))),
                                                             fluidRow(column(8,selectInput("factor_B_sp_ab", label = tags$span("Factor B (sub-plot)",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                                            style = "color:#0072B2;",
                                                                                                                                                            title = "Column for factor B treatments assigned to the sub-plot units – either numerical or categorical")),NULL)))),
                                           selectInput("replicates_sp", label = tags$span("Number of replicates or blocks",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                                  style = "color:#0072B2;",
                                                                                                                                  title = "Column for replicates or blocks. Only one value per trial - numerical")),NULL),
                                           selectInput("means_sp", label = tags$span("Means",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                    style = "color:#0072B2;",
                                                                                                    title = "Column for treatment means. No post-hoc letters allowed - numerical")),NULL),
                                           selectInput("letters_sp", label = tags$span("Post hoc test letters",tags$i(class = "glyphicon glyphicon-info-sign",
                                                                                                                      style = "color:#0072B2;",
                                                                                                                      title = "Column for post-hoc letters. No treatment means allowed - categorical")),NULL))))),
                            br(),
                            column(12,
                                   DT::dataTableOutput('contents2')
                            )
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
    menuItem("Disclosure", tabName = "Disclosure"),
    menuItem("Upload file", tabName = "FileUpload"),
    menuItem("Estimator", tabName = "Estimator")
  )
)

# BODY content ------------------------------------------------------------------------------

body_content <- dashboardBody(
  tabItems(
    disclosure_tab,
    upload_tab,
    estimator_tab
  )
)

# UI =========================================================================

ui <-  dashboardPage(
  
  dashboardHeader(title = "MSE FINDR"),
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
    updateSelectInput(session,"factor_B_omit_sp", choices = value)    
    updateSelectInput(session,"factor_B_sp", choices = value)
    updateSelectInput(session,"factor_A_omit_sp", choices = value)
    updateSelectInput(session,"factor_A_sp_ab", choices = value)
    updateSelectInput(session,"factor_B_sp_ab", choices = value)
    updateSelectInput(session,"means_sp", choices = value)
    updateSelectInput(session,"letters_sp", choices = value)
  })
  
  
  data_filtered <- reactive({
    req(rv$data)
    validate(need(input$file1 != "", "Data set must be uploaded"))
    
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
  
  
  
  
  addData <- eventReactive(input$go_button, {
    dt <- data_filtered()
    
    if (input$exp_design == "Completely randomized design (CRD)" || input$exp_design == "Randomized complete block design (RCBD)") {
      dt <- dt %>%
        dplyr::group_by(.data[[input$trial_id]]) %>%
        mutate(
          msd = mean(get_MSD(.data[[input$means]], .data[[input$letters]])$n),
          MSE = switch(input$mean_sep,
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
          MSE = switch(input$mean_sep,
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
            switch(input$mean_sep,
                   "Fisher's LSD" = Fisher_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B),
                   "Tukey's HSD" = Tukey_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
                   "Bonferroni correction for multiple comparison" = Bonferroni_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
                   "Sidak correction for multiple comparison" = Sidak_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
                   "Scheffe's" = Scheffe_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B)
            )
          } else {
            switch(input$mean_sep,
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
          MSE = switch(input$variation_sp,
                       "A (main-plot)" = switch(input$mean_sep,
                                                "Fisher's LSD" = Fisher_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B),
                                                "Tukey's HSD" = Tukey_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
                                                "Bonferroni correction for multiple comparison" = Bonferroni_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
                                                "Sidak correction for multiple comparison" = Sidak_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B),
                                                "Scheffe's" = Scheffe_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A, n_factor_B)
                       ),
                       "B (sub-plot)" = switch(input$mean_sep,
                                               "Fisher's LSD" = Fisher_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_A),
                                               "Tukey's HSD" = Tukey_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B, n_factor_A),
                                               "Bonferroni correction for multiple comparison" = Bonferroni_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B, n_factor_A),
                                               "Sidak correction for multiple comparison" = Sidak_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B, n_factor_A),
                                               "Scheffe's" = Scheffe_MSE_ab(msd, n_replicates, input$alpha, df_error, n_factor_B, n_factor_A)
                       ),
                       "B (sub-plot) within A (main-plot)" = switch(input$mean_sep,
                                                                    "Fisher's LSD" = Fisher_MSE(msd, n_replicates, input$alpha, df_error),
                                                                    "Tukey's HSD" = Tukey_MSE(msd, n_replicates, input$alpha, df_error, n_factor_B),
                                                                    "Bonferroni correction for multiple comparison" = Bonferroni_MSE(msd, n_replicates, input$alpha, df_error, n_factor_B),
                                                                    "Sidak correction for multiple comparison" = Sidak_MSE(msd, n_replicates, input$alpha, df_error, n_factor_B),
                                                                    "Scheffe's" = Scheffe_MSE(msd, n_replicates, input$alpha, df_error, n_factor_B)
                       )
          )
        ) %>%
        select(-msd, n_factor_A, n_factor_B, n_replicates)
    }
    
    return(dt)
  })
  
  
  output$contents2 <- DT::renderDataTable({
    DT::datatable(addData(), options = list(searching = FALSE, 
                                            pageLength = 20,
                                            scrollX = T))
  })
  
  
  output$downloadData01 <- downloadHandler(
    filename = function() {paste("result_MSEFindR.csv",sep="")},
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