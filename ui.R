
library(shiny)
library(unmarked)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("N-mixture model; Royal (2004)"),
  
  # sidebarLayout(position="left",
  # 
  # # Side panel for inputs ---
  # sidebarPanel(
  #     fileInput('fish_upload', 'Choose catch file to upload',
  #               accept = c(
  #                 'text/csv',
  #                 'text/comma-separated-values',
  #                 '.csv'
  #               )),
  #     radioButtons("fish_separator","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
  #     
  #   
  #     fileInput('covariate_upload', 'Choose covariate file to upload. Leave blank if no covariates',
  #               accept = c(
  #                 'text/csv',
  #                 'text/comma-separated-values',
  #                 '.csv'
  #               )),
  #     radioButtons("covar_separator","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
  # 
  #     uiOutput('abu_covars'),
  #     
  #     uiOutput('detect_covars')
  # )
  # ,
  # 
  # # Main panel for displaying outputs ----
  # mainPanel(
    
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Load Data",       
                         fluidRow(column(4,
                           fileInput('fish_upload', 'Choose catch file to upload',
                                                      accept = c(
                                                        'text/csv',
                                                        'text/comma-separated-values',
                                                        '.csv')),
                         radioButtons("fish_separator","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
                         fileInput('covariate_upload', 'Choose covariate file to upload. Leave blank if no covariates',
                                   accept = c(
                                     'text/csv',
                                     'text/comma-separated-values',
                                     '.csv')),
                         radioButtons("covar_separator","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
                         #uiOutput('abu_covars'),
                         #uiOutput('detect_covars')
                         selectInput('abu_covars',"Select abundance predictor(s)",choices=c(),multiple=TRUE),
                         selectInput('detect_covars',"Select detection predictor(s)",choices=c(),multiple=TRUE)
                          ),
                         column(8,
                          "Catch figure",
                          plotOutput("fishplot"),
                          "Covariate Summary",
                          verbatimTextOutput("covarsummar")
                                )
                         )),
                tabPanel("Fish Data", DT::dataTableOutput("sample_table")),
                tabPanel("Covariate Data", DT::dataTableOutput("covar_table")),
                tabPanel("Input Summary", verbatimTextOutput("InputSummary")),
                tabPanel("Model Summary", verbatimTextOutput("results")),
                # tabPanel("Abundance estimates", verbatimTextOutput("abundancepredictions")),
                # tabPanel("Detection estimates", verbatimTextOutput("Detectionpredictions"))#,
                tabPanel("Combined estimates",
                         fluidRow(
                           column(12,verbatimTextOutput("abundancepredictions")),
                           column(12,verbatimTextOutput("Detectionpredictions"))
                         )),
                tabPanel("Predictions",
                         "In development",
                         selectInput('abu_covars_pred',"Select abundance predictor",choices=c(),multiple=FALSE),
                         plotOutput("covarPlot"))#,
                # tabPanel("Report",
                #          HTML('<br><h3>Click the button below to generate a report.</h3><br>'),
                #          downloadButton("report", "Generate report"))
    )
  )
  
#) 
# )
