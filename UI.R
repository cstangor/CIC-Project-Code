#http://shiny.rstudio.com/articles/validation.html
#Shiny related questions to the shiny-discuss list
#shinyapps-users list is meant to be for the .io service
#devtools::install:+github("shiny", "rstudio")

# Raise the default file size limit to 2000MG
options(shiny.maxRequestSize= -1)
source('startup.R')
source('CIC_startup.R', local=FALSE)
source("grade_penalty.R", local=FALSE)
source("course_persistence.R", local=FALSE)
source("degree_persistence.R", local=FALSE)
source("degree_persistence_flowchart.R", local=FALSE)
#library(sqldf)
#source("http://sqldf.googlecode.com/svn/trunk/R/sqldf.R")

# files course_grid.csv and STEM_CIP.csv are necessary

# Echo all output and messages to the log
#sink(file = "CIC.log", append = FALSE, type = c("output", "message"), split = TRUE)

fluidPage(
  fluidRow(
    downloadButton('downloadPlot_gp', 'Grade Penalty Analysis\n(be patient!)'),
    downloadButton('downloadPlot_dp', 'Degree Persistence Analysis'),
    downloadButton('downloadPlot_cp', 'Course Persistence Analysis'),
    
  #User-directed messages
    #downloadButton('downloadPlot_dp', 'Download Degree Persistence Results'),
    #downloadButton('downloadPlot_cp', 'Download Course Persistence Results'),
    tags$br(),    
    tags$header(id="textbox",  rows=3, cols = 240,
                "Welcome. This page will help you create the CIC LARA analyses. cstangor@gmail.com for help", 
                style="color:red", class="shiny-text-output")
  ),
  tags$br(),    
  fluidRow( #Row 1
    column (3,
            #helpText('This page will help you create the CIC analyses.  
            #         Check the CIC.log file for messages'),
            
            radioButtons('sep', 'Select a separator type:',
                         c(','=',', ';'=';', Tab='\t'), selected = NULL, inline = TRUE),
            
            #h1("-----------"),
            uiOutput('sr_checkbox'),
            
            fileInput('sr_file',  'Choose Student Record file:',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tab'
                  )
            ),
            
            #h1("-----------"),
            uiOutput('sc_checkbox'),
            
            fileInput('sc_file',  'Choose Student Course file:',
                      accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.tsv'
                      )
            ),
            
            
            uiOutput('variables1'),
            uiOutput('variables2'),
            
            
            #actionButton("Course_Persistence_Button","Create Course Persistence PDF"),
            #actionButton("Degree_Persistence_Button","Create Degree Persistence PDF")
            
            textInput("university","Please enter your university name"),
            textInput("select_on","On which variables do you want to select?")
            
            ),
    column(9,
           tabsetPanel(id = 'tabsetpanel1',
             tabPanel('Data', value = 'data_selected',
                      tableOutput("mytable1")),
             tabPanel('Summary', value = 'summary_selected',
                      tableOutput("mytable2")),
             tabPanel('Charts', value = 'charts_selected',
                      plotOutput("myPlot"))
           )
    )
  ) #fluidRow
  ) #fluidPage
