library(shiny)
library("stringr")
library(ggplot2)
library("ggrepel")
library(dplyr)
library(tidyr)
#library(plotly)
library(shinyFiles)
library(gridExtra)
library(DT)
appCSS <-
 "#color ~ .selectize-control.single .selectize-dropdown [data-value=X] { color: blue }
  #color ~ .selectize-control.single .selectize-dropdown [data-value=YES] { color: red }"
path_save<-"Please select a folder:"
# Define UI for random distribution application
fluidPage(#theme = "bootstrap.css",
  tags$head(tags$style(HTML(appCSS))),
  # Application title
  titlePanel("THEME"),

  sidebarLayout(
    sidebarPanel( ##"OVERALL PARAMETERS"
      conditionalPanel(condition="input.tabselected==1",
      tags$span(style="color:black",strong(("OVERALL PARAMETERS"))),
      tags$hr(style="border-color: grey;"),

      fileInput("filecal", "Calibration set",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),

      fileInput("fileval", "Validation set",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),

      actionLink("optionread","Read options",icon= icon("cog")),
      conditionalPanel("input.optionread%2==1",
      checkboxInput('header', 'Header', TRUE),
      selectInput("sep","Separator: ",
                  c('Comma'=',',
                    'Semicolon'=';',
                    'Tab'='\t'),selected='\t', width ="50%"),
      selectInput('dec', 'Decimal ',
                  c('comma'=",",
                    'dot'="."), selected='.', width ="50%")
      ),

      tags$hr(style="border-color: grey;"),

      selectInput("optCV","Cross-validation: ",
                  c(NA,1:20)),

      selectInput("optBW","Backward Selection: ",
                  c(NA,seq(0,1,by=.1)),selected="NO"),
      tags$hr(style="border-color: grey;"),
      tags$span(style="color:black",strong("Tuning parameters")),
      selectInput("opts","s: ",
                  seq(0,1,by=.1),selected=.5),

      selectInput("optl","l: ",
                  seq(1,10,by=1),selected=1),
      tags$hr(style="border-color: grey;"),
      
      
      shinyDirButton("outpufiles", "Choose a folder" ,
                     title = path_save,
                     buttonType = "default", class = NULL),
      tags$hr(style="border-color: grey;"),
      
      ##Action
      actionButton("goButton", "Go!",icon = icon("rocket"))
      #br()
      ),


    conditionalPanel(condition="input.tabselected==2",
      tags$span(style="color:black",strong(("PLOT OPTIONS"))),
      tags$hr(style="border-color: grey;"),
      uiOutput("Modelplot"),
      tags$hr(style="border-color: grey;"),
      selectInput("Labelprintplot","Label: ",c("No","Yes"),selected="No", width ="120px"),
      selectInput("mycex","Label size: ",seq(1,10,1),selected=6, width ="120px"),
      uiOutput("mycolor"),
      selectInput("mycexaxis","Axis label size: ",seq(1,5,.5),selected=2, width ="120px"),
      selectInput("mycextitle","Axis text size: ",seq(1,5,.5),selected=1, width ="120px"),
      tags$hr(style="border-color: grey;"),
      actionButton("PlotButton", "Refresh",width="120px",icon=icon("refresh"))
      #submitButton("Update View", icon("refresh"))
      ),


    conditionalPanel(condition="input.tabselected==4",
      tags$span(style="color:black",strong(("PLOT OPTIONS"))),
      tags$hr(style="border-color: grey;"),
      uiOutput("YEqRMSEC"),
      actionButton("PlotButton3", "Refresh",width="120px",icon=icon("refresh"))
      ),

    width=2),


    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(


      tabsetPanel(type = "tabs",
                  tabPanel("Data View",value=1,icon = icon("eye"),
                           fluidPage(
                           dataTableOutput("datausers"),
                           dataTableOutput("codeusers")
                           )
                  ),
                  tabPanel("Variable selection",value=1,icon = icon("cogs"),
                           fluidPage(
                             uiOutput("BlocksNB"),
                             uiOutput("SelectBlocksconfig"),
                             uiOutput("SelectBlock"),
                             uiOutput("VarSel")
                             )
                  ),
                  tabPanel("Model design",value=1,  h3(""), icon = icon("cogs"),
                           column(10,
                                  uiOutput("EquationsNB")
                                  ),
                           column(3,
                                  tags$div(class = "h5", id = NULL, NULL,strong("Number of components")),
                                  tags$div(class = "h5", id = NULL,NULL,tags$span(style="color:white",".")),
                                  uiOutput("SelectModDesComp")
                           ),
                           column(2,
                                  tags$div(class = "h5", id = NULL,NULL,strong("Roles of Thematic Blocks")),
                                  tags$div(class = "h5", id = NULL,NULL,strong("Eq. 1")),
                                  uiOutput("SelectModDes1")
                                  ),

                           column(2,
                                  tags$div(class = "h5", id = NULL,NULL,strong(uiOutput("textBlocksRolesEq2"))),
                                  tags$div(class = "h5", id = NULL,NULL,strong(uiOutput("textEq2"))),
                                  uiOutput("SelectModDes2")
                                  ),
                           column(2,
                                  tags$div(class = "h5", id = NULL,NULL,strong(uiOutput("textBlocksRolesEq3"))),
                                  tags$div(class = "h5", id = NULL,NULL,strong(uiOutput("textEq3"))),
                                  uiOutput("SelectModDes3"))

                  ),

                  tabPanel("Cross-validation",value=4,icon = icon("bar-chart-o"),
                           fluidPage(
                             fluidRow(
                               column(2,downloadButton("SavePlotButton3", "Save",width="120px"))
                             ),
                             fluidRow(
                               tags$hr(style="border-color: grey;"),
                               plotOutput("plotRMSE",height = "100%") #height="100%"
                             )
                           )
                      ),

                  tabPanel("Interpretation",value=2,icon = icon("bar-chart-o"),
                           fluidPage(
                            fluidRow(
                             column(2,uiOutput("blocktoplot")),
                             column(2,uiOutput("Xaxe")),
                             column(2,uiOutput("Yaxe"))
                             ),
                           fluidRow(
                             column(2,downloadButton("SavePlotButton", "Save",width="120px"))
                              ),
                           fluidRow(
                             tags$hr(style="border-color: grey;"),
                             plotOutput("plotind",height = "100%")
                             )
                           )

                        ),

                  tabPanel("Prediction",value=2,icon = icon("bar-chart-o"),
                           fluidPage(
                             fluidRow(
                               #column(3,uiOutput("ModelplotPred")),
                               column(3,uiOutput("Yblockeqplot")),
                               column(2,uiOutput("Yvarplot"))
                               ),
                             fluidRow(
                               #column(2,actionButton("PlotButton2", "Refresh",width="120px")),
                               column(2,downloadButton("SavePlotButton2", "Save",width="120px"))
                             ),
                             fluidRow(
                               tags$hr(style="border-color: grey;"),
                               plotOutput("plotY",height = "100%") #height="100%"
                               )
                           )
                  ),
          id = "tabselected")
  )
)
)


