# EPSScall
# Russ McRee, russ at holisticinfosec dot io
# v1 09JUN2022

library(DT)
library(jsonlite)
library(plotly)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)

ui <- fluidPage(theme = shinytheme("cerulean"),
  useShinyjs(),
  id = "form",
  navbarPage(title = "EPSScall"),
  sidebarLayout(
    sidebarPanel(
      h3("Queries:"),
      textInput("txt1", "CVE:", ""),
      textInput("txt2", "Date:", ""),
      textInput("txt3", "EPSS greater than:", ""),
      textInput("txt4", "Percentile greater than:", ""),
      actionButton("search", "Search"),
      downloadButton("download", "Download"),
      br(),
      br(),
      actionButton("resetAll", "Reset all"),
      br(),
      br(),
      p(strong("Notes:"),br(),
        "Input", strong("CVE"), "as CVE-YYYY-nnnn", br(),
        "Input", strong("Date"), "as YYYY-MM-DD", br(),
        "Input", strong("EPSS greater than"), "as 0.nn and", 
        strong("Percentile greater than"), "as 0.nn"),
    p(strong("Project:"),br(),
      a("GitHub", href = "https://github.com/holisticinfosec/EPSScall"), br(),
      a("Author", href = "https://twitter.com/holisticinfosec"))),
   
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Query", br(), 
                  p("The Exploit Prediction Scoring System (EPSS) 
                               is an open, data-driven effort for estimating 
                               the likelihood (probability) that a software 
                               vulnerabilities will be exploited in the wild."),
                           p("The EPSS", a("model", 
                           href = "https://www.first.org/epss/model"), 
                           "produces a probability score 
                               between 0 and 1 (0 and 100%). The higher the 
                               score, the greater the probability that a 
                               vulnerability will be exploited."),
                           p("Reference:", 
                             a("EPSS API", href = "https://www.first.org/epss/api")),
                           h3("Search Results"),
                           DT::dataTableOutput("tblOutput")),
                  tabPanel("Timeline", br(), dataTableOutput("timeOutput")),
                  tabPanel("Graph", br(), br(), p("Currently, only one CVE can be plotted at a time."),
                           plotlyOutput("graph"),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"
        )           
      )
    )
  )
))

server <- function(input, output, session) {

# EPSS CVEs logic  
    
  get_cves <- function(search) {
    url <- paste0("https://api.first.org/data/v1/epss?","cve=", input$txt1,
                  "&date=",input$txt2,"&epss-gt=", input$txt3,"&percentile-gt=",
                  input$txt4)
    EPSS_list <- fromJSON(url, flatten = TRUE)
    df <- EPSS_list$data
  }  
  
  cves <- reactive({
    query <- isolate(input$txt1)
    get_cves(query)
  })
  
  output$tblOutput <- DT::renderDT(cves())
  
  output$download <- downloadHandler(
    filename = function(){"EPSSresults.csv"}, 
    content = function(fname){
      write.csv(cves(), fname)
    })

# timeline logic  
    
  get_timeline <- function(search) {
    url <- paste0("https://api.first.org/data/v1/epss?","cve=", input$txt1,
                  "&scope=time-series")
    time_list <- fromJSON(url, flatten = FALSE)
    df <- as.data.frame(time_list$data$`time-series`)
  }
  
  timeline <- reactive({
    queryT <- isolate(input$txt1)
    get_timeline(queryT)
  })
  
  output$timeOutput <- DT::renderDT(timeline())

# visualize timeline logic  
    
  output$graph <- renderPlotly({
     url <- paste0("https://api.first.org/data/v1/epss?","cve=", input$txt1,
                   "&scope=time-series")
     time_list <- fromJSON(url, flatten = FALSE)
     df <- as.data.frame(time_list$data$`time-series`)
     x <- df$date
     y <- df$epss
     data <- data.frame(x, y)
     plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'lines')%>% 
     layout(title = list(text = "Timeline", x = 0.3), 
            xaxis = list(title = list(text ='Date')),
            yaxis = list(title = list(text ='EPSS')))
  })

# Reset input   
     
  observeEvent(input$resetAll, {
    reset("form")
  })

}

shinyApp(ui, server)