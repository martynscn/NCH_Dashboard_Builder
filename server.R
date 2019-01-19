library(DT)
library(shiny)
library(highcharter)
library(magrittr)
library(httr)
library(jsonlite)
library("data.table")
library(googlesheets)
library(tidyr)
library(dplyr,warn.conflicts = FALSE, quietly = TRUE)
library(formattable)
library(knitr)
library(shinydashboard)
library(lubridate)
library(readr)
library(ggplot2)
library(lazyeval)
library(tidyverse)
library(plotly)
library(V8)
#options(error = 999)
appCSS <- "
#loading {
  position: fixed;
left: 50%;
top: 50%;
z-index: 1;
width: 150px;
height: 150px;
margin: -75px 0 0 -75px;
border: 16px solid #f3f3f3;
border-radius: 50%;
border-top: 16px solid #3498db;
width: 120px;
height: 120px;
-webkit-animation: spin 2s linear infinite;
animation: spin 2s linear infinite;
}
@-webkit-keyframes spin {
0% { -webkit-transform: rotate(0deg); }
100% { -webkit-transform: rotate(360deg); }
}
@keyframes spin {
0% { transform: rotate(0deg); }
100% { transform: rotate(360deg); }
}
"



plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 12,colour = "black",hjust=0.5),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=8),
    axis.title = element_text(size=15),
    axis.text = element_text(size=15),
    axis.title.x = element_text(hjust=1,size=15),
    axis.title.y = element_text(hjust=1,size=15),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "bold"),
    legend.text = element_text(colour = "black", face = "bold"),
    axis.text.y = element_text(size=15),
    axis.text.x = element_text(vjust=-1,angle=90,size=15))
}
server <- function(input, output, session) {
  
  
  observeEvent(input$Update, {
    show("app-content")
    
    url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ5Zgn_CarwUu09Y_0r9Zc9TB1ZAo5Qh1UjhdnhS_h6RfNq7QolP-bA-56dX31mWG-vtK4OEENcrnQ-/pub?gid=0&single=true&output=csv"
    # url <- input$csvurl
    csv_data <- read.csv(file = url, stringsAsFactors = FALSE, na.strings = "N/A", check.names = FALSE)
    # csv_data <- read.csv(text=input$mydata[[name]], stringsAsFactors = FALSE, na.strings = "N/A", check.names = FALSE)
    original_names <- names(csv_data)
    cleaned_names <- make.names(original_names)
    csv_data_list <- lapply(1:ncol(csv_data), function(x) {
      if(x == 1) {csv_data[,x]
      } else {as.numeric(csv_data[,x])}})
 
    csv_data_df <- reactive({
       names_lookup_table <- data.frame(original_names = original_names, cleaned_names = cleaned_names)
 
      csv_data_df <- as.data.frame(x = csv_data_list,row.names = NULL, stringsAsFactors = FALSE,
                                   col.names = cleaned_names)
    })
    
    
    output$state_output <- renderUI({
      selectInput('state',label="Select state",choices=csv_data_df()$States,selected = csv_data_df()$States[1])
    })
    output$resolutions_output <- renderUI({
      resolution_names <-  cleaned_names[2:length(cleaned_names)]
      names(resolution_names) <- original_names[2:length(original_names)]
      selectInput('resolutions',label="Select resolutions",choices=resolution_names)
    })
    
    bar_data <- reactive({
      names_lookup_table <- data.frame(original_names = original_names, cleaned_names = cleaned_names)
      csv_data_df <- as.data.frame(x = csv_data_list,row.names = NULL, stringsAsFactors = FALSE,col.names = cleaned_names)
      resolution_symbol <- rlang::sym(input$resolutions)
      firstCol <- names(csv_data_df)[1]
      dt <- csv_data_df[,c(firstCol,input$resolutions)]
      dt <- arrange(dt, desc(!!resolution_symbol))
    })
    names_lookup_table <- reactive({
      names_lookup_table <- data.frame(original_names = original_names, cleaned_names = cleaned_names)
    })
    # Define bar_data_per_state ----
    bar_data_per_state <- reactive({
      state_symbol <- rlang::sym(input$state)
      dt <- as.vector(csv_data_df()[csv_data_df()[1] == input$state,c(2:ncol(csv_data_df()))])
      df <- data.frame(Resolution = names(csv_data_df())[2:ncol(csv_data_df())],StateValue = as.numeric(dt))
      df_joined <- left_join(x = df, y = names_lookup_table(), by = c("Resolution" = "cleaned_names"))
      df_joined_mutated <- mutate(df_joined, Resolution_Abbr = paste("Resolution ", 1:nrow(df_joined)))
      arranged_df <- arrange(df_joined_mutated, desc(StateValue))
    })
    # Resolution bar chart ----
    output$bar_chart_per_resolution <- renderHighchart({
      i <- which(names_lookup_table()[,2] == input$resolutions)
      resolution_full_name <- names_lookup_table()[i,1]
      series1 <- bar_data()[,2] * 100
      series_name <- paste("Resolution", i-1, sep = " ")
      h <- highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_title(text = resolution_full_name) %>% 
        hc_xAxis(categories = bar_data()[,1]) %>% 
        hc_add_series(data = series1,
                      name = series_name) %>% 
        hc_yAxis(title = list(text = "% completion"),
                 labels = list(format = "{value}%"), max = 100) %>% 
        hc_legend(enabled = FALSE) %>% 
        hc_tooltip(pointFormat = paste0(series_name, " completion rate: {point.y}%"))
    })
    
    # States bar chart ----
    output$bar_chart_per_state <- renderHighchart({
      series1 <- bar_data_per_state()[,2] * 100
      series_name <- input$state
      highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_title(text = paste("Report from ",input$state, sep = " ")) %>% 
        hc_xAxis(categories = bar_data_per_state()[,4]) %>% 
        hc_add_series(data = series1,
                      name = series_name) %>% 
        hc_yAxis(title = list(text = "% completion"),
                 labels = list(format = "{value}%"), max = 100) %>% 
        hc_legend(enabled = FALSE) %>% 
        hc_tooltip(useHTML = FALSE, pointFormat = "Completion rate of {point.y}%") 
    })
    observeEvent(input$refresh, {
      
      show(id="loading", anim = TRUE, animType = "fade")
      Sys.sleep(1.5)
      
      hide(id = "loading", anim = TRUE, animType = "fade") 
      shinyjs::js$refresh()
    })
    output$plots_first_row <- renderUI({
      fluidRow(
               highchartOutput("bar_chart_per_state"),
               highchartOutput("bar_chart_per_resolution"),
         HTML('<button onclick="topFunction()" id="myBtn" title="Go to top">Go Back To The Top</button>')
        
      )
    })
    for (name in names(input$mydata)) {
      output[[name]] <- renderTable(head(read.csv(text=input$mydata[[name]]),4))
    }
  })
}
