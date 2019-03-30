library(shinyjs)
library(tibble)
source("helper.R", local = TRUE)

server <- function(input, output, session) {
  observeEvent(
    input$Update, {
    # input$mydata, {
    show("app-content")
    # url <- input$csvurl
    # csv_data <- read.csv(file = url, stringsAsFactors = FALSE, na.strings = "N/A", check.names = FALSE)
    csv_data <- read.csv(file = "www/NCH_Training_2019 - Summary.csv", stringsAsFactors = FALSE, na.strings = "N/A", check.names = FALSE)
    # csv_data <- read.csv(text=input$mydata[[name]], stringsAsFactors = FALSE, na.strings = "N/A", check.names = FALSE)
    original_names <- names(csv_data)
    cleaned_names <- make.names(original_names)
    resolution_names <-  cleaned_names[2:length(cleaned_names)]
    names(resolution_names) <- original_names[2:length(original_names)]
    csv_data_list <- lapply(1:ncol(csv_data), function(x) {
      if(x == 1) {csv_data[,x]
      } else {as.numeric(csv_data[,x])}})
    names_lookup_table <- data.frame(original_names = original_names, cleaned_names = cleaned_names)
    csv_data_df <- as.data.frame(x = csv_data_list,row.names = NULL, stringsAsFactors = FALSE,
                                   col.names = cleaned_names)

    output$state_output <- renderUI({
      selectInput('state',label="Select state",choices=csv_data_df$States,selected = csv_data_df$States[1])
    })
    output$resolutions_output <- renderUI({
      selectInput('resolutions',label="Select resolution",choices=resolution_names)
    })
    output$map_resolution_output <- renderUI({
      selectInput("map_resolution","Select resolution",choices=resolution_names)
    })
    bar_data_per_resolution <- reactive({
      names_lookup_table <- data.frame(original_names = original_names, cleaned_names = cleaned_names)
      csv_data_df <- as.data.frame(x = csv_data_list,row.names = NULL, stringsAsFactors = FALSE,col.names = cleaned_names)
      resolution_symbol <- rlang::sym(input$resolutions)
      firstCol <- names(csv_data_df)[1]
      dt <- csv_data_df[,c(firstCol,input$resolutions)]
      if(input$sortData == "Descending") {
        dt <- arrange(dt, desc(!!resolution_symbol))
      } else if(input$sortData == "Ascending") {
        dt <- arrange(dt, !!resolution_symbol)
      }
      dt
    })
    names_lookup_table <- reactive({
      names_lookup_table <- data.frame(original_names = original_names, cleaned_names = cleaned_names)
    })
    # Define bar_data_per_state ----
    bar_data_per_state <- reactive({
      state_symbol <- rlang::sym(input$state)
      dt <- as.vector(csv_data_df[csv_data_df[1] == input$state,c(2:ncol(csv_data_df))])
      df <- data.frame(Resolution = names(csv_data_df)[2:ncol(csv_data_df)],StateValue = as.numeric(dt))
      df_joined <- left_join(x = df, y = names_lookup_table(), by = c("Resolution" = "cleaned_names"))
      df_joined_mutated <- mutate(df_joined, Resolution_Abbr = paste("Resolution ", 1:nrow(df_joined)))
      dt <- df_joined_mutated
      if(input$sortData == "Descending") {
        dt <- arrange(dt, desc(StateValue))
      } else if(input$sortData == "Ascending") {
        dt <- arrange(dt, StateValue)
      }
      dt
    })
    # Resolution bar chart ----
    output$bar_chart_per_resolution <- renderHighchart({
      i <- which(names_lookup_table()[,2] == input$resolutions)
      resolution_full_name <- names_lookup_table()[i,1]
      series1 <- bar_data_per_resolution()[,2] * 100
      series_name <- paste("Resolution", i-1, sep = " ")
      h <- highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_title(text = resolution_full_name) %>% 
        hc_xAxis(categories = bar_data_per_resolution()[,1]) %>% 
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
               # highchartOutput("bar_chart_per_state"),
               # highchartOutput("bar_chart_per_resolution"),
         HTML('<button onclick="topFunction()" id="myBtn" title="Go to top">Go Back To The Top</button>')
        
      )
    })
    for (name in names(input$mydata)) {
      output[[name]] <- renderTable(head(read.csv(text=input$mydata[[name]]),4))
    }
    #Theme Expression ----
    themeExpr <- reactive({
      switch(input$theme,
             null = hc_theme_null(),
             darkunica = hc_theme_darkunica(),
             gridlight = hc_theme_gridlight(),
             sandsignika = hc_theme_sandsignika(),
             fivethirtyeight = hc_theme_538(),
             economist = hc_theme_economist(),
             chalk = hc_theme_chalk(),
             handdrwran = hc_theme_handdrawn()
      )
    })
    
    # Map logic ----
    output$res_map <- renderHighchart({
      NigeriaStates <- data.frame(code = c("Abia", "Adamawa", 
                                           "Akwa Ibom", "Anambra", "Bauchi", "Bayelsa", "Benue", "Borno", 
                                           "Cross River", "Delta", "Ebonyi", "Edo", "Ekiti", "Enugu", "Federal Capital Territory", 
                                           "Gombe", "Imo", "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", 
                                           "Kogi", "Kwara", "Lagos", "Nassarawa", "Niger", "Ogun", "Ondo", 
                                           "Osun", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe", 
                                           "Zamfara"), MNHStateNames = c('Abia','Adamawa','Akwa Ibom','Anambra','Bauchi','Bayelsa','Benue','Borno','Cross River','Delta','Ebonyi','Edo','Ekiti','Enugu','FCT','Gombe','Imo','Jigawa','Kaduna','Kano','Katsina','Kebbi','Kogi','Kwara','Lagos','Nasarawa','Niger','Ogun','Ondo','Osun','Oyo','Plateau','Rivers','Sokoto','Taraba','Yobe','Zamfara'))
      df <- csv_data_df
      my_map_data <- left_join(x = NigeriaStates, y = df, by = c("MNHStateNames" = "States"),copy = TRUE)
      res <- input$map_resolution
      filtered_map_data <- my_map_data[!is.na(my_map_data[,res]),c("code",res)]
      hc <- hcmap("countries/ng/ng-all",download_map_data = FALSE, data = filtered_map_data, value = res,
                  joinBy = c("woe-name", "code"), name = res,
                  dataLabels = list(enabled = TRUE, format = "{point.name}"),
                  borderColor = "#FAFAFA", borderWidth = 0.1,
                  tooltip = list(valueDecimals = 2, valuePrefix = "",
                                 valueSuffix = ""))
      hc
    })
    # summary formattable logic ----
    output$summaryTable <- formattable::renderFormattable({
      csv_data_interval <- sapply(csv_data_df[2:ncol(csv_data_df)], function(x) {
        cut(x, breaks = c(-Inf,0.01,0.25,0.50,0.75,0.99,Inf),
            labels = c("No action taken","Slow progress","Moderate progress","Significant progress","Near completion","Completed"))
      })
      csv_data_int_df <- as.data.frame(csv_data_interval)
      csv_data_int_df <- add_column(csv_data_int_df,States = csv_data_df$States, .before = 1)
      names(csv_data_int_df)[2:ncol(csv_data_int_df)] <- paste0("R",c(1:(ncol(csv_data_int_df)-1)))
      library(formattable)
      
      formatted_table <- 
        formattable(x = csv_data_int_df
                    ,
                    formatters =
                      list(
                        States =
                          formatter(
                            .tag = "span",
                            style = x ~ ifelse(test = 
                              x == "FCT", yes = formattable::style(color = "green", font.weight = "bold"),no = NA)
                          ),
                          area(col = 2:ncol(csv_data_int_df)) ~ formatter(
                          "div",
                            # "span",
                          style = x ~
                            ifelse(
                              is.na(x),
                              formattable::style(color = "black", font.weight = "bold",border = "1px","border-radius" = "4px", background = "#e5e8e8"),
                              ifelse(
                              x == "Near completion",
                              formattable::style(color = "white", font.weight = "bold",border = "1px","border-radius" = "4px", background = "#6aa84f"),
                              ifelse(
                                x == "No action taken",
                                formattable::style(color = "black", font.weight = "bold",border = "1px", background = "#e94443"),
                                ifelse(
                                  x == "Slow progress",
                                  formattable::style(color = "black", font.weight = 5,border = "1px", background = "#bf9000"),
                                  ifelse(
                                    x == "Moderate progress",
                                    formattable::style(color = "black", font.weight = 5,border = "1px", background = "#fef83f"),
                                    ifelse(
                                      x == "Significant progress",
                                      formattable::style(color = "black", font.weight = 2,background = "#a3ea16","padding-right" = "2px"), #"background-color" = "gray" can also work
                                      ifelse(
                                        x == "Completed",
                                        formattable::style(color = "white", font.weight = "bold",background = "#274e13", font.size = 2)
                                        ,NA
                                        # ,x ~ icontext("")
                                      )))))))
                        ))
                    ,align = c("c","c","c"))
    })
  })
}
