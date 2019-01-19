library(shiny)
library(shinyjs)
library(V8)
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


jsfile <- "getdata.js"
cssfile <- "style.css"
js_scroll_file <-'scroll.js'
ui <- shinyUI(
  # fluidPage(
  
  dashboardPage(
    
    header = dashboardHeader(title = span(
      tags$img(src = "Coat_of_arms_of_Nigeria.png", width = "50px", height = "50px"),
      "NCH Sample Dashboard", style = "font-family: Roboto; font-weight: bold"
    ), titleWidth = "400px",
    # Drop-down menus ----
    dropdownMenu(
      type = "messages",
      badgeStatus = "primary",
      icon = icon("comments"),
      messageItem(from = "Admin", message = "Development still in progress")
    ),
    dropdownMenu(
      type = "tasks",
      badgeStatus = "success",
      icon = icon("check-square"),
      taskItem(text = "Set-up Sample NCH dashboard", value = 20, color = "red")
    )),
    # sidebar menu ----
    sidebar = dashboardSidebar(width = "400px",collapsed = FALSE,
                               fluidRow(
                                 # h5("Drop Datasets in the box below"),
                                 # div(class="col-xs-12", id="drop-area", ondragover="dragOver(event)",ondrop="dropData(event)")
                                 div(class="col-xs-12", id="drop-area", ondragover="dragOver(event)",ondrop="dropData(event)", 
                                     textInput("csvurl", "paste link below"),
                                     actionButton("Update", "Update"))
                               ),
                               # Dashboard side bars ----
                               hidden(div(style="width=1000px",
                                          id='app-content',
                                          textInput("csvurl", "Input url of csv"),
                                          # selectInput("state", "Select state",csv_data_df$States, csv_data_df$States[1]),
                                          # selectInput("resolutions", "Select resolutions", resolution_names),
                                          sidebarMenu(id = "sidebarMenu",
                                                      menuItem(text = "Dashboard",
                                                               # menuSubItem("Manual Table Maker", tabName = "Manual_Table_Maker",icon = icon("table")),
                                                               # menuSubItem("Summary of NCH", tabName = "Formatted_table", icon = icon("list-alt")),
                                                               menuSubItem("Analysis Chart", tabName = "Barchart", icon = icon("bar-chart-o"), selected = TRUE),
                                                               # menuSubItem("Maps", tabName = "Maps", icon = icon("map")),
                                                               tabName = "dashboard", icon = icon("dashboard"), selected = TRUE, expandedName = "DashboardExpandedName", startExpanded = FALSE)
                                                      # hr(),
                                                      # Other menu items ----
                                                      # menuItem("Settings",
                                                      #          fluidRow(
                                                      #            column(6, selectInput("colours","Select colours",colours,"blue")),
                                                      #                   conditionalPanel(condition = "input['sidebarMenu'] != 'Manual_Table_Maker'",
                                                      #                                    column(6, selectInput("theme","Theme for charts", c(FALSE, "fivethirtyeight", "economist","darkunica","gridlight","sandsignika","null","handdrwran","chalk")))),
                                                      #                   fluidRow(column(12,checkboxInput('expandBookMarkOpts','Check to show bookmark options', value = FALSE, width = "100%"))),
                                                      #                   conditionalPanel(condition = "input.expandBookMarkOpts == true", 
                                                      #                                    selectizeInput(inputId = "bookmarkType",label = "Type of bookmark", choices = c("server","url"),options = list(create = TRUE,placeholder = "Select type of bookmark")),
                                                      #                                    # selectizeInput(inputId = "objBookmarks", label = "Objects",choices = allObjs,selected = NULL, multiple = TRUE),
                                                      #                                    selectInput("includeExcluedBk","Select one",choices = c("Include to bookmark","Exclude from bookmark")),
                                                      #                                    bookmarkButton(title = "Save the current state of the dashboard", label = "Bookmark dashboard"),
                                                      #                                    textOutput(outputId = "myprint"),
                                                      #                                    br(),
                                                      #                                    textOutput(outputId = "lastSaved"))
                                                      #          ), icon = icon("cog", lib = "glyphicon"))
                                                      # ,hr(),
                                                      # menuItem("Update and Export Data",icon = icon("refresh", lib="font-awesome"),badgeLabel = "not-active yet",badgeColor = "red")
                                                      # ,menuItem("Resources", tabName = "Resources", icon = icon("book"))
                                          ),hr(),h5("Chart Controls.")
                                          # Chart controls ----
                                          # fluidRow(
                                            # conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Formatted_table'",
                                            #                  column(6, selectInput("type", "Type", c("line","column","bar","spline"), "bar"))),
                                            # conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Formatted_table'",
                                            #                  column(6, selectInput("sortData","Sort Data", choices = c("Ascending","Descending","Default"), selected = "Descending"))),
                                            # conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Formatted_table'",
                                            #                  column(6, selectInput("aggr_fn","Aggregate function", choices = c("sum","Count" = "Length", "No. of values" = "NROW", "Distinct Count" = "n_distinct","mean","median","max","min","var","sd","IQR"), selected = "n_distinct"))),  
                                            # Primary labels ----
                                            # conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
                                            #                  column(6, selectInput("placeholder1", "Filter placeholder1",resolution_names, resolution_names[1]))),
                                            # conditionalPanel(condition = "input['sidebarMenu'] != 'Scatter_Chart' && input['sidebarMenu'] != 'Maps' && input['sidebarMenu'] != 'Correlation_Matrix' && input['sidebarMenu'] != 'Manual_Table_Maker' && input['sidebarMenu'] != 'Barchart' && input['sidebarMenu'] != 'Formatted_table'",
                                            #                  column(6, selectInput("placeholder1", "Filter placeholder2",resolution_names, resolution_names[1])))
                                            # )
                                          ))),
    
    
    
    # Dashboard body ----
    body = dashboardBody(
      tags$head(tags$link(rel="stylesheet", href=cssfile, type="text/css"),
                tags$script(src=jsfile),
                tags$script(src=js_scroll_file)),
      useShinyjs(),
      inlineCSS(appCSS),
      shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
      hidden(div(id='loading')),
      tabItems(
        tabItem(tabName = "dashboard",status = "primary",
                fluidRow(h1("Welcome to the MNH Quality of Care Dashboard for analysis"))
        ),
        # tabItem(tabName = "Manual_Table_Maker",
                # fluidRow(column(12, selectizeInput("all_primary_indicators", "Select multiple resolutions",c("Select multiple" = "",resolution_names), multiple = TRUE, selected = resolution_names[c(2,3,4)], width = "100%"))),
                # shiny::dataTableOutput("manualDTMaker")
        # ),
        # tabItem(tabName = "Formatted_table",
        #         formattableOutput("formattedTable")),
        tabItem(tabName = "Barchart",
                hidden(div(style="width=1000px",
                                                id='app-content',
                                                fluidRow(HTML('<h1> NCH Resolution Implementation Status Tracker </h1>'),actionButton("refresh", "Refresh to enter new data")),
                                                fluidRow(column(6,uiOutput("state_output")),column(6,uiOutput("resolutions_output"))),
                                                # fluidRow(uiOutput("bar_chart_per_resolution")),
                                                uiOutput("plots_first_row")
        ))
                # highchartOutput("bar_chart_per_state"),
                # highchartOutput("bar_chart_per_resolution")
                )
        # tabItem(tabName = "Resources","pdf document",hr(), tags$iframe(style = "height:350px; width:100%;scrolling=yes", src = "Primary_ Assessment_Tool.pdf"))
        # ,tabItem(tabName = "Maps")
        )),
    title = "Analysis of MNH QoC Assessments",
    skin = "green"
    ))