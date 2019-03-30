library(shinyjs)
library(dplyr)
library(tibble)
library(formattable)
source("helper.R", local = TRUE)

# Define UI ----
ui <- shinyUI(
  dashboardPage(
    header = dashboardHeader(title = span(tags$img(src = "Coat_of_arms_of_Nigeria.png", width = "50px", height = "50px", align = "left"),
      "NCH Dashboard", style = "font-family: Roboto; font-weight: bold; align: right"
    ), titleWidth = "300px",
    # Drop-down menus ----
    dropdownMenu(
      type = "messages",
      badgeStatus = "primary",
      icon = icon("comments"),
      messageItem(from = "Admin", message = "Development still in progress")),
    dropdownMenu(
      type = "tasks",
      badgeStatus = "success",
      icon = icon("check-square"),
      taskItem(text = "Set-up correlation analysis", value = 100, color = "green"),
      taskItem(text = "Set-up table maker and summariser", value = 100, color = "green"),
      taskItem(text = "Set-up Map view", value = 50, color = "yellow"),
      taskItem(text = "Develop grading system to grade issues", value = 15, color = "red"))),
    # Side bar menus ----
    sidebar = dashboardSidebar(
      tags$head(tags$link(rel="stylesheet", href=cssfile, type="text/css"),
                tags$script(src=jsfile),
                tags$script(src=js_scroll_file)),
      useShinyjs(),
      inlineCSS(appCSS),
      shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
      hidden(div(id='loading')),
      width = "300px",collapsed = FALSE,
      fluidPage(
        fluidRow(
          div(class="col-xs-12", id="drop-area", ondragover="dragOver(event)",ondrop="dropData(event)", width = "100px", height = "100px"
              ,fluidRow(
                column(8,textInput("csvurl",NULL, placeholder = "paste link below")),
                column(4,actionButton("Update", "Update"))
                , height = "100px")
          )
        ),
        sidebarMenu(id = "sidebarMenu",
                    menuItem(text = "Dashboard",
                             menuSubItem("Per state", tabName = "per_state", icon = icon("bar-chart-o")),
                             menuSubItem("Per resolution", tabName = "per_resolution", icon = icon("bar-chart-o")),
                             menuSubItem("Map", tabName = "map", icon = icon("map")),
                             menuSubItem("Summary",tabName = "summary", icon = icon("table"), selected = TRUE),
                             tabName = "dashboard", icon = icon("dashboard"), expandedName = "DashboardExpandedName", startExpanded = FALSE),
                    menuItem("Settings",
                             fluidRow(column(6, selectInput("colours","Select colours",colours,"blue")),
                                      column(6, selectInput("theme","Theme for charts", themes))),
                             fluidRow(column(12,checkboxInput('expandBookMarkOpts','Check to show bookmark options', value = FALSE, width = "100%"))),
                                      conditionalPanel(condition = "input.expandBookMarkOpts == true", 
                                                       selectizeInput("bookmarkType",label = "Type of bookmark", choices = c("server","url"),options = list(create = TRUE,placeholder = "Select type of bookmark")),
                                                       selectizeInput("objBookmarks", label = "Objects",choices = allObjs,selected = NULL, multiple = TRUE),
                                                       selectInput("includeExcluedBk","Select one",choices = c("Include to bookmark","Exclude from bookmark")),
                                                       bookmarkButton(title = "Save the current state of the dashboard", label = "Bookmark dashboard"),
                                                       textOutput(outputId = "myprint"),
                                                       br(),
                                                       textOutput(outputId = "lastSaved"))
                             ), icon = icon("cog", lib = "glyphicon")),
        hr(),h6("Chart Controls"),
        fluidRow(
          column(6, selectInput("type","Type",c("line","column","bar","spline"),"bar")),
          column(6, selectInput("sortData","Sort Data", c("Ascending","Descending","Default"), "Descending"))
          )
        )
      ),
    body = dashboardBody(
      tags$head(tags$link(rel="stylesheet", href=cssfile, type="text/css"),
                tags$script(src=jsfile),
                tags$script(src=js_scroll_file)),
      useShinyjs(),
      inlineCSS(appCSS),
      shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
      hidden(div(id='loading')),
      fluidPage(
        tabItems(
          tabItem(tabName = "dashboard",status = "primary",
                  fluidRow(h1("Welcome to the MNH Quality of Care Dashboard for analysis"))),
          tabItem(tabName = "per_state", 
                  div(style = "width=1000px", id = "state-content",
                      uiOutput("state_output"),
                      highchartOutput("bar_chart_per_state")
                  )
          ),
          tabItem(tabName = "per_resolution",
                  div(style="width=1000px",id='app-content',
                      fluidRow(column(3,actionButton("refresh", "Refresh to enter new data")),
                               column(9,HTML('<h5> NCH Resolution Implementation Status Tracker (Per Resolution) </h5>'))),
                      fluidRow(column(12,uiOutput("resolutions_output", width = "100%")), width = "100%"),
                      highchartOutput("bar_chart_per_resolution"),
                      # uiOutput("plots_first_row")
                      HTML('<button onclick="topFunction()" id="myBtn" title="Go to top">Go Back To The Top</button>')
                  )),
          tabItem(tabName = "map",
                  fluidRow(column(12,uiOutput("map_resolution_output", width = "100%"))),
                  highchartOutput("res_map"),
                  HTML('<button onclick="topFunction()" id="myBtn" title="Go to top">Go Back To The Top</button>')
                  ),
          tabItem(tabName = "summary"
                  ,formattableOutput("summaryTable")
                  )
        )
      )),
    title = "NCH Sample Dashboard",
    skin = "yellow"))