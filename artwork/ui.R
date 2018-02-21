library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(DT)
library(plotly)

dashboardPage(
  
  dashboardHeader(title = "Artwork Prices"),
  
  dashboardSidebar(
    sidebarMenu(id = "sbmenu",
    menuItem("Prices", tabName = "prices", icon = icon("line-chart")),
    br(),
    actionButton("buttonRunAnalysis", "Update predictions", icon = icon("arrow-circle-right"), width="87%")
    )
    
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "prices",
              h2("Artwork Prices"),
              conditionalPanel(condition = "input.buttonRunAnalysis%2 == 1",
              fluidRow(
                box(
                  title = "Table", width = 12, status = "primary",collapsible = FALSE,
                  withSpinner(dataTableOutput("table_plot"))
                )
              ))
              
      )
  )))
  

