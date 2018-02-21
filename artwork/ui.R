library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(DT)
library(plotly)

dashboardPage(
  
  dashboardHeader(title = "Artwork Prices"),
  
  dashboardSidebar(
    sidebarMenu(id = "sbmenu",
    menuItem("Prices", tabName = "prices", icon = icon("line-chart")))
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "prices",
              h2("Artwork Prices"),
              fluidRow(
                box(
                  title = "Table", width = 12, status = "primary",collapsible = FALSE,
                  withSpinner(dataTableOutput("table_plot"))
                )
              )
      )
  )))
  

