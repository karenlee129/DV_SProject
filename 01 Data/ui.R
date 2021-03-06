#ui.R

require(shiny)
require(shinydashboard)
require(leaflet)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Barchart", tabName = "barchart", icon = icon("bar-chart-o")),
      menuItem("Scatter Plot", tabName = "scatterplot", icon = icon("th"))
      #menuItem("Map", tabName = "map", icon = icon("th")),
      #menuItem("Table", tabName = "table", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "crosstab",
              actionButton(inputId = "light", label = "Light"),
              actionButton(inputId = "dark", label = "Dark"),
              sliderInput("KPI1", "KPI_Low_Max_value:", 
                          min = 1, max = 1930.22,  value = 1930.22),
              sliderInput("KPI2", "KPI_Medium_Max_value:", 
                          min = 1930.23, max = 7108.52,  value = 7108.52),
              textInput(inputId = "title", 
                        label = "Crosstab Title",
                        value = "global economics"),
              actionButton(inputId = "clicks1",  label = "Click me"),
              plotOutput("distPlot1")
      ),
      
      # Second tab content
      tabItem(tabName = "barchart",
              #actionButton(inputId = "clicks2",  label = "Click me"),
              plotOutput("distPlot2")
      ),
      
      # Third tab content
      tabItem(tabName = "scatterplot",
              #actionButton(inputId = "clicks3",  label = "Click me"),
              plotOutput("distPlot3")
      )
    )
  )
)

