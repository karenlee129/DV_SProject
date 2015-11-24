


df1 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select country, year, real_gross_domestic_income
                                                 from globaleconomics
                                              ;"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_ryl96', PASS='orcl_ryl96', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))

View(df1)

#ui.R 
library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

# Application title
headerPanel("Hello Shiny!"),

# Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("KPI1", 
                "KPI_Low_Max_value:", 
                min = 1,
                max = 1930.22, 
                value = 1930.22),
    sliderInput("KPI2", 
                "KPI_Medium_Max_value:", 
                min = 1930.22,
                max = 4108.52, 
                value = 4108.52)
  ),

# Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
    #plotOutput("distTable")
  )
))
