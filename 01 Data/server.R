# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.50 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  

  
  df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select country, year, round(real_gross_domestic_income) as real_gross_domestic_income, kpi as ratio, 
case
when kpi < "p1" then \\\'03 Low\\\'
when kpi < "p2" then \\\'02 Medium\\\'
else \\\'01 High\\\'
end kpi
from (select country, year, 
sum(real_gross_domestic_income) as real_gross_domestic_income,
sum(real_gross_domestic_income) as kpi
from globaleconomics
where real_gross_domestic_income is not NULL and year > 2000 and year < 2009
group by country, year);"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_ryl96', PASS='orcl_ryl96', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value(), p2=KPI_Medium_Max_value()), verbose = TRUE)))
  })
  
  output$distPlot1 <- renderPlot(height=1800, width=900, {             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_y_discrete() +
      labs(title='KPI of Countries from 2001-2009') +
      labs(x=paste("Year"), y=paste("Country")) +
      
      layer(data=df1(), 
            mapping=aes(x=YEAR, y=COUNTRY, label=REAL_GROSS_DOMESTIC_INCOME), 
            stat="identity", 
            stat_params=list(), 
            geom="text",size = 3,
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=YEAR, y=COUNTRY, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) 
    plot
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for Second Tab:
  
  df2 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select country, CONSUMPTION_PERCENTAGE, year
from globaleconomics
where YEAR = 2009
order by CONSUMPTION_PERCENTAGE desc;"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_ryl96', PASS='orcl_ryl96', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))) 

  output$distPlot2 <- renderPlot(height=600, width=900, {
    plot1 <- ggplot() + 
      geom_bar() +
      coord_flip() +
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Consumption Percentage per Country in 2009') +
      labs(x=paste("Country"), y=paste("Consumption Percentage")) +
      layer(data=df2, 
            mapping=aes(x=COUNTRY, y=CONSUMPTION_PERCENTAGE, label=CONSUMPTION_PERCENTAGE), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) 
    
    plot1
  })
  
  # Begin code for Third Tab:
  
  df3 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select POP, REAL_GDP, YEAR, COUNTRY
from globaleconomics
where REAL_GDP is not NULL and POP < 266859;"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_ryl96', PASS='orcl_ryl96', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  
  output$distPlot3 <- renderPlot(height=600, width=900, {
    plot3 <- ggplot() + 
      geom_point() +
      labs(title='Real GDP Versus Population') +
      labs(x=paste("Pop"), y=paste("Real GDP")) +
      layer(data=df3, 
            mapping=aes(x=POP, y=REAL_GDP, color=YEAR, label=COUNTRY), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            position=position_identity()
      ) +
      layer(data=df3, 
            mapping=aes(x=POP, y=REAL_GDP, color=YEAR, label=COUNTRY), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(), 
            position=position_identity()
            
      ) 
    plot3
  })
})
