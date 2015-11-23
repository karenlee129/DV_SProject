# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    # Start your code here.
    
    # The following is equivalent to KPI Story 2 Sheet 2 and Parameters Story 3 in "Crosstabs, KPIs, Barchart.twb"
    
    KPI_Low_Max_value = input$KPI1     
    KPI_Medium_Max_value = input$KPI2
    
    KPI_Low_Max_value = 1930.22     
    KPI_Medium_Max_value = 4108.52
    
    df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                    "select country, real_gross_domestic_income, year
                                                    from globaleconomics
                                                    where real_gross_domestic_income is not NULL;"')), 
                                     httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_ryl96', PASS='orcl_ryl96', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); 
    
    df<-dplyr::filter(df, YEAR > 2000)
    df<-dplyr::filter(df, YEAR < 2010)
    
    df <- df %>% mutate(ratio = REAL_GROSS_DOMESTIC_INCOME) %>% mutate(KPI = ifelse(ratio <= KPI_Low_Max_value, '03 Low', ifelse(ratio <= KPI_Medium_Max_value, '02 Medium', '01 High')))
    
 plot <- ggplot() + 
      coord_cartesian() + 
      scale_y_discrete() +
      labs(title='KPI of Countries from 2001-2009') +
      labs(x=paste("Year"), y=paste("Country")) +
      
      layer(data=df, 
            mapping=aes(x=YEAR, y=COUNTRY, label=ratio), 
            stat="identity", 
            stat_params=list(), 
            geom="text",size = 3,
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=df, 
            mapping=aes(x=YEAR, y=COUNTRY, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) 
    
    # End your code here.
    return(plot)
  }) # output$distPlot
})
