# install.packages('rsconnect')

## Shiny 
library(dygraphs)
library(shiny)
library(shinythemes)


######################
# User Interface: Creates the structure for your app's look and appearance (Frontend)
library(shinyWidgets)

ui = navbarPage(
  
  title = "House Sales", 
  
  theme = shinytheme("superhero"),
  
  tabPanel(
    title = "Explore", 
    
    sidebarLayout(
      
      sidebarPanel(
        width = 1, 
        height = 100, 
        # h1 = "Explore the Dataset", 
        
        switchInput(
          inputId = "Id015",
          label = "Shock", 
          labelWidth = "10px"
        ),
        
        switchInput(
          inputId = "Id015_2",
          label = "NAs", 
          labelWidth = "10px"
        ),
          
        selectInput(
          inputId = "n",
          label = "Select Zip Code", 
          choices = names(split_zip), 
          selected = 1
        ),
      
        # width = - 3, 
        # h1 = "Explore the Dataset", 
        selectInput(
          inputId = "n2",
          label = "Advanced Information", 
          choices = c("Outlook", "Weekly periodicity", "Monthly periodicity", "Seasonality", 
                      "Trend", "Stochastic Process", "Intervention Deep dive"), 
          selected = 1,
          plotly::plotlyOutput("second"))
      ),
      
      dygraphs::dygraphOutput("first"),
      plotly::plotlyOutput("second")
    )
  )
)

##################
# The server is for actually running R code. (Backend)

server = function(input, output) {
  
  output$first = renderDygraph({
    
    tseries = as_tibble(split_zip[[as.character(input$n)]]) %>% 
      dplyr::group_by(datesold) %>% 
      dplyr::summarise(Expense = sum(price)) %>% 
      dplyr::mutate(datesold = as.Date(datesold)) %>% 
      dplyr::left_join(dates, by = "datesold") %>%
      dplyr::arrange(datesold) %>%
      dplyr::mutate(na_filled = na_interpolation(Expense, option = "spline"))

    series = xts(x = tseries$Expense, order.by = tseries$datesold)
    
    dygraph(series, main = paste("Housing Expenditure - ", as.character(input$n))) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
    dyRangeSelector() %>% 
    dyRoller(rollPeriod = 1) %>%
      dyOptions(colors = "#FF9900")
  })
  
  output$second = renderPlotly({
    
    if(input$n2 == "Weekly periodicity"){  
    
    tseries = as_tibble(split_zip[[as.character(input$n)]]) %>% 
      dplyr::group_by(datesold) %>% 
      dplyr::summarise(Expense = sum(price)) %>% 
      dplyr::mutate(datesold = as.Date(datesold)) %>% 
      dplyr::left_join(dates, by = "datesold") %>%
      dplyr::arrange(datesold) %>%
      dplyr::mutate(na_filled = na_interpolation(Expense, option = "spline"))
    
    
    tseries$day = weekdays(ymd(tseries$datesold))
      
    days = tseries %>% 
        dplyr::group_by(day) %>% 
        dplyr::summarise(mean = mean(Expense), 
                         se = sqrt(var(Expense)),
                         upper = qnorm(0.80, mean, se), 
                         lower = qnorm(0.20, mean, se))
      
    days$day = ordered(days$day, levels=c("Monday", "Tuesday", "Wednesday", 
                                            "Thursday","Friday", "Saturday", "Sunday"))
    
    
    data = data.frame(days$mean, days$se, days$day)
   
      
    plot_ly(data = data, x = ~days.day, y = ~days.mean, type = 'bar',
                                                       error_y = ~list(array = days.se,
                                                                       color = '#FF9900'), 
                                                       marker = list(color = 'rgb(158,202,225)',
                                                                     line = list(color = 'rgb(8,48,107)',
                                                                                 width = 1.5))) %>% layout(
                                                                                   title = 'Internal EAD (90% Confidence Zone)',
                                                                                   font = t,
                                                                                   xaxis = list(
                                                                                     type = 'category',
                                                                                     title = 'Day of the Week'
                                                                                   ),
                                                                                   yaxis = list(
                                                                                     title = '% Network delivered on time',
                                                                                     range = c(0,max(days$mean)*1.6)
                                                                                   ))
      }
      
  })
    
}


shinyApp(ui = ui, server = server)










