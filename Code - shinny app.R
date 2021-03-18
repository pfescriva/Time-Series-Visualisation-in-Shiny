############
#### Preliminary Stuff

devtools::install_git("https://github.com/bernardo-dauria/kaggler.git")

library(kaggler)

kgl_auth(username = "mkearney", key = "9as87f6faf9a8sfd76a9fsd89asdf6dsa9f8")

library(reticulate)

# py_install("kaggle") # In case 

kaggle = import("kaggle")

kaggle$api$authenticate()

kaggle$api$dataset_download_files("htagholdings/property-sales", "raw_sales.csv", unzip = T)

suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(astsa))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(fUnitRoots))
suppressPackageStartupMessages(library(FitARMA))
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(fBasics))

library(plotly)
library(marima)
library(MASS)
library(reticulate)
library(imputeTS)
library(lubridate)
library(tidyverse)
library(dygraphs)
library(xts)
library(tseries)


################
#### Kick-off

data = read.csv("C:/Users/Carmen/OneDrive/Archivos - Todo/1 - Master Statistics/Period 3/Time Series/Time-Series-Visualisation-in-Shiny/raw_sales.csv/raw_sales.csv")

dates = tibble(datesold = seq(as.Date(min(data$datesold)), as.Date(max(data$datesold)), by='day'))
data$datesold = as.Date(data$datesold)
library(shiny)

detach("package:shiny", unload = TRUE)

tseries = data %>% 
  dplyr::group_by(datesold) %>% 
  dplyr::summarise(Expense = sum(price)) %>% 
  dplyr::right_join(dates, by = "datesold") %>%
  dplyr::arrange(datesold) %>%
  dplyr::mutate(na_filled = na_interpolation(Expense, option = "spline"))

any(is.na(tseries)) # There are no missing values

series = xts(x = tseries$Expense, order.by = tseries$datesold)
series = xts(x = tseries$na_filled, order.by = tseries$datesold)

dygraph(series, main = "PC Componentes Total Sales Evolution") %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
  dyRangeSelector() %>% 
  dyRoller(rollPeriod = 1)

library(tsibble)
ts = ts(tseries$Expense, start = as.Date(min(tseries$datesold)))
compos <- mstl(ts(tseries$datesold, frequency = 7))

autoplot(compos)

time = cbind(tseries, compos$random)
names(time) = c("datesold", "Expense", "Filled", "random")


##########
### App



## Shiny 
library(dygraphs)
library(shiny)
library(shinythemes)

# User Interface: Creates the structure for your app's look and appearance (Frontend)

ui = navbarPage(
  
  title = "House Sales", 
  
  theme = shinytheme("superhero"),
  
  tabPanel(
    title = "Explore", 
    
    sidebarLayout(
      
      sidebarPanel(
        width = 2, 
        height = 100, 
        # h1 = "Explore the Dataset", 
        selectInput(
          inputId = "Group_choice",
          label = "Select Zip Code", 
          choices = colnames(data), 
          selected = 1
        ),
        
        # width = - 3, 
        # h1 = "Explore the Dataset", 
        selectInput(
          inputId = "Group_choice2",
          label = "Advanced Information", 
          choices = colnames(data), 
          selected = 1      )
      ),
      
      mainPanel(dygraphs::dygraphOutput("uniplot"))
      
    )
  ),
  
  tabPanel(
    title = "Forecasts", 
    
    sidebarLayout(
      
      sidebarPanel(
        width = 2, 
        # h1 = "Explore the Dataset", 
        selectInput(
          inputId = "Group_choice",
          label = "Select Zip Code", 
          choices = colnames(data), 
          selected = 1
        ),
        
        # width = - 3, 
        # h1 = "Explore the Dataset", 
        selectInput(
          inputId = "Group_choice2",
          label = "Advanced Information", 
          choices = colnames(data), 
          selected = 1      )
      ),
      
      mainPanel(dygraphOutput("uniplot"))
      
    )
  ),
  
  tabPanel(
    title = "Forecasts Deep Dive", 
    
    sidebarLayout(
      
      sidebarPanel(
        width = 2, 
        # h1 = "Explore the Dataset", 
        selectInput(
          inputId = "Group_choice",
          label = "Select Zip Code", 
          choices = colnames(data), 
          selected = 1
        ),
        
        # width = - 3, 
        # h1 = "Explore the Dataset", 
        selectInput(
          inputId = "Group_choice2",
          label = "Advanced Information", 
          choices = colnames(data), 
          selected = 1      )
      ),
      
      mainPanel(dygraphOutput("uniplot"))
      
    )
  ),
  
  tabPanel(
    title = "Prices Overview", 
    
    sidebarLayout(
      
      sidebarPanel(
        width = 2, 
        # h1 = "Explore the Dataset", 
        selectInput(
          inputId = "Group_choice",
          label = "Select Zip Code", 
          choices = colnames(series), 
          selected = 1
        ),
        
        # width = - 3, 
        # h1 = "Explore the Dataset", 
        selectInput(
          inputId = "Group_choice2",
          label = "Advanced Information", 
          choices = colnames(series), 
          selected = 1      )
      ),
      
      mainPanel(dygraphOutput("uniplot"))
      
    )
  )
)


# The server is for actually running R code. (Backend)

server <- function(input, output) {
  input$Group_choice 
  output$uniplot = renderDygraph({dygraph(series, main = "PC Componentes Total Sales Evolution") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
      dyRangeSelector() %>% 
      dyRoller(rollPeriod = 1)})
  
}


shinyApp(ui = ui, server = server)














####### 
# Drafty stuff



# So, yeah, you need either to improve the imputation deal with outliers first 

outliers = time %>% 
  select(datesold, random) %>%
  drop_na %>%
  filter(random < quantile(random, 0.25) - 3*IQR(random) | random > quantile(random, 0.75) + 3*IQR(random))



# Now you want to do this by zipcode!!!

outliers = outliers %>% 
  right_join(tseries, series, by = "datesold")



compos$random

as_tibble(compos)


stl(tseries$Expense)

dcmp = series %>%
  decompose() %>%
  autoplot()


dim(data)
str(data)

library(tidyverse)
library(forecast)
library(tseries)
library(tsibble)

x <- ts(data$price)
stl(x, "periodic")

dcmp = data %>%
  decompose() %>%
  autoplot()

components(dcmp)

stl(data)

components(dcmp) %>% autoplot()





# For me to understand the data 

table(data$propertyType)

# Overall analysis

