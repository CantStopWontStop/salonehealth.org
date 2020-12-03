# load packages
library(tidyverse)
library(magrittr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(lubridate)


# get covid data
WHO_COVID_19_global_data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") 

# select Salone data
salone <- WHO_COVID_19_global_data %>%
  filter(Country == 'Sierra Leone') %>%
  filter(Date_reported > ymd(20200331))


# plot Salone epicurve
saloneNewCases <- ggplot(data = salone) + 
  geom_col(mapping = aes(Date_reported, New_cases),fill="#262626") +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs( x = "Date", y = "New Cases",
          title ="New COVID-19 Cases in Sierra Leone",
          caption = "Last Refreshed"
        ) +
  theme_minimal()

saloneNewCases


ggplotly(saloneNewCases)

saloneNewDeaths <- ggplot(data = salone) + 
  geom_col(mapping = aes(Date_reported, New_deaths),fill="#262626") +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs( x = "Date", y = "New Deaths",
        title ="New COVID-19 Deaths in Sierra Leone",
        caption = "Last Refreshed"
  ) +
  theme_bw()

ggplotly(saloneNewCases)

plotly_example("shiny", "event_data")

#salonePlot <- plot_ly(salone, x = ~Date_reported, y =~New_cases,type = "bar", name = "Sierra Leone")
#salonePlot <- layout(salonePlot, xaxis = x_axis, yaxis = y_axis)
#salonePlot
# define UI

 ui <- ui <- dashboardPage(
    dashboardHeader(title = "Salone Against COVID"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("News", tabName = "newspaper", icon = icon("newspaper-o")), 
          menuItem("FAQs", tabName = "question", icon = icon("question-circle-o")), 
           menuItem("Resources", tabName = "calendar", icon = icon("calendar"))
      )
    ),
    dashboardBody(
      fluidRow(
        box(plotlyOutput("plot"))
      ),
      fluidRow(
        plotlyOutput(verbatimTextOutput("hover"))
      ),
      fluidRow(
        box(plotlyOutput("plot1"))
      )
    )
  )
 
 

ui <- fluidPage(
  radioButtons("plotType", "Plot Type:", choices = c("ggplotly", "plotly")),
  plotlyOutput("plot"),
  verbatimTextOutput("hover"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brushing"),
  verbatimTextOutput("selecting"),
  verbatimTextOutput("brushed"),
  verbatimTextOutput("selected")
)

 
 # Define server function  
 server <- function(input, output) {
   
   WHO_COVID_19_global_data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") 
   
   # select Salone data
   salone <- WHO_COVID_19_global_data %>%
     filter(Country == 'Sierra Leone') %>%
     filter(Date_reported > ymd(20200331))
   
   output$plot <- renderPlotly({
     salonePlot <- plot_ly(salone, x = ~Date_reported, y =~salone$New,type = "bar", name = "Sierra Leone")
     salonePlot %>% 
     layout(dragmode = "select") %>%
       event_register("plotly_selecting")
    salonePlot
   })
 } # server
 
 
 # Create Shiny object
 shinyApp(ui = ui, server = server)

