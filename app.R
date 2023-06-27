#!/usr/bin/Rscript
Sys.setlocale(category = "LC_ALL", locale = "pt_BR.UTF-8")

library(shiny)
library(shinythemes)
library(lubridate)
library(tidyverse)
library(tsibble)
library(fpp3)
library(forecast)
library(ggpubr)
library(kableExtra)


###  Reading Objects ### 
#EDA
originalts <- readRDS('originalts.RDS')
Max_Temp <- readRDS('maxtemp.RDS')
Precip <- readRDS('precipitation.RDS')
Wind <- readRDS('windspeed.RDS')
SunRad <- readRDS('sunradiation.RDS')

#Modelling
fit01 <- readRDS('fit01.RDS')
fit01_box <- readRDS('fit01box.RDS')
fit02 <- readRDS('fit02.RDS')
fit02_box <- readRDS('fit02box.RDS')
fit03 <- readRDS('fit03.RDS')
fit03_box <- readRDS('fit03box.RDS')
fit04 <- readRDS('fit04.RDS')
fit04_box <- readRDS('fit04box.RDS')
DailyEQM <- readRDS('dailyeqm.RDS')
DailyEQMAlt <- readRDS('dailyeqmalt.RDS')

# Forecasting 
forecast <- readRDS("forecast.RDS")
forecast_plot <- readRDS("forecastplot.RDS")


# Definição da interface do usuário
ui <- fluidPage(
  titlePanel("Dashboard de Séries Temporais"),
  
  #Definindo as páginas do dashboard
  tabsetPanel(
    tabPanel("EDA",
             sidebarLayout(
               sidebarPanel(
                 verbatimTextOutput("ACFs_PAcFs")
               ),
               
               mainPanel(
                 plotOutput("time_series_plot"),
                 tabsetPanel(
                   tabPanel("Max Temperature",
                            plotOutput("MaxTemperature")),
                   tabPanel("Sun Radiation",
                            plotOutput("SunRad")),
                   tabPanel("Wind Speed",
                            plotOutput("WindSpeed")),
                   tabPanel("Precipitation",
                            plotOutput("Precipitation"))
                 )
               )
             )),
    tabPanel("Proposed Models",
             mainPanel(
               tabsetPanel(
                 tabPanel("Model 0",splitLayout(
                   verbatimTextOutput("fit01"),
                   verbatimTextOutput("fit01_box")
                 ),
                 plotOutput("fit01_resid")),
                 tabPanel("Model 1",splitLayout(
                   verbatimTextOutput("fit02"),
                   verbatimTextOutput("fit02_box")
                 ), plotOutput("fit02_resid")),
                 tabPanel("Model 2",splitLayout(
                   verbatimTextOutput("fit03"),
                   verbatimTextOutput("fit03_box")
                 ), plotOutput("fit03_resid")),
                 tabPanel("Model 3",splitLayout(
                   verbatimTextOutput("fit04"),
                   verbatimTextOutput("fit04_box")
                 ), plotOutput("fit04_resid")))
             )),
    
    tabPanel("Validation",
             mainPanel(
               tabsetPanel(
                 tabPanel("Daily EQM",
                          plotOutput("DailyEQM")),
                 tabPanel("Weekly EQM",
                          plotOutput("DailyEQMAlt"))
               ))),
    tabPanel("Prediction",
             mainPanel(
               tableOutput("Predict"),
               tableOutput("Previsao"),
               plotOutput("fore_plot")
             ))
  ),
  theme = shinytheme("cerulean")
)

# Definição do servidor
server <- function(input, output) {
  output$time_series_plot <- renderPlot({originalts})
  output$MaxTemperature <- renderPlot({Max_Temp})
  output$SunRad <- renderPlot({SunRad})
  output$WindSpeed <- renderPlot({Wind})
  output$Precipitation <- renderPlot({Precip})
  output$ACFs_PAcFs <- renderPrint({paste('sexo sexo2 sexo3')})
  
  output$fit01 <- renderPrint({report(fit01)})
  output$fit01_box <- renderPrint({fit01_box})
  output$fit01_resid <- renderPlot({fit01 |> gg_tsresiduals()})
  output$fit02 <- renderPrint({report(fit02)})
  output$fit02_box <- renderPrint({fit02_box})
  output$fit02_resid <- renderPlot({fit02 |> gg_tsresiduals()})
  output$fit03 <- renderPrint({report(fit03)})
  output$fit03_box <- renderPrint({fit03_box})
  output$fit03_resid <- renderPlot({fit03 |> gg_tsresiduals()})
  output$fit04 <- renderPrint({report(fit04)})
  output$fit04_box <- renderPrint({fit04_box})
  output$fit04_resid <- renderPlot({fit04 |> gg_tsresiduals()})
  output$DailyEQM <- renderPlot({DailyEQM})
  output$DailyEQMAlt <- renderPlot({DailyEQMAlt})
  output$Predict <- function(){
  week_pred = as.data.frame(forecast %>% t()) 
  colnames(week_pred) = week_pred[1,]
  week_pred <- week_pred[-1,]
  week_pred %>% 
    select("Today") %>% 
  knitr::kable("html", col.names = NA, row.names = FALSE) %>% 
  kable_styling("striped", full_width = F) %>% 
    column_spec(1, color = "black", background ="red")
  }
  output$Previsao <- function(){
  week_pred = as.data.frame(forecast %>% t()) 
  colnames(week_pred) = week_pred[1,]
  week_pred <- week_pred[-1,]
  week_pred %>% 
    select(-Today) %>%
  knitr::kable("html",col.names = NA, row.names = FALSE) %>% 
  kable_styling("striped", full_width = F) %>%
  column_spec(1, color = "black", background ="lightblue")
  #row_spec(which(forecast$Days == "Tomorrow"), bold = T, color = "white", background = "orange") 
  }
  output$fore_plot <- renderPlot({forecast_plot})
}

# Execução do aplicativo Shiny
shinyApp(ui = ui, server = server)

#rsconnect::setAccountInfo(name='h172284',
#                          token='2C6F9B54FB985E741B7204754EE2E723',
#                          secret='YeNESbbouskHtZIqBICsAUa5PfEzJiQnH1H8sqJ4')

#rsconnect::deployApp()
