#!/usr/bin/Rscript
### Installing Packages ###
install.packages("shiny")
install.packages("shinythemes")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("tsibble")
install.packages("fpp3")
install.packages("forecast")
install.packages("ggpubr")
install.packages("kableExtra")
install.packages("knitr")
### Reading Packages ###
library(shiny)
library(shinythemes)
library(lubridate)
library(tidyverse)
library(tsibble)
library(fpp3)
library(forecast)
library(ggpubr)
library(kableExtra)
##############################################
###  Reading Objects ### 
#EDA
originalTS <- readRDS('Images/OriginalTS.RDS')
Max_Temp <- readRDS('Images/MaxTemp.RDS')
Precip <- readRDS('Images/Precipitation.RDS')
Wind <- readRDS('Images/WindSpeed.RDS')
SunRad <- readRDS('Images/Sun_Radiation.RDS')

#Modelling
fit01 <- readRDS('Modelos/fit01.RDS')
fit01_box <- readRDS('Modelos/fit01_box.RDS')
fit02 <- readRDS('Modelos/fit02.RDs')
fit02_box <- readRDS('Modelos/fit02_box.RDS')
fit03 <- readRDS('Modelos/fit03.RDs')
fit03_box <- readRDS('Modelos/fit03_box.RDS')
fit04 <- readRDS('Modelos/fit04.RDs')
fit04_box <- readRDS('Modelos/fit04_box.RDS')
DailyEQM <- readRDS('Modelos/DailyEQM.RDS')
DailyEQMAlt <- readRDS('Modelos/DailyEQMAlt.RDS')

# Forecasting 
forecast <- readRDS("Forecast.RDS")
forecast_plot <- readRDS("Forecast_plot.RDS")
##############################################

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
  output$time_series_plot <- renderPlot({originalTS})
  output$MaxTemperature <- renderPlot({Max_Temp})
  output$SunRad <- renderPlot({SunRad})
  output$WindSpeed <- renderPlot({Wind})
  output$Precipitation <- renderPlot({Precip})
  output$ACFs_PAcFs <- renderPrint({paste('sexo')})
  
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
    forecast %>%
      filter(Days == "Today") %>% 
      mutate(predictions = paste0(round(predictions,1),"ºC")) %>% 
      pivot_wider(names_from = Days, values_from = predictions) %>% 
      knitr::kable("html") %>% 
      kable_styling("striped", full_width = F) %>% 
      row_spec(which(forecast$Days == "Today"), bold = T, color = "white", background = "red")
  }
  output$Previsao <- function(){
    forecast %>%
      filter(Days != "Today") %>% 
      mutate(predictions = paste0(round(predictions,1),"ºC")) %>% 
      pivot_wider(names_from = Days, values_from = predictions) %>% 
      knitr::kable("html") %>% 
      kable_styling("striped", full_width = F) %>%
      column_spec(1, color = "black", background ="lightblue")
      #row_spec(which(forecast$Days == "Tomorrow"), bold = T, color = "white", background = "orange") 
  }
  output$fore_plot <- renderPlot({forecast_plot})
}

# Execução do aplicativo Shiny
#shinyApp(ui = ui, server = server)

