### Installing Packages ###
#install.packages("shiny")
install.packages("kableExtra")
install.packages("knitr")
### Reading Packages ###
library(shiny)
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
fit01 <- readRDS('Modelos/fit01.RDs')
fit02 <- readRDS('Modelos/fit02.RDs')
fit03 <- readRDS('Modelos/fit03.RDs')
fit04 <- readRDS('Modelos/fit04.RDs')
DailyEQM <- readRDS('Modelos/DailyEQM.RDS')
DailyEQMAlt <- readRDS('Modelos/DailyEQMAlt.RDS')

# Forecasting 
forecast <- readRDS("Forecast.RDS")
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
                   tabPanel("temp_max",
                            plotOutput("MaxTemperature")),
                   tabPanel("Sun_Radiation",
                            plotOutput("SunRad")),
                   tabPanel("WindSpeed",
                            plotOutput("WindSpeed")),
                   tabPanel("Precipitation",
                            plotOutput("Precipitation"))
                 )
               )
             )),
    
    tabPanel("Modelling",
               mainPanel(
                 tabsetPanel(
                   tabPanel("Model 0",splitLayout(
                     verbatimTextOutput("fit01"),
                     plotOutput("fit01_resid")
                   )),
                   tabPanel("Model 1",splitLayout(
                     verbatimTextOutput("fit02"),
                     plotOutput("fit02_resid")
                   )),
                   tabPanel("Model 2",splitLayout(
                     verbatimTextOutput("fit03"),
                     plotOutput("fit03_resid")
                   )),
                   tabPanel("Model 3",splitLayout(
                     verbatimTextOutput("fit04"),
                     plotOutput("fit04_resid")
                   ))),
                plotOutput("DailyEQM"),
                plotOutput("DailyEQMAlt")
             )),
    tabPanel("Prediction",
              mainPanel(
                verbatimTextOutput("Predict"),
                tableOutput("Previsao")
                
              ))
  )
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
  output$fit01_resid <- renderPlot({fit01 |> gg_tsresiduals()})
  output$fit02 <- renderPrint({report(fit02)})
  output$fit02_resid <- renderPlot({fit02 |> gg_tsresiduals()})
  output$fit03 <- renderPrint({report(fit03)})
  output$fit03_resid <- renderPlot({fit03 |> gg_tsresiduals()})
  output$fit04 <- renderPrint({report(fit04)})
  output$fit04_resid <- renderPlot({fit04 |> gg_tsresiduals()})
  output$DailyEQM <- renderPlot({DailyEQM})
  output$DailyEQMAlt <- renderPlot({DailyEQMAlt})
  output$Predict <- renderPrint({paste0(round(forecast$Today,1),"ºC")})
  output$Previsao <- renderTable({knitr::kable(forecast)})
}

# Execução do aplicativo Shiny
shinyApp(ui = ui, server = server)

