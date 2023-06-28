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
original_maxtemp <- readRDS('original_maxtemp.RDS')
original_precip <- readRDS('original_prec.RDS')
original_windspeed <- readRDS('original_wind.RDS')
original_sunrad <- readRDS('original_sunrad.RDS')
Max_Temp <- readRDS('maxtemp.RDS')
Precip <- readRDS('precipitation.RDS')
Wind <- readRDS('windspeed.RDS')
SunRad <- readRDS('sunradiation.RDS')
## Stationary tests
Max_temp_kss = readRDS('maxtemp_kpss.RDS')
Max_temp_adf = readRDS('maxtemp_adf.RDS')
Windspeed_kss = readRDS('windspeed_kpss.RDS')
Windspeed_adf = readRDS('windspeed_adf.RDS')
Sunrad_kss = readRDS('sun_radiation_kpss.RDS')
Sunrad_adf = readRDS('sun_radiation_adf.RDS')
Precip_kss = readRDS('preciptation_kpss.RDS')
Precip_adf = readRDS('preciptation_adf.RDS')
#Diff series
Max_temp_diff = readRDS('maxtemp_diff.RDS')
Precip_diff = readRDS('preciptation_diff.RDS')
Wind_diff = readRDS('windspeed_diff.RDS')
Sunrad_dif = readRDS('sunradiation_diff.RDS')

##Diff series Stationary tests
Max_temp_kss_diff = readRDS('maxtemp_kpss_diff.RDS')
Max_temp_adf_diff = readRDS('maxtemp_adf_diff.RDS')
Windspeed_kss_diff = readRDS('windspeed_kpss_diff.RDS')
Windspeed_adf_diff = readRDS('windspeed_adf_diff.RDS')
Sunrad_kss_diff = readRDS('sun_radiation_kpss_diff.RDS')
Sunrad_adf_diff = readRDS('sun_radiation_adf_diff.RDS')
Precip_kss_diff = readRDS('preciptation_kpss_diff.RDS')
Precip_adf_diff = readRDS('preciptation_adf_diff.RDS')


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
  titlePanel("Campinas-SP Max Temperature Prediction"),
  
  #Definindo as páginas do dashboard
  tabsetPanel(
    tabPanel("EDA",
             sidebarLayout(
               sidebarPanel(
                 fluidPage(
                   h2("Introduction"),
                   verbatimTextOutput("Intro1"),
                   verbatimTextOutput("Intro2"),
                   verbatimTextOutput("Intro3")
                   )
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Max Temperature",
                            fluidPage(
                              h2("Original time series"),
                                plotOutput("OriginalMaxTemperature"),
                                plotOutput("MaxTemperature"),
                              h2("Stationary tests Original TS"),
                            splitLayout(
                              verbatimTextOutput("max_temp_kss"),
                              verbatimTextOutput("max_temp_adf")),
                            h2("One diff ACF and PACF"),
                              plotOutput("MaxTemperature_diff"),
                            h2("Stationary tests One diff TS"),
                            splitLayout(
                              verbatimTextOutput("max_temp_kss_diff"),
                              verbatimTextOutput("max_temp_adf_diff")))),
                   tabPanel("Sun Radiation",
                            fluidPage(
                              h2("Original time series"),
                              plotOutput("OriginalSunRad"),
                              plotOutput("SunRad"),
                              h2("Stationary tests Original TS"),
                              splitLayout(
                                verbatimTextOutput("sunrad_kss"),
                                verbatimTextOutput("sunrad_adf")),
                              h2("One diff ACF and PACF"),
                                plotOutput("Sunrad_diff"),
                              h2("Stationary tests One diff TS"),
                              splitLayout(
                                verbatimTextOutput("sunrad_kss_diff"),
                                verbatimTextOutput("sunrad_adf_diff")))),
                   tabPanel("Wind Speed",
                            fluidPage(
                              h2("Original time series"),
                              plotOutput("OriginalWindSpeed"),
                              plotOutput("WindSpeed"),
                              h2("Stationary tests Original TS"),
                              splitLayout(
                                verbatimTextOutput("windspeed_kss"),
                                verbatimTextOutput("windspeed_adf")),
                              h2("One diff ACF and PACF"),
                              plotOutput("Windspeed_diff"),
                              h2("Stationary tests One diff TS"),
                              splitLayout(
                                verbatimTextOutput("windspeed_kss_diff"),
                                verbatimTextOutput("windspeed_adf_diff")))),
                   tabPanel("Precipitation",
                            fluidPage(
                              h2("Original time series"),
                              plotOutput("OriginalPrecipitation"),
                              plotOutput("Precipitation"),
                              h2("Stationary tests Original TS"),
                              splitLayout(
                                verbatimTextOutput("precip_kss"),
                                verbatimTextOutput("precip_adf")),
                              h2("One diff ACF and PACF"),
                              plotOutput("Precipitation_diff"),
                              h2("Stationary tests One diff TS"),
                              splitLayout(
                                verbatimTextOutput("precip_kss_diff"),
                                verbatimTextOutput("precip_adf_diff"))))
                 )
               )
             )),
    tabPanel("Proposed Models",
             sidebarPanel(
               fluidPage(
                 h2("Model diagnostics"),
                 verbatimTextOutput("mod1")
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Model 0",
                          fluidPage(
                            h2("Fit Model 0 (Only autoregressive in Max Temperature)"),
                            splitLayout(
                              verbatimTextOutput("fit01"),
                              verbatimTextOutput("fit01_box")),
                            h2("Model 0 Residuals"),
                            plotOutput("fit01_resid"))),
                 tabPanel("Model 1",
                          fluidPage(
                            h2("Fit Model 1 Model with Sun radiation as Exogenous variable"),
                            splitLayout(
                              verbatimTextOutput("fit02"),
                              verbatimTextOutput("fit02_box")),
                            h2("Model 1 Residuals"),
                            plotOutput("fit02_resid"))),
                 tabPanel("Model 2",
                          fluidPage(
                            h2("Fit Model 2 Model with Windspeed and Precipitation as Exogenous variables"),
                            splitLayout(
                              verbatimTextOutput("fit03"),
                              verbatimTextOutput("fit03_box")),
                            h2("Model 2 Residuals"),
                            plotOutput("fit03_resid"))),
                 tabPanel("Model 3",
                          fluidPage(
                          h2("Fit Model 3 Model with all 3 variables as Exogenous variables"),
                          splitLayout(
                   verbatimTextOutput("fit04"),
                   verbatimTextOutput("fit04_box")),
                   h2("Model 3 Residuals"),
                   plotOutput("fit04_resid"))))
             )),
    
    tabPanel("Validation",
             sidebarPanel(
               fluidPage(
                 h2("Model Choice"),
                 verbatimTextOutput("valid1"),
                 verbatimTextOutput("valid2"),
                 verbatimTextOutput("valid3"),
                 verbatimTextOutput("valid4"),
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Daily EQM",
                          fluidPage(
                            h2("EMQ in one day Max temperature prediction over last 14 days"),  
                            plotOutput("DailyEQM"))),
                 tabPanel("Weekly EQM",
                          fluidPage(
                            h2("EMQ of Max temperature prediction over 7 days window in last 4 weeks"),
                            plotOutput("DailyEQMAlt")))
               ))),
    tabPanel("Prediction",
             sidebarPanel(
               fluidPage(
                 h2("Max temperature for next week"),
                 verbatimTextOutput("pred1"),
               )
             ),
             mainPanel(
               fluidPage(
                 h2("The Max temperature predicted for Today is"),
                 tableOutput("Predict"),
                 h2("The Max temperature predicted for next week is"),
                 tableOutput("Previsao"),
                 h2("The Max temperature predicted plot"),
                 plotOutput("fore_plot")
               )))
  ),
  theme = shinytheme("cerulean")
)

# Definição do servidor
server <- function(input, output) {
  output$time_series_plot <- renderPlot({originalts})
  output$OriginalMaxTemperature <- renderPlot({original_maxtemp})
  output$MaxTemperature <- renderPlot({Max_Temp})
  output$max_temp_kss <- renderPrint({Max_temp_kss})
  output$max_temp_adf <- renderPrint({Max_temp_adf})
  output$OriginalSunRad <- renderPlot({original_sunrad})
  output$sunrad_kss <- renderPrint({Sunrad_kss})
  output$sunrad_adf <- renderPrint({Sunrad_adf})
  output$SunRad <- renderPlot({SunRad})
  output$OriginalWindSpeed <- renderPlot({original_windspeed})
  output$windspeed_kss <- renderPrint({Windspeed_kss})
  output$windspeed_adf <- renderPrint({Windspeed_adf})
  output$WindSpeed <- renderPlot({Wind})
  output$OriginalPrecipitation <- renderPlot({original_precip})
  output$precip_kss <- renderPrint({Precip_kss})
  output$precip_adf <- renderPrint({Precip_adf})
  output$Precipitation <- renderPlot({Precip})
  output$MaxTemperature_diff <- renderPlot({Max_temp_diff})
  output$max_temp_kss_diff <- renderPrint({Max_temp_kss_diff})
  output$max_temp_adf_diff <- renderPrint({Max_temp_adf_diff})
  output$Precipitation_diff <- renderPlot({Precip_diff})
  output$precip_kss_diff <- renderPrint({Precip_kss_diff})
  output$precip_adf_diff <- renderPrint({Precip_adf_diff})
  output$Windspeed_diff <- renderPlot({Wind_diff})
  output$windspeed_kss_diff <- renderPrint({Windspeed_kss_diff})
  output$windspeed_adf_diff <- renderPrint({Windspeed_adf_diff})
  output$Sunrad_diff <- renderPlot({Sunrad_dif})
  output$sunrad_kss_diff <- renderPrint({Sunrad_kss_diff})
  output$sunrad_adf_diff <- renderPrint({Sunrad_adf_diff})
  output$Intro1 <- renderText({
  bold_text = paste("It was selected a dataset with climate prevision for Campinas and region, in the period from April 11th, 2022 till up to date. The selected variables were: Max temperature (ºC), precipitation (mm), wind speed (Km/h)  and sun radiation (MJ/m^2). The objective of this study is to find the best model to predict the nest day's max temperature.")
  wrapped_text <- strwrap(bold_text, width = 55)
  paste(wrapped_text, collapse = "\n")
  })
  output$Intro2 <- renderText({
    bold_text = paste("In order to do it, first thing is to plot each time series of all variables to observe its behavior through time, also plotting its autocorrelation (ACF) and partial autocorrelation(PACF) graphs and Dickey-Fuller's augmented test (ADF) and Kwiatkowski-Phillips-Schmidt-Shin's test (KPSS).")
    wrapped_text <- strwrap(bold_text, width = 55)
    paste(wrapped_text, collapse = "\n")
  })
  output$Intro3 <- renderText({
    bold_text = paste("Generally, the time series show similar characterístics, as observed by ADF test wich indicates statinarity of the series, but in contrary to the previous one with KPSS the result is non-stationary. Other factor observed is that all ACFs behave the same way, indicating sazonality.  Considering the KPSS test, to manipulate the data into an stationary non-correlated series, it is only necessary to differentiate it once as observed in the new ACFs and PACFs  graphs with one differential.")
    wrapped_text <- strwrap(bold_text, width = 55)
    paste(wrapped_text, collapse = "\n")
  })
  
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
  output$mod1 <- renderText({
    bold_text = paste("As observed by the tested models and it's respective residual error, the results were that AIC and BIC (metrics to select a model) are very similar between all models. All residual errors have acceptable behavior, near zero and non-correlated, and that's why all histograms distributions are similar to a Normal distribution. As with Box-Pierce's test it indicates evidence for all models that the residual errors are non-correlated.")
    wrapped_text <- strwrap(bold_text, width = 54)
    paste(wrapped_text, collapse = "\n")
  })
  output$valid1 <- renderText({
    bold_text = paste("To validate the best model to predict the max temperature, i.e. the model that makes less mistakes throughout the timeline would be a good candidate. That is why the graph shows the mean error of each model in a timeline.")
    wrapped_text <- strwrap(bold_text, width = 55)
    paste(wrapped_text, collapse = "\n")
  })
  output$valid2 <- renderText({
    bold_text = paste("Basing the choice on the mean error of the models, it's visible that models 0 and 2 are the ones that makes less mistakes, it's  valid to observe that models 1 and 3 have periods of time in which they are better than models 0 and 2, but considering all time, model 2 is the best choice to predict the next max temperature.")
    wrapped_text <- strwrap(bold_text, width = 54)
    paste(wrapped_text, collapse = "\n")
  })
  output$valid3 <- renderText({
    bold_text = paste("Daily EQM means the mean squared error for only one prediction ahead. The x axis in the graf, represents the last n days where we used to validate the models and Y axis represents the EQM on that day.")
    wrapped_text <- strwrap(bold_text, width = 54)
    paste(wrapped_text, collapse = "\n")
  })
  output$valid4 <- renderText({
    bold_text = paste("Weekly EMQ means the mean squared error calculated over seven predictions ahead. The predictions are updated every day, keeping a window of seven days. The x axis in this case, represents one of the this windows of seven days.")
    wrapped_text <- strwrap(bold_text, width = 54)
    paste(wrapped_text, collapse = "\n")
  })
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
  output$pred1 <- renderText({
    bold_text = paste("This page shows the prediction of the model for the next 7 days.")
    wrapped_text <- strwrap(bold_text, width = 55)
    paste(wrapped_text, collapse = "\n")
  })
}

# Execução do aplicativo Shiny
shinyApp(ui = ui, server = server)

#rsconnect::setAccountInfo(name='h172284',
#                          token='2C6F9B54FB985E741B7204754EE2E723',
#                          secret='YeNESbbouskHtZIqBICsAUa5PfEzJiQnH1H8sqJ4')

#rsconnect::deployApp()
