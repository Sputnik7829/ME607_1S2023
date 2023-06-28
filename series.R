#!/usr/bin/Rscript
install.packages("ggpubr")
install.packages("jsonlite")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("tsibble")
install.packages("fpp3")
install.packages("fable")
install.packages('urca')
install.packages("forecast")
install.packages("httr")

library(httr)
library(ggpubr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(tsibble)
library(fpp3)
library(fable)

end_date = (today() - 1) #Yesterday
begin_date = "2022-11-04" #Has no records before

url <- paste0("https://api.open-meteo.com/v1/forecast?latitude=-22.91&longitude=-47.06&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,windspeed_10m_max,shortwave_radiation_sum&current_weather=true&forecast_days=1&start_date=",begin_date,"&end_date=",end_date,"&timezone=America%2FSao_Paulo")

resposta <- GET(url)

dados_json <- content(resposta, as = "text")
dados <- fromJSON(dados_json)
data_api <- as.data.frame(dados)
tsdf = data_api %>% 
  mutate(time = as.Date(daily.time)) %>% 
  rename(temp_max = daily.temperature_2m_max,
         temp_min = daily.temperature_2m_min,
         preciptation = daily.precipitation_sum,
         windspeed = daily.windspeed_10m_max,
         sun_radiation = daily.shortwave_radiation_sum
  ) %>% 
  select(time,
         temp_max,
         temp_min,
         preciptation,
         windspeed,
         sun_radiation) %>% 
  as_tsibble()

Original_maxtemp = tsdf %>% 
  ggplot(aes(x = time, y = temp_max)) +
  geom_line(color = "red") +
  labs(title = "Max temperature over Campinas city",
       y = "Temperature in Cº",
       x = "Date")+
  scale_x_date(date_labels = "%b/%y", date_breaks = "1 month")+
  theme_bw()
saveRDS(Original_maxtemp,'original_maxtemp.RDS')

Original_prec = tsdf %>% 
  ggplot(aes(x = time, y = preciptation)) +
  geom_line(color = "green") +
  labs(title = "Total Precipitation over Campinas city",
       y = "Precipitation in mm",
       x = "Date")+
  scale_x_date(date_labels = "%b/%y", date_breaks = "1 month")+
  theme_bw()
saveRDS(Original_prec,'original_prec.RDS')

Original_wind = tsdf %>% 
  ggplot(aes(x = time, y = windspeed)) +
  geom_line(color = "blue") +
  labs(title = "Total wind speed at 10m over Campinas city",
       y = "Wind speed in km/h",
       x = "Date")+
  scale_x_date(date_labels = "%b/%y", date_breaks = "1 month")+
  theme_bw()
saveRDS(Original_wind,'original_wind.RDS')

Original_sunrad = tsdf %>% 
  ggplot(aes(x = time, y = sun_radiation)) +
  geom_line(color = "darkorange") +
  labs(title = "The sum of solar radiation over Campinas city",
       y = "Solar radiation in MJ/m²",
       x = "Date")+
  scale_x_date(date_labels = "%b/%y", date_breaks = "1 month")+
  theme_bw()
saveRDS(Original_sunrad,'original_sunrad.RDS')


acf <- forecast::ggAcf(tsdf$temp_max) +
  labs(title = "Max Temperature Time Series ACF") +
  theme_bw()
pacf <- forecast::ggPacf(tsdf$temp_max) +
  labs(title = "Max Temperature Time Series Partial ACF") +
  theme_bw()
saveRDS(ggarrange(acf,pacf, ncol = 2),'maxtemp.RDS')

acf <- forecast::ggAcf(tsdf$preciptation) +
  labs(title = "Preciptation Time Series ACF") +
  theme_bw()
pacf <- forecast::ggPacf(tsdf$preciptation) +
  labs(title = "Preciptation Time Series PACF") +
  theme_bw()
saveRDS(ggarrange(acf,pacf, ncol = 2),'precipitation.RDS')

acf <- forecast::ggAcf(tsdf$windspeed) +
  labs(title = "Wind Speed Time Series ACF") +
  theme_bw()
pacf <- forecast::ggPacf(tsdf$windspeed) +
  labs(title = "Wind Speed Time Series PACF") +
  theme_bw() 
saveRDS(ggarrange(acf,pacf, ncol = 2), 'windspeed.RDS')

acf <- forecast::ggAcf(tsdf$sun_radiation) +
  labs(title = "Radiation Time Series ACF") +
  theme_bw() 

pacf <- forecast::ggPacf(tsdf$sun_radiation) +
  labs(title = "Radiation Time Series PACF") +
  theme_bw() 
saveRDS(ggarrange(acf,pacf, ncol = 2),'sunradiation.RDS')

#Stationary test for exogenous variables
saveRDS(tseries::kpss.test(tsdf$temp_max), 'maxtemp_kpss.RDS')
saveRDS(tseries::adf.test(tsdf$temp_max), 'maxtemp_adf.RDS')

saveRDS(tseries::kpss.test(tsdf$preciptation), 'preciptation_kpss.RDS')
saveRDS(tseries::adf.test(tsdf$preciptation), 'preciptation_adf.RDS')

saveRDS(tseries::kpss.test(tsdf$windspeed), 'windspeed_kpss.RDS')
saveRDS(tseries::adf.test(tsdf$windspeed), 'windspeed_adf.RDS')

saveRDS(tseries::kpss.test(tsdf$sun_radiation), 'sun_radiation_kpss.RDS')
saveRDS(tseries::adf.test(tsdf$sun_radiation), 'sun_radiation_adf.RDS')

#Diffencial in time series

tsdf_diff = tsdf %>% mutate_if(is.numeric, difference) %>% drop_na()

acf <- forecast::ggAcf(tsdf_diff$temp_max) +
  labs(title = "Max Temperature diff Time Series ACF") +
  theme_bw()
pacf <- forecast::ggPacf(tsdf_diff$temp_max) +
  labs(title = "Max Temperature diff Time Series Partial ACF") +
  theme_bw()
saveRDS(ggarrange(acf,pacf, ncol = 2),'maxtemp_diff.RDS')

acf <- forecast::ggAcf(tsdf_diff$preciptation) +
  labs(title = "Precipitation diff Time Series ACF") +
  theme_bw()
pacf <- forecast::ggPacf(tsdf_diff$preciptation) +
  labs(title = "Precipitation diff Time Series Partial ACF") +
  theme_bw()
saveRDS(ggarrange(acf,pacf, ncol = 2),'preciptation_diff.RDS')

acf <- forecast::ggAcf(tsdf_diff$windspeed) +
  labs(title = "Wind speed diff Time Series ACF") +
  theme_bw()
pacf <- forecast::ggPacf(tsdf_diff$windspeed) +
  labs(title = "Wind speed diff Time Series Partial ACF") +
  theme_bw()
saveRDS(ggarrange(acf,pacf, ncol = 2),'windspeed_diff.RDS')

acf <- forecast::ggAcf(tsdf_diff$sun_radiation) +
  labs(title = "Sun Radiation diff Time Series ACF") +
  theme_bw()
pacf <- forecast::ggPacf(tsdf_diff$sun_radiation) +
  labs(title = "Sun Radiation diff Time Series Partial ACF") +
  theme_bw()
saveRDS(ggarrange(acf,pacf, ncol = 2),'sunradiation_diff.RDS')

#Stationary test for exogenous variables
saveRDS(tseries::kpss.test(tsdf_diff$temp_max), 'maxtemp_kpss_diff.RDS')
saveRDS(tseries::adf.test(tsdf_diff$temp_max), 'maxtemp_adf_diff.RDS')

saveRDS(tseries::kpss.test(tsdf_diff$preciptation), 'preciptation_kpss_diff.RDS')
saveRDS(tseries::adf.test(tsdf_diff$preciptation), 'preciptation_adf_diff.RDS')

saveRDS(tseries::kpss.test(tsdf_diff$windspeed), 'windspeed_kpss_diff.RDS')
saveRDS(tseries::adf.test(tsdf_diff$windspeed), 'windspeed_adf_diff.RDS')

saveRDS(tseries::kpss.test(tsdf_diff$sun_radiation), 'sun_radiation_kpss_diff.RDS')
saveRDS(tseries::adf.test(tsdf_diff$sun_radiation), 'sun_radiation_adf_diff.RDS')

#Serie with sun radiation as only exogenous variable

fit_01 <- tsdf %>% 
  model(ARIMA(temp_max))  #Automatic best fit for pdq() and PDQ() parameters
saveRDS(fit_01, 'fit01.RDS')

fit1_params = fit_01 %>% mutate(map_dfr(`ARIMA(temp_max)`, c("fit", "spec")))

fit_01 |> gg_tsresiduals()
saveRDS(Box.test(augment(fit_01)$.innov, lag = 7, fitdf = 5),
        'fit01box.RDS')

fit_02 <- tsdf |>
  model(ARIMA(temp_max ~ sun_radiation))  
saveRDS(fit_02, 'fit02.RDS')

fit2_params = fit_02 %>% mutate(map_dfr(`ARIMA(temp_max ~ sun_radiation)`, c("fit", "spec")))

fit_02 |> gg_tsresiduals()
saveRDS(Box.test(augment(fit_02)$.innov, lag = 7, fitdf = 5),
        'fit02box.RDS')

#Serie with wind speed and precipitation as exogenous variables

fit_03 <- tsdf |>
  model(ARIMA(temp_max ~ windspeed + preciptation))  
saveRDS(fit_03, 'fit03.RDS')

fit3_params = fit_03 %>% mutate(map_dfr(`ARIMA(temp_max ~ windspeed + preciptation)`, c("fit", "spec")))

fit_03 |> gg_tsresiduals()
saveRDS(Box.test(augment(fit_03)$.innov, lag = 7, fitdf = 5),
        'fit03box.RDS')

#Serie with wind speed, precipitation and sun radiation as exogenous variables

fit_04 <- tsdf_diff |>
  model(ARIMA(temp_max ~ windspeed +
                preciptation +
                sun_radiation))  
saveRDS(fit_04, 'fit04.RDS')

fit4_params = fit_04 %>% mutate(map_dfr(`ARIMA(temp_max ~ windspeed + preciptation + sun_radiation)`, c("fit", "spec")))

fit_04 |> gg_tsresiduals()
saveRDS(Box.test(augment(fit_04)$.innov, lag = 7, fitdf = 5),
        'fit04box.RDS')


#Cross Validation

metrics = NULL

for(i in 0:13){
  df = tsdf[1:(length(tsdf$time)-14+i),]
  model0 = df %>% 
    model(ARIMA(temp_max ~ pdq(fit1_params$p,
                               fit1_params$d,
                               fit1_params$q) +
                  PDQ(fit1_params$P,
                      fit1_params$D,
                      fit1_params$Q)))
  model1 = df %>% 
    model(ARIMA(temp_max ~ sun_radiation +
                  pdq(fit2_params$p,
                      fit2_params$d,
                      fit2_params$q) +
                  PDQ(fit2_params$P,
                      fit2_params$D,
                      fit2_params$Q)))
  model2 = df %>% 
    model(ARIMA(temp_max ~ windspeed +
                  preciptation +
                  pdq(fit3_params$p,
                      fit3_params$d,
                      fit3_params$q) +
                  PDQ(fit3_params$P,
                      fit3_params$D,
                      fit3_params$Q)))
  model3 = df %>% 
    model(ARIMA(temp_max ~ windspeed +
                  preciptation +
                  sun_radiation +
                  pdq(fit4_params$p,
                      fit4_params$d,
                      fit4_params$q) +
                  PDQ(fit4_params$P,
                      fit4_params$D,
                      fit4_params$Q)))
  
  df_future = tsdf[(length(tsdf$time)-13+i):(length(tsdf$time)),1:2]
  df_future = df_future %>% 
    mutate(preciptation = mean(df$preciptation),
           sun_radiation = mean(df$sun_radiation),
           windspeed= mean(df$windspeed))
  
  forecast0 = forecast(model0, new_data = df_future)
  forecast1 = forecast(model1, new_data = df_future)
  forecast2 = forecast(model2, new_data = df_future)
  forecast3 = forecast(model3, new_data = df_future)
  
  df_test = tsdf[(length(tsdf$time)-13+i):(length(tsdf$time)),1:2]
  df_test = df_test %>% 
    mutate(predict0 = forecast0$.mean,
           predict1 = forecast1$.mean,
           predict2 = forecast2$.mean,
           predict3 = forecast3$.mean)
  
  daily_eqm0 = ((df_test$temp_max - df_test$predict0)^2)[1]
  daily_eqm1 = ((df_test$temp_max - df_test$predict1)^2)[1]
  daily_eqm2 = ((df_test$temp_max - df_test$predict2)^2)[1]
  daily_eqm3 = ((df_test$temp_max - df_test$predict3)^2)[1]
  
  metrics_new = data.frame(window = (14 - i),
                           daily_eqm0 = daily_eqm0,
                           daily_eqm1 = daily_eqm1,
                           daily_eqm2 = daily_eqm2,
                           daily_eqm3 = daily_eqm3)
  
  metrics = rbind(metrics,metrics_new)
}


dailyEQM <- metrics %>% 
  pivot_longer(names_to = "daily_eqm",cols = -1)%>%
  mutate(daily_eqm = case_when(daily_eqm=="daily_eqm1"~"Model 1",
                               daily_eqm=="daily_eqm2"~"Model 2",
                               daily_eqm=="daily_eqm3"~"Model 3",
                               daily_eqm=="daily_eqm0"~"Model 0")) %>%
  ggplot(aes(x = window, y = sqrt(value), color = daily_eqm))+
  geom_line(linewidth = 0.8)+
  scale_color_manual(values = c("darkblue", "red","darkgreen","purple"))+
  scale_x_reverse()+
  labs(x = "Window", y = "RMSE", color = "Proposed Models")+
  theme_bw()+
  theme(legend.position = "bottom")
saveRDS(dailyEQM,'dailyeqm.RDS')

# Model 2 or Model 0 looks better to predict

metrics %>% 
  summarise(mean_daily_model0 = mean(daily_eqm0),
            mean_daily_model1 = mean(daily_eqm1),
            mean_daily_model2 = mean(daily_eqm2),
            mean_daily_model3 = mean(daily_eqm3))



metrics_alt = NULL

for(i in 0:20){
  
  df = tsdf[1:(length(tsdf$time)-28+i),]
  model0 = df %>% 
    model(ARIMA(temp_max ~ pdq(fit1_params$p,
                               fit1_params$d,
                               fit1_params$q) +
                  PDQ(fit1_params$P,
                      fit1_params$D,
                      fit1_params$Q)))
  model1 = df %>% 
    model(ARIMA(temp_max ~ sun_radiation +
                  pdq(fit2_params$p,
                      fit2_params$d,
                      fit2_params$q) +
                  PDQ(fit2_params$P,
                      fit2_params$D,
                      fit2_params$Q)))
  model2 = df %>% 
    model(ARIMA(temp_max ~ windspeed +
                  preciptation +
                  pdq(fit3_params$p,
                      fit3_params$d,
                      fit3_params$q) +
                  PDQ(fit3_params$P,
                      fit3_params$D,
                      fit3_params$Q)))
  model3 = df %>% 
    model(ARIMA(temp_max ~ windspeed +
                  preciptation +
                  sun_radiation +
                  pdq(fit4_params$p,
                      fit4_params$d,
                      fit4_params$q) +
                  PDQ(fit4_params$P,
                      fit4_params$D,
                      fit4_params$Q)))
  
  df_future = tsdf[(length(tsdf$time)-27+i):(length(tsdf$time)-21+i),1:2]
  df_future = df_future %>% 
    mutate(preciptation = mean(df$preciptation),
           sun_radiation = mean(df$sun_radiation),
           windspeed= mean(df$windspeed))
  
  forecast0 = forecast(model0, new_data = df_future)
  forecast1 = forecast(model1, new_data = df_future)
  forecast2 = forecast(model2, new_data = df_future)
  forecast3 = forecast(model3, new_data = df_future)
  
  df_test = tsdf[(length(tsdf$time)-27+i):(length(tsdf$time)-21+i),1:2]
  df_test = df_test %>% 
    mutate(predict0 = forecast0$.mean,
           predict1 = forecast1$.mean,
           predict2 = forecast2$.mean,
           predict3 = forecast3$.mean)
  
  eqm0 = (sum(df_test$temp_max - df_test$predict0)^2)/nrow(df_test)
  eqm1 = (sum(df_test$temp_max - df_test$predict1)^2)/nrow(df_test)
  eqm2 = (sum(df_test$temp_max - df_test$predict2)^2)/nrow(df_test)
  eqm3 = (sum(df_test$temp_max - df_test$predict3)^2)/nrow(df_test)
  
  metrics_new = data.frame(window = (21 - i),
                           eqm0 = eqm0,
                           eqm1 = eqm1,
                           eqm2 = eqm2,
                           eqm3 = eqm3)
  
  metrics_alt = rbind(metrics_alt,metrics_new)
}

DailyEQMAlt <- metrics_alt %>% 
  pivot_longer(names_to = "eqm",cols = -1) %>%
  mutate(eqm = case_when(eqm=="eqm1"~"Model 1",
                         eqm=="eqm2"~"Model 2",
                         eqm=="eqm3"~"Model 3",
                         eqm=="eqm0"~"Model 0")) %>% 
  ggplot(aes(x = window, y = sqrt(value), color = eqm))+
  geom_line(linewidth = 0.8)+
  scale_color_manual(values = c("darkblue", "red","darkgreen","purple"))+
  scale_x_reverse()+
  labs(x = "Window", y = "RMSE", color = "Proposed models")+
  theme_bw()+
  theme(legend.position = "bottom")
saveRDS(DailyEQMAlt,'dailyeqmalt.RDS')
# Model 2 is the winner

metrics_alt %>% 
  summarise(mean_eqm_model0 = mean(eqm0),
            mean_eqm_model1 = mean(eqm1),
            mean_eqm_model2 = mean(eqm2),
            mean_eqm_model3 = mean(eqm3))


# Forecasting for Model 2
tsdf_future <- new_data(tsdf, 7) |>
  mutate(preciptation = mean(tsdf$preciptation),
         sun_radiation = mean(tsdf$sun_radiation),
         windspeed= mean(tsdf$windspeed))

fore <- forecast(fit_02, new_data = tsdf_future)
forecast_plot <- fore |>
  autoplot(tsdf) +
  labs(y = "Percentage change")
saveRDS(forecast_plot,"forecastplot.RDS")
fore <- data.frame("Days" = c(0:6), Date = fore$time,"predictions" = fore$.mean)
fore <- fore %>% mutate(Days = ifelse(Days == 0,
                                      "Today",
                                      ifelse(Days == 1,
                                             "Tomorrow",
                                             paste0("Next ", Days, " days"))),
                        predictions = paste0(round(predictions,2),"Cº"))
saveRDS(fore,"forecast.RDS")
write_csv(fore, "max_temp_predictions.csv")
