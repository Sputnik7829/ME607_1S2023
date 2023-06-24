install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(tsibble)
library(fpp3)

end_date = (today() - 1) #Yesterday
begin_date = "2022-11-04" #Has no records before

url <- paste0("https://api.open-meteo.com/v1/forecast?latitude=-22.91&longitude=-47.06&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,windspeed_10m_max,shortwave_radiation_sum&current_weather=true&forecast_days=1&start_date=",begin_date,"&end_date=",end_date,"&timezone=America%2FSao_Paulo")

resposta <- GET(url)

dados_json <- content(resposta, as = "text")
dados <- fromJSON(dados_json)
df <- as.data.frame(dados)
tsdf = df %>% 
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


tsdf %>% 
  pivot_longer(c(temp_max,
                 preciptation,
                 windspeed,
                 sun_radiation), names_to = "var", values_to = "value") %>% 
  ggplot(aes(x = time, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y")+
  labs(title = "Max Temperature over Campinas Region",
       y = "Daily Max Temperature",
       x = "Date")

op = par(mfrow = c(1,2))
acf(tsdf$temp_max)
pacf(tsdf$temp_max)
par(op)

op = par(mfrow = c(1,2))
acf(tsdf$preciptation)
pacf(tsdf$preciptation)
par(op)

op = par(mfrow = c(1,2))
acf(tsdf$windspeed)
pacf(tsdf$windspeed)
par(op)

op = par(mfrow = c(1,2))
acf(tsdf$sun_radiation)
pacf(tsdf$sun_radiation)
par(op)


#AIC and BIC bigger than the exogenous model.

fit_01 <- tsdf %>% 
  model(ARIMA(temp_max))  #Automatic best fit for pdq() and PDQ() parameters
report(fit_01)

#Stationary test for exogenous variables

tseries::adf.test(tsdf$preciptation)
tseries::adf.test(tsdf$windspeed)
tseries::adf.test(tsdf$sun_radiation)

#Serie with sun radiation as only exogenous variable

fit_02 <- tsdf |>
  model(ARIMA(temp_max ~ sun_radiation))  
report(fit_02)

fit_02 |> gg_tsresiduals()
Box.test(augment(fit_02)$.innov, lag = 7, fitdf = 5)

#Serie with wind speed and precipitation as exogenous variables

fit_03 <- tsdf |>
  model(ARIMA(temp_max ~ windspeed + preciptation))  
report(fit_03)

fit_03 |> gg_tsresiduals()
Box.test(augment(fit_03)$.innov, lag = 7, fitdf = 5)

#Serie with wind speed, precipitation and sun radiation as exogenous variables

fit_04 <- tsdf |>
  model(ARIMA(temp_max ~ windspeed +
                preciptation +
                sun_radiation))  
report(fit_04)

fit_04 |> gg_tsresiduals()
Box.test(augment(fit_04)$.innov, lag = 7, fitdf = 5)

tsdf_future <- new_data(tsdf, 8) |>
  mutate(temp_max = mean(tsdf$temp_max),
         preciptation = mean(tsdf$preciptation),
         sun_radiation = mean(tsdf$sun_radiation),
         windspeed= mean(tsdf$windspeed))
forecast(fit_02, new_data = tsdf_future) |>
  autoplot(tsdf) +
  labs(y = "Percentage change")


#Cross Validation

tsdf[1:(length(tsdf$time)-11),]

metrics = NULL


for(i in 0:13){
  df = tsdf[1:(length(tsdf$time)-14+i),]
  model1 = df %>% 
    model(ARIMA(temp_max ~ sun_radiation + pdq(0,1,4)+ PDQ(2,0,0)))
  model2 = df %>% 
    model(ARIMA(temp_max ~ windspeed + preciptation + pdq(3,1,1)+ PDQ(1,0,0)))
  model3 = df %>% 
    model(ARIMA(temp_max ~ windspeed + preciptation + sun_radiation + pdq(3,0,2)+ PDQ(1,0,0)))
  
  df_future = tsdf[(length(tsdf$time)-13+i):(length(tsdf$time)),1:2]
  df_future = df_future %>% 
    mutate(preciptation = mean(df$preciptation),
           sun_radiation = mean(df$sun_radiation),
           windspeed= mean(df$windspeed))
  
  forecast1 = forecast(model1, new_data = df_future)
  forecast2 = forecast(model2, new_data = df_future)
  forecast3 = forecast(model3, new_data = df_future)
  
  df_test = tsdf[(length(tsdf$time)-13+i):(length(tsdf$time)),1:2]
  df_test = df_test %>% 
    mutate(predict1 = forecast1$.mean,
           predict2 = forecast2$.mean,
           predict3 = forecast3$.mean)
  
  daily_eqm1 = ((df_test$temp_max - df_test$predict1)^2)[1]
  daily_eqm2 = ((df_test$temp_max - df_test$predict2)^2)[1]
  daily_eqm3 = ((df_test$temp_max - df_test$predict3)^2)[1]
  
  eqm1 = (sum(df_test$temp_max - df_test$predict1)^2)/nrow(df_test)
  eqm2 = (sum(df_test$temp_max - df_test$predict2)^2)/nrow(df_test)
  eqm3 = (sum(df_test$temp_max - df_test$predict3)^2)/nrow(df_test)
  
  metrics_new = data.frame(window = (14 - i),
                           eqm1 = eqm1,
                           eqm2 = eqm2,
                           eqm3 = eqm3,
                           daily_eqm1 = daily_eqm1,
                           daily_eqm2 = daily_eqm2,
                           daily_eqm3 = daily_eqm3)
  
  metrics = rbind(metrics,metrics_new)
}

metrics %>% 
  select(1:4) %>% 
  pivot_longer(names_to = "eqm",cols = -1) %>%
  mutate(eqm = case_when(eqm=="eqm1"~"Model 1",
                         eqm=="eqm2"~"Model 2",
                         eqm=="eqm3"~"Model 3")) %>% 
  ggplot(aes(x = window, y = sqrt(value), color = eqm))+
  geom_line(size = 0.8)+
  scale_color_manual(values = c("darkblue", "red","darkgreen"))+
  scale_x_reverse()+
  labs(x = "Window", y = "RMSE", color = "Proposed models")+
  theme_bw()+
  theme(legend.position = "bottom")

metrics %>% 
  select(c(1,5,6,7))%>% 
  pivot_longer(names_to = "daily_eqm",cols = -1)%>%
  mutate(daily_eqm = case_when(daily_eqm=="daily_eqm1"~"Model 1",
                         daily_eqm=="daily_eqm2"~"Model 2",
                         daily_eqm=="daily_eqm3"~"Model 3")) %>%
  ggplot(aes(x = window, y = sqrt(value), color = daily_eqm))+
  geom_line(size = 0.8)+
  scale_color_manual(values = c("darkblue", "red","darkgreen"))+
  scale_x_reverse()+
  labs(x = "Window", y = "RMSE", color = "Proposed Models")+
  theme_bw()+
  theme(legend.position = "bottom")

# Model 2 looks better predict

