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

#url <- paste0("https://api.open-meteo.com/v1/forecast?latitude=-22.91&longitude=-47.06&hourly=temperature_2m,relativehumidity_2m,precipitation,windspeed_10m,windspeed_80m,windspeed_120m,windspeed_180m,is_day&current_weather=true&forecast_days=1&start_date=",begin_date,"&end_date=",end_date,"&timezone=America%2FSao_Paulo")
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

# AIC e BIC maiores comparados ao modelo exogeno.
fit_01 <- tsdf %>% 
  model(ARIMA(temp_max))  # se não colocarmos pdq(,,) é feita uma escolha automática
report(fit_01)

#Séries exogenas estacionarias
tseries::adf.test(tsdf$preciptation)
tseries::adf.test(tsdf$windspeed)
tseries::adf.test(tsdf$sun_radiation)

#Serie com apenas radiação solar como variavel exogena
fit_02 <- tsdf |>
  model(ARIMA(temp_max ~ sun_radiation))  # se não colocarmos pdq(,,) é feita uma escolha automática
report(fit_02)

fit_02 |> gg_tsresiduals()
Box.test(augment(fit_02)$.innov, lag = 7, fitdf = 5)

#Serie com velocidade do vento e precipitação como variaveis exogenas
fit_03 <- tsdf |>
  model(ARIMA(temp_max ~ windspeed + preciptation))  # se não colocarmos pdq(,,) é feita uma escolha automática
report(fit_03)

fit_03 |> gg_tsresiduals()
Box.test(augment(fit_03)$.innov, lag = 7, fitdf = 5)

#Serie com velocidade do vento, precipitação e radição solar
# como variaveis exogenas
fit_04 <- tsdf |>
  model(ARIMA(temp_max ~ windspeed +
                preciptation +
                sun_radiation))  # se não colocarmos pdq(,,) é feita uma escolha automática
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


