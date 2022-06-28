# Forecasting exchange rates using prophet
## Loading the libraries

library(tidyverse)
library(forecast)
library(readr)
library(prophet)


## Loading the data

data <- read_csv("C:/Users/admin/Desktop/my data/Time series/Time series Project/data.csv") 

attach(data)


new_data=rename(data,ds=Date,y=Mean) # prophet model uses ds and y column

new_data$ds= format(as.Date(new_data$ds, format = "%m/%d/%Y"),"%Y-%m-%d")
       ## changing the date format to YYYY-MM-DD

## Splitting the data

data_sample <- new_data %>%
  filter(ds>= "2018-06",ds<"2022-01")%>%
  select('ds','y')


plot.ts(data_sample$y,main= "USD/KEN exchange rate from 2018-2021")

## creating the prophet model
mdl<- prophet(data_sample)

## Forecasting 30 days into the future

future<- make_future_dataframe(mdl, periods = 30)

fcast<- predict(mdl,future)

tail(fcast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

## Plots of the components

plot(mdl,fcast)

prophet_plot_components(mdl,fcast)
## positive increasing trend apart from 3 time periods, weekends have no 
## effect on the exchange rate fluctuation, each week day has no significance 
## on how exchange rates are affected. up and down yearly fluctuation. There
## is moderate yearly seasonality.

## Automatic changepoint detection for trend

plot(mdl,fcast) + add_changepoints_to_plot(mdl)


## Adjusting trend flexibility at 0.5

mdl_1<- prophet(data_sample,changepoint.prior.scale = 0.5)
fcast_1<- predict(mdl_1,future)
plot(mdl_1,fcast_1)

## suitable model

## Adjusting trend flexibility at 0.001

mdl_2<- prophet(data_sample,changepoint.prior.scale = 0.001)
fcast_2<- predict(mdl_2,future)
plot(mdl_2,fcast_2)
## This model is underfitting the trend. The model is not following general trend but 
## is fitting to seasonality

## Adjusting trend flexibility at 0.99

mdl_3<- prophet(data_sample,changepoint.prior.scale = 0.99)
fcast_3<- predict(mdl_3,future)
plot(mdl_3,fcast_3)
## the model is overfitting the trend


## Adjusting seasonality
### Dealing with yearly seasonality
prophet:::plot_yearly(mdl)
## default fourier order for yearly seasonality is 10. Seasonality needs to 
## fit higher frequency changes and be less smooth

mdl<- prophet(data_sample,yearly.seasonality = 20)
prophet:::plot_yearly(mdl)

## Random seasonality

mdl_4<- prophet(data_sample,seasonality.prior.scale = 0.001)
fcast_4<-predict(mdl_4,future)
plot(mdl_4,fcast_4)
## the model is less affected by seasonality and more closely follows the
## trend of the data

## Using yearly seasonality 

mdl_5<- prophet(weekly.seasonality = F) 
mdl_5<- add_seasonality(mdl_5,name = 'yearly',period= 365,fourier.order = 10)
mdl_5<- fit.prophet(mdl_5,data_sample)

fcast_5<- predict(mdl_5,future)
plot(mdl_5,fcast_5)
## Yearly seasonality is slightly fit onto the data.


mdl_6<- prophet(weekly.seasonality = F) 
mdl_6<- add_seasonality(mdl_6,name = 'yearly',period= 365,fourier.order = 2)
mdl_6<- fit.prophet(mdl_6,data_sample)

fcast_6<- predict(mdl_6,future)
plot(mdl_6,fcast_6)
## Yearly seasonality is smoothed and it looks less erratic. Reducing 
## fourier order is a way of combating overfitting.

## Final model

final_model<- prophet(weekly.seasonality = F,changepoint.prior.scale = 0.9)
# increasing the effect of the changepoints and removing weekly seasonality.
final_model<- add_seasonality(final_model,name = 'yearly',period = 365,
                              fourier.order = 10)
final_model<- fit.prophet(final_model,data_sample)
fcast_7<- predict(final_model,future)
plot(final_model,fcast_7)













































