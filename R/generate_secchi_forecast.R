# generate_example_forecast
generate_secchi_forecast <- function(forecast_date, # a recommended argument so you can pass the date to the function
                                      model_id,
                                      targets_url, # where are the targets you are forecasting?
                                      horizon = 30, #how many days into the future
                                      variable_collect = c("Secchi_m_sample"),
                                      variable = c("Secchi_m_sample"),
                                      site, # what site(s)
                                      project_id = 'vera4cast') {

  horizon_dates <- data.frame(datetime = seq.Date(from = forecast_date + days(1), by = "day", length.out = horizon)) #if this becomes a date time we may
  #have a problem, days(1) should fix it

  # Put your forecast generating code in here, and add/remove arguments as needed.
  # Forecast date should not be hard coded
  # This is an example function that also grabs weather forecast information to be used as co-variates
  #-------------------------------------

  # Get targets
  message('Getting targets')
  targets <- readr::read_csv(targets_url, show_col_types = F) |>
    filter(variable %in% variable_collect,
           site_id %in% site,
           datetime < forecast_date)
  #-------------------------------------

  # Get the weather data
  # message('Getting weather')
  # uses the RopenMeteo function to grab weather from the sites
  # and you can specify the length of the future period and number of days in the past
  # you can modify the data that are collected in the get_daily_weather function
  # or if you want to generate an hourly forecast, you can use get_hourly_weather
  # weather_dat <- site |>
  #   map_dfr(get_daily_weather, site_list = site_list, past = 90, future = 30, vars = "temperature_2m")
  #-------------------------------------

  # split it into historic and future
  # historic_weather <- weather_dat |>
  #   filter(datetime < forecast_date) |>
  #   # calculate a daily mean (remove ensemble)
  #   group_by(datetime, variable, site_id) |>
  #   summarise(prediction = mean(prediction)) |>
  #   pivot_wider(names_from = variable, values_from = prediction) |>
  #   mutate(air_temperature = air_temperature - 273.15)
  #
  # forecast_weather <- weather_dat |>
  #   filter(datetime >= forecast_date) |>
  #   pivot_wider(names_from = variable, values_from = prediction) |>
  #   mutate(air_temperature = air_temperature - 273.15)
  #-------------------------------------

  # Fit model
  # message('Fitting model')

# targets_test <- targets %>%
#   mutate(date = date(datetime)) %>%
#   select(-c(datetime)) %>%
#   rename("datetime" = "date") %>%
#   select(-c("depth_m"))
# targets_test_2 <- aggregate(observation ~ datetime + site_id + duration + variable + project_id, data = targets_test, mean)


  # fit_df <- targets_test_2 |>
  #   #filter(datetime >= "2024-01-01") %>%
  #   distinct() %>%
  #   pivot_wider(names_from = variable, values_from = observation) #|>
  #   # left_join(historic_weather, by = join_by(site_id, datetime))
  # fit_df <- na.omit(fit_df)
  #
   # model_fit <- lm(fit_df$Secchi_m_sample ~ fit_df$Chla_ugL_mean)
  # #-------------------------------------

  #make model
 forecasted_secchi_rolling_avg <-  targets %>%
    select(observation) %>%
    na.omit() %>%
    slice_tail(n=3) %>%
    summarise(mu = mean(observation), sigma = sd(observation)) %>%
    slice(rep(1:n(), each = 30)) %>%
    cbind(horizon_dates) %>%     #add values to dates
    pivot_longer(names_to = "parameter", values_to = "prediction", cols = c(mu, sigma))

  # forecasted_secchi_2 <- targets %>%
  #   select(observation) %>%
  #   na.omit() #%>%

  #add values to dates


  # Generate forecasts
  message('Generated forecast')
  # forecast <- (forecast_weather$air_temperature * model_fit$coefficients[2]) + model_fit$coefficients[1]
  #
  forecast_df <- data.frame(datetime = horizon_dates,
                            reference_datetime = forecast_date,
                            model_id = model_id,
                            site_id = site,
                            #parameter = forecasted_secchi_rolling_avg$parameter,
                            family = 'normal',
                            #prediction = forecasted_secchi_rolling_avg$prediction,
                            variable = variable,
                            depth_m = NA,
                            duration = targets$duration[1],
                            project_id = project_id)

  forecast_df <- forecast_df |>
    right_join(forecasted_secchi_rolling_avg, by = c('datetime')) |>
    select(reference_datetime, datetime, site_id, model_id, variable, family, parameter, prediction, depth_m, project_id, duration)

  #forecasted_secchi <- data.frame(forecasted_secchi_rolling_avg)
  #-------------------------------------

  return(forecast_df)
  #return(forecasted_secchi)

}


