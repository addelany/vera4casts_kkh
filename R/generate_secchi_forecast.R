# generate_example_forecast
generate_secchi_forecast <- function(forecast_date, # a recommended argument so you can pass the date to the function
                                      model_id,
                                      targets_url, # where are the targets you are forecasting?
                                      horizon = 10, #how many days into the future
                                      variable = "Secchi_m_sample",
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
    filter(variable == variable,
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
  #   map_dfr(get_daily_weather, site_list = site_list, past = 60, future = 30, vars = "temperature_2m")
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
  # fit_df <- targets |>
  #   pivot_wider(names_from = variable, values_from = observation) #|>
  #   # left_join(historic_weather)
  #
  # model_fit <- lm(fit_df$Temp_C_mean ~ fit_df$air_temperature)
  # #-------------------------------------

  #make model
 forecasted_secchi <-  targets %>%
    select(observation) %>%
    na.omit() %>%
    slice_tail(n=3) %>%
    summarise(mu = mean(observation), sigma = sd(observation)) %>%
    slice(rep(1:n(), each = 10)) %>%
    cbind(horizon_dates) %>%     #add values to dates
    pivot_longer(names_to = "parameter", values_to = "prediction", cols = c(mu, sigma))

  #add values to dates


  # Generate forecasts
  message('Generated forecast')
  # forecast <- (forecast_weather$air_temperature * model_fit$coefficients[2]) + model_fit$coefficients[1]
  #
  forecast_df <- data.frame(datetime = horizon_dates,
                            reference_datetime = forecast_date,
                            model_id = model_id,
                            site_id = site,
                            parameter = forecasted_secchi$parameter,
                            family = 'ensemble',
                            prediction = forecasted_secchi$prediction,
                            variable = variable,
                            depth_m = NA,
                            duration = targets$duration[1],
                            project_id = project_id)
  forecasted_secchi <- data.frame(forecasted_secchi)
  #-------------------------------------

  return(forecast_df)
  return(forecasted_secchi)

}